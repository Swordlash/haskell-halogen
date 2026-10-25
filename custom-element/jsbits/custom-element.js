// Custom elements whose content is a Halogen component, for
// Halogen.CustomElement. The GHC JavaScript backend links this file and calls
// these by name; the wasm backend embeds it into its JSFFI module
// (Halogen.JSBits.browserJS).
//
// customElements.define is permanent: a name, once defined, keeps its class
// for the life of the page. Halogen.CustomElement.define may be called again
// for the same name (browser GHCi's :main, the tutorial's next compile), so
// each name gets one class, defined once, whose instances ask an entry of
// the registry below what to do: the entry holds the current definition (the
// names it observes, its methods, its shadow root and its styles) and the
// Haskell callbacks behind it, which a later define replaces and a disposal
// takes away. An element whose entry has no callbacks renders nothing.
//
// The registry is the page's, kept on globalThis under a Symbol of its own
// rather than in this file's scope, because the wasm backend evaluates this
// file once per splice and per program, and there is one customElements per
// page.
//
// Haskell knows each element by a number of its own (instance_id), and keeps
// its mounted tree under it. Every callback here is one Haskell made with
// mkSyncCallback, so what the page does to an element is seen while it does
// it, as an event listener is.

// No nullish assignment here, or anywhere in this file: the wasm backend
// embeds the file in a C string, where its two question marks and the equals
// sign are a trigraph, which the C compiler warns of.
function halogen_custom_element_registry() {
  const key = Symbol.for("haskell-halogen-custom-element");
  if (!globalThis[key]) globalThis[key] = { entries: new Map(), nextId: 1 };
  return globalThis[key];
}

// What this file keeps on an element: its number, the properties set on it,
// the shadow root it was given, the observer of attributes its class does
// not observe, and whether it has been disconnected and may be about to go.
function halogen_custom_element_state(element) {
  const key = Symbol.for("haskell-halogen-custom-element.state");
  let state = element[key];
  if (!state) {
    state = { id: halogen_custom_element_registry().nextId++, props: {}, shadow: null, observer: null, leaving: false };
    Object.defineProperty(element, key, { value: state });
  }
  return state;
}

// One class per name. observedAttributes is read once, by
// customElements.define, so it is the first definition's names; a later
// definition's other names are watched with a MutationObserver instead (see
// mount_point). Attribute names are compared in lower case, as HTML stores
// them.
function halogen_custom_element_class(entry) {
  return class extends HTMLElement {
    static get observedAttributes() {
      return entry.classObserved;
    }
    connectedCallback() {
      halogen_custom_element_connected(entry, this);
    }
    disconnectedCallback() {
      halogen_custom_element_disconnected(entry, this);
    }
    attributeChangedCallback(name, before, after) {
      if (before !== after) halogen_custom_element_changed(entry, this, name);
    }
  };
}

// An element connected again before its disconnection was acted on was moved
// (removed and inserted in one go, as appendChild and insertBefore do), and
// keeps its tree. One connected while the name is inert is only noted, for
// the next define to mount.
function halogen_custom_element_connected(entry, element) {
  const state = halogen_custom_element_state(element);
  if (state.leaving) {
    // Moved: it keeps its tree, as a rule. But if its name was defined
    // again while it was out of the page, that definition disposed of its
    // tree and did not mount it (it was not connected then), so it would
    // stay empty: the hook is called all the same, and Haskell mounts an
    // element that has no tree and leaves one that has alone.
    state.leaving = false;
    if (entry.hooks) entry.hooks.connect(element);
    return;
  }
  entry.connected.add(element);
  if (entry.hooks) entry.hooks.connect(element);
}

// A disconnection is acted on in a microtask, once whatever moved the element
// has had the chance to insert it again.
function halogen_custom_element_disconnected(entry, element) {
  const state = halogen_custom_element_state(element);
  state.leaving = true;
  queueMicrotask(() => {
    if (!state.leaving) return;
    state.leaving = false;
    entry.connected.delete(element);
    if (state.observer) {
      state.observer.disconnect();
      state.observer = null;
    }
    if (entry.hooks) entry.hooks.disconnect(element);
  });
}

function halogen_custom_element_changed(entry, element, name) {
  if (entry.hooks && entry.connected.has(element) && entry.observedLower.includes(name.toLowerCase())) {
    entry.hooks.change(element);
  }
}

// A method of the element: a promise of what the component answers, decoded
// from the JSON Haskell resolves it with. No argument is passed as [], one as
// itself, several as an array, so that a method of () takes none, one of a
// value takes that, and one of a tuple takes its parts.
function halogen_custom_element_call(entry, element, name, args) {
  return new Promise((resolve, reject) => {
    if (!entry.hooks) {
      resolve(null);
      return;
    }
    // An attribute a later definition observes is watched with a
    // MutationObserver, whose records are delivered in a microtask: a page
    // that sets one and calls a method in the same task would have the
    // method answered with the old input. Taking the records now, before
    // the call, delivers the change first, and the observer's own callback
    // then finds nothing left to deliver.
    const state = halogen_custom_element_state(element);
    if (state.observer && state.observer.takeRecords().length > 0 && entry.connected.has(element)) {
      entry.hooks.change(element);
    }
    const json = args.length === 0 ? "[]" : JSON.stringify(args.length === 1 ? args[0] : args);
    entry.hooks.method({ element, name, args: json ?? "null", resolve, reject });
  });
}

// Install the accessors of the observed names and the methods on the class's
// prototype, and take away those of the definition before that this one does
// not have. A name HTMLElement already has (title, hidden, click, focus, ...)
// is left to the browser: shadowing it would break what the page and its
// frameworks expect of every element.
function halogen_custom_element_install(entry, observed, methods) {
  const proto = entry.proto;
  const builtIn = (name) => name in HTMLElement.prototype;
  for (const name of entry.installed) delete proto[name];
  entry.installed = [];
  entry.properties = observed.filter((name) => !builtIn(name));
  for (const name of entry.properties) {
    Object.defineProperty(proto, name, {
      configurable: true,
      enumerable: true,
      get() {
        return halogen_custom_element_state(this).props[name];
      },
      set(value) {
        halogen_custom_element_state(this).props[name] = value;
        halogen_custom_element_changed(entry, this, name);
      },
    });
    entry.installed.push(name);
  }
  for (const name of methods) {
    if (builtIn(name) || entry.installed.includes(name)) continue;
    Object.defineProperty(proto, name, {
      configurable: true,
      writable: true,
      value: function (...args) {
        return halogen_custom_element_call(entry, this, name, args);
      },
    });
    entry.installed.push(name);
  }
}

// Define the name, or take it over again: returns the elements connected now,
// for Haskell to mount, or what customElements.define threw (a name it does
// not take, or one something else has defined).
function halogen_custom_element_define(name, token, observedJson, methodsJson, shadow, styles, connect, change, disconnect, method) {
  const registry = halogen_custom_element_registry();
  const observed = JSON.parse(observedJson);
  let entry = registry.entries.get(name);
  if (!entry) {
    entry = {
      classObserved: observed.map((n) => n.toLowerCase()),
      observedLower: [],
      hooks: null,
      connected: new Set(),
      installed: [],
      properties: [],
      observed: [],
      shadow: 1,
      sheet: null,
      token: null,
    };
    try {
      if (customElements.get(name)) {
        throw new DOMException(`${name} is defined already, and not by Halogen.CustomElement`, "NotSupportedError");
      }
      const definition = halogen_custom_element_class(entry);
      entry.proto = definition.prototype;
      // This upgrades the elements of the name already in the document;
      // with no callbacks yet, each is only noted as connected.
      customElements.define(name, definition);
    } catch (error) {
      return error;
    }
    registry.entries.set(name, entry);
  }
  entry.observed = observed;
  entry.observedLower = observed.map((n) => n.toLowerCase());
  entry.shadow = shadow;
  entry.sheet = null;
  if (styles !== "" && typeof CSSStyleSheet === "function") {
    try {
      entry.sheet = new CSSStyleSheet();
      entry.sheet.replaceSync(styles);
    } catch {
      entry.sheet = null;
    }
  }
  halogen_custom_element_install(entry, observed, JSON.parse(methodsJson));
  entry.token = token;
  entry.hooks = { connect, change, disconnect, method };
  // An element removed in this task is still noted as connected until its
  // microtask has run, and is left to it.
  return Array.from(entry.connected).filter((element) => element.isConnected);
}

// Make the name inert, if the definition with this token is still its own:
// a later define has taken it over otherwise.
function halogen_custom_element_inert(name, token) {
  const entry = halogen_custom_element_registry().entries.get(name);
  if (!entry || entry.token !== token) return;
  entry.hooks = null;
  for (const element of entry.connected) {
    const state = halogen_custom_element_state(element);
    if (state.observer) {
      state.observer.disconnect();
      state.observer = null;
    }
  }
}

function halogen_custom_element_instance_id(element) {
  return halogen_custom_element_state(element).id;
}

// Where an element's component renders, made ready for it: its shadow root,
// attached the first time one is asked for and kept from then on, with the
// definition's styles adopted into it; or, with no shadow root, the element
// itself. Properties a page set on the element before its class was defined
// (React and Vue set them as soon as they create it) are its own data
// properties, which hide the class's accessors: they are taken over here.
function halogen_custom_element_mount_point(element) {
  const entry = halogen_custom_element_registry().entries.get(element.localName);
  const state = halogen_custom_element_state(element);
  for (const name of entry.properties) {
    if (Object.hasOwn(element, name)) {
      const value = element[name];
      delete element[name];
      state.props[name] = value;
    }
  }
  if (!state.shadow && entry.shadow !== 0) {
    try {
      state.shadow = element.attachShadow({ mode: entry.shadow === 1 ? "open" : "closed" });
    } catch {
      state.shadow = element.shadowRoot;
    }
  }
  const root = state.shadow ?? element;
  if (root !== element && "adoptedStyleSheets" in root) root.adoptedStyleSheets = entry.sheet ? [entry.sheet] : [];
  if (state.observer) state.observer.disconnect();
  state.observer = null;
  const unobserved = entry.observedLower.filter((name) => !entry.classObserved.includes(name));
  if (unobserved.length > 0) {
    state.observer = new MutationObserver(() => {
      if (entry.hooks && entry.connected.has(element)) entry.hooks.change(element);
    });
    state.observer.observe(element, { attributes: true, attributeFilter: unobserved });
  }
  return root;
}

// The observed names' attributes and properties, as JSON: [attributes,
// properties]. A property that JSON cannot carry (a function, a cycle) is
// left out.
function halogen_custom_element_props(element) {
  const entry = halogen_custom_element_registry().entries.get(element.localName);
  const state = halogen_custom_element_state(element);
  const attributes = {};
  const properties = {};
  for (const name of entry.observed) {
    const attribute = element.getAttribute(name);
    if (attribute !== null) attributes[name] = attribute;
    if (Object.hasOwn(state.props, name)) {
      try {
        const json = JSON.stringify(state.props[name]);
        if (json !== undefined) properties[name] = JSON.parse(json);
      } catch {
        // Not JSON: as if it were not set.
      }
    }
  }
  return JSON.stringify([attributes, properties]);
}

function halogen_custom_element_dispatch(element, name, detailJson) {
  element.dispatchEvent(new CustomEvent(name, { detail: JSON.parse(detailJson), bubbles: true, composed: true }));
}

function halogen_custom_element_length(array) {
  return array.length;
}

function halogen_custom_element_index(array, index) {
  return array[index];
}

function halogen_custom_element_is_error(value) {
  return value instanceof Error || (typeof DOMException !== "undefined" && value instanceof DOMException);
}

function halogen_custom_element_error_name(error) {
  return String(error.name);
}

function halogen_custom_element_error_message(error) {
  return String(error.message);
}

function halogen_custom_element_call_element(call) {
  return call.element;
}

function halogen_custom_element_call_name(call) {
  return call.name;
}

function halogen_custom_element_call_args(call) {
  return call.args;
}

function halogen_custom_element_resolve(call, json) {
  call.resolve(JSON.parse(json));
}

function halogen_custom_element_reject(call, message) {
  call.reject(new Error(message));
}

// An event's detail as JSON, for onEvent: "null" for an event without one,
// or with one JSON cannot carry.
function halogen_custom_element_detail(event) {
  try {
    return JSON.stringify(event.detail) ?? "null";
  } catch {
    return "null";
  }
}
