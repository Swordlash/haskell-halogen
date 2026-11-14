// The client of haskell-halogen-remote: what shows, in a real page, a
// component whose program runs somewhere else. The GHC JavaScript backend
// links this file and calls these by name; the wasm backend embeds it into
// its JSFFI module (Halogen.JSBits.browserJS), for the loopback; a native
// server (Halogen.Remote.Server) serves it to pages as it is, so it is plain
// functions on the global object and nothing a module or a newer engine
// would be needed for.
//
// The program sends batches of operations, which the client applies to real
// nodes it keeps by number (and to the page's history and stores); the
// client sends first where the page is and what its stores hold, then, for
// each event a listener of the program's hears (and its handler's declared
// match, if it has one, lets through), what handlers read of it, and where
// the page is when it goes back or forward. Halogen.Remote.Protocol
// describes all of it. Node 0 is the window and node 1 the container the client
// was attached to; every other node is made by an operation.
//
// A node taken out and not put back by the end of the batch is gone, on both
// sides: the client forgets it then, with what is under it and the listeners
// on them, as the program's mirror does. Not when it is taken out, since a
// render may put it back (the node of a child component whose parent
// replaced the element around it), and a batch never ends in the middle of
// a render.

// A client of the container, sending the page's events, as JSON text, to
// send. It shows nothing until its first batch, which begins by emptying the
// container.
function halogen_remote_attach(container, send) {
  var client = {
    container: container,
    send: send,
    nodes: new Map(),
    ids: new WeakMap(),
    listeners: new Map(),
    // The keys of the listeners on each node, by its number, so that a
    // node that is forgotten takes its own listeners off and no others.
    keysOf: new Map(),
    // The nodes the batch being applied has taken out.
    removed: [],
    // How many of the program's calls it has carried out
    // (halogen_remote_connect's probe waits for a frame after one), and
    // how many moves through the page's history (it waits for the page to
    // have gone).
    calls: 0,
    traversals: 0,
    // The client's own listeners on the window, [type, handler] pairs.
    own: [],
    // The address the program was last told of, or moved the page to.
    location: halogen_remote_href(),
  };
  halogen_remote_forget_all(client);
  halogen_remote_watch_page(client);
  container.setAttribute("data-halogen-remote", "attached");
  return client;
}

// What the client sends first, before anything else (Halogen.Remote.Protocol):
// where the page is (its whole address, and its <base href>'s address when
// it has one), and the items of its two stores that are the library's,
// whose keys start with haskell-halogen-core's storagePrefix.
function halogen_remote_hello() {
  var hasBase = typeof document !== "undefined" && document.querySelector("base[href]") !== null;
  return {
    hello: {
      location: halogen_remote_href(),
      base: hasBase ? document.baseURI : null,
      storage: { local: halogen_remote_items("local"), session: halogen_remote_items("session") },
    },
  };
}

function halogen_remote_hello_text() {
  return JSON.stringify(halogen_remote_hello());
}

function halogen_remote_href() {
  return typeof location === "undefined" ? "/" : location.href;
}

// The prefix haskell-halogen-core keeps a program's items under
// (Halogen.VDom.DOM.Monad.Class.storagePrefix): only these are the
// program's, and only these cross the wire.
var halogen_remote_storage_prefix = "haskell-halogen:";

// A store, or null where the page is denied it (a strict private window,
// storage turned off), which then reads as empty, as in the browser backend.
function halogen_remote_store(kind) {
  try {
    return kind === "local" ? globalThis.localStorage : globalThis.sessionStorage;
  } catch (e) {
    return null;
  }
}

function halogen_remote_items(kind) {
  var items = {};
  try {
    var store = halogen_remote_store(kind);
    if (!store) return items;
    for (var i = 0; i < store.length; i++) {
      var key = store.key(i);
      if (key !== null && key.indexOf(halogen_remote_storage_prefix) === 0) items[key] = store.getItem(key);
    }
  } catch (e) {
    return {};
  }
  return items;
}

// The client's own listeners on the window, added as it attaches, so before
// any of the program's: when the page goes back or forward (a popstate, or
// a fragment the reader edits, which comes with one), the program is told
// where the page is now before the event reaches a listener of its own; and
// when another tab changes an item of the library's in local storage, the
// program is told the change.
function halogen_remote_watch_page(client) {
  if (typeof window === "undefined" || typeof window.addEventListener !== "function") return;
  function moved() {
    var href = halogen_remote_href();
    if (href === client.location) return;
    client.location = href;
    client.send(JSON.stringify({ location: href }));
  }
  function stored(event) {
    var key = event.key;
    if (key !== null && key.indexOf(halogen_remote_storage_prefix) !== 0) return;
    var kind = event.storageArea === halogen_remote_store("local") ? "local" : event.storageArea === halogen_remote_store("session") ? "session" : null;
    if (kind === null) return;
    client.send(JSON.stringify({ storage: { kind: kind, key: key, value: key === null ? null : event.newValue } }));
  }
  client.own = [
    ["popstate", moved],
    ["hashchange", moved],
    ["storage", stored],
  ];
  for (var i = 0; i < client.own.length; i++) window.addEventListener(client.own[i][0], client.own[i][1]);
}

function halogen_remote_unwatch_page(client) {
  if (typeof window === "undefined" || typeof window.removeEventListener !== "function") return;
  for (var i = 0; i < client.own.length; i++) window.removeEventListener(client.own[i][0], client.own[i][1]);
  client.own = [];
}

function halogen_remote_forget_all(client) {
  halogen_remote_unlisten_all(client);
  client.nodes.clear();
  client.ids = new WeakMap();
  client.nodes.set(0, typeof window === "undefined" ? globalThis : window);
  client.nodes.set(1, client.container);
}

function halogen_remote_unlisten_all(client) {
  client.listeners.forEach(function (entry) {
    entry.target.removeEventListener(entry.type, entry.handler);
  });
  client.listeners.clear();
  client.keysOf.clear();
}

// Apply one message of the program's, if it is a batch: {"n": sequence,
// "ops": [...]}. Any other message (a probe's answer, or a kind this client
// does not know, from a newer program) changes nothing on the page.
function halogen_remote_apply(client, text) {
  halogen_remote_message(client, text);
}

// The same, returning the message, for a caller that wants to see what
// else it was (halogen_remote_connect, for the probe's answers): null when
// it is not a JSON object.
function halogen_remote_message(client, text) {
  var message = JSON.parse(text);
  if (message === null || typeof message !== "object") return null;
  if (Array.isArray(message.ops)) halogen_remote_batch(client, message.ops);
  return message;
}

// A batch's operations. The nodes its removals took out that are still out
// at its end are forgotten.
function halogen_remote_batch(client, ops) {
  client.removed = [];
  for (var i = 0; i < ops.length; i++) halogen_remote_op(client, ops[i]);
  var removed = client.removed;
  client.removed = [];
  for (var j = 0; j < removed.length; j++) {
    if (!removed[j].parentNode) halogen_remote_forget(client, removed[j]);
  }
}

function halogen_remote_op(client, op) {
  var nodes = client.nodes;
  var node, parent, child;
  switch (op[0]) {
    case "clear":
      halogen_remote_forget_all(client);
      client.removed = [];
      while (client.container.firstChild) client.container.removeChild(client.container.firstChild);
      break;
    case "create":
      node = op[2] === null ? document.createElement(op[3]) : document.createElementNS(op[2], op[3]);
      nodes.set(op[1], node);
      client.ids.set(node, op[1]);
      break;
    case "text":
      node = document.createTextNode(op[2]);
      nodes.set(op[1], node);
      client.ids.set(node, op[1]);
      break;
    case "set-text":
      node = nodes.get(op[1]);
      if (node) node.data = op[2];
      break;
    case "insert":
      // The index is where the child ends up once taken from wherever it
      // was, so it is taken out first, unless it is there already.
      parent = nodes.get(op[1]);
      child = nodes.get(op[3]);
      if (parent && child && parent.childNodes[op[2]] !== child) {
        if (child.parentNode) child.parentNode.removeChild(child);
        parent.insertBefore(child, parent.childNodes[op[2]] || null);
      }
      break;
    case "remove":
      // Taken out, and forgotten at the end of the batch unless an insert
      // puts it back before then.
      parent = nodes.get(op[1]);
      child = nodes.get(op[2]);
      if (parent && child && child.parentNode === parent) {
        parent.removeChild(child);
        client.removed.push(child);
      }
      break;
    case "attr":
      node = nodes.get(op[1]);
      if (node) {
        if (op[2] === null) node.setAttribute(op[3], op[4]);
        else node.setAttributeNS(op[2], op[3], op[4]);
      }
      break;
    case "unattr":
      node = nodes.get(op[1]);
      if (node) {
        if (op[2] === null) node.removeAttribute(op[3]);
        else node.removeAttributeNS(op[2], op[3]);
      }
      break;
    case "prop":
      // Set only when it differs, as the browser backends do: setting an
      // input's value to what it holds would move the caret of a reader
      // typing in it.
      node = nodes.get(op[1]);
      if (node && node[op[2]] !== op[3]) node[op[2]] = op[3];
      break;
    case "unprop":
      node = nodes.get(op[1]);
      if (node) halogen_remote_reset_property(node, op[2]);
      break;
    case "listen":
      halogen_remote_listen(client, op[1], op[2], op[3]);
      break;
    case "unlisten":
      halogen_remote_unlisten(client, op[1], op[2], op[3]);
      break;
    case "options":
      halogen_remote_options(client, op[1], op[2], op[3], op[4]);
      break;
    case "call":
      client.calls += 1;
      halogen_remote_call(nodes.get(op[1]), op[2], op[3]);
      break;
    case "set":
      // The document's title, on node 0 (the window, which the program
      // knows as its document too), and nothing else.
      if (op[1] === 0 && op[2] === "title" && typeof op[3] === "string" && typeof document !== "undefined") document.title = op[3];
      break;
    case "history":
      halogen_remote_history(client, op[1], op[2]);
      break;
    case "storage":
      halogen_remote_storage(op[1], op[2], op[3]);
      break;
    default:
      break;
  }
}

// The methods of an element the program may call through the wire: what a
// component does to the page through the Web.* types that a page across a
// wire has to do for it (Halogen.Remote.Protocol). Only these, with the
// arguments the program sends (already read down to what each takes), and
// only on an element: the list is not a way to call anything by name.
var halogen_remote_methods = ["focus", "blur", "click", "select", "scrollIntoView", "showModal", "show", "close"];

// A call of one of them. A node the client does not have (forgotten, or
// never made) is not called; neither is a method the element does not have
// (show on a div). One that throws (showModal on a dialog already open as a
// non-modal one) is logged, and the rest of the batch goes on.
function halogen_remote_call(node, method, args) {
  if (!node || typeof Element === "undefined" || !(node instanceof Element)) return;
  if (halogen_remote_methods.indexOf(method) < 0 || typeof node[method] !== "function") return;
  try {
    node[method].apply(node, Array.isArray(args) ? args : []);
  } catch (e) {
    if (typeof console !== "undefined") console.warn("halogen-remote: " + method + " failed:", e);
  }
}

// A move in the page's history: a push or a replacement of the current
// entry, with the address the program wrote (null for the page's own), or
// a move through the entries. The browser throws for an address of another
// origin; that is logged, and the page stays where it was, while the
// program's mirror has moved (the router writes only paths).
function halogen_remote_history(client, how, argument) {
  if (typeof history === "undefined") return;
  try {
    var address = typeof argument === "string" ? argument : undefined;
    if (how === "push") history.pushState(null, "", address);
    else if (how === "replace") history.replaceState(null, "", address);
    else if (how === "go" && typeof argument === "number") {
      // The page goes a moment later, and says so with a popstate.
      client.traversals += 1;
      history.go(argument);
    }
  } catch (e) {
    if (typeof console !== "undefined") console.warn("halogen-remote: history " + how + " failed:", e);
  }
  // A push or a replacement fires no popstate: the program knows already.
  client.location = halogen_remote_href();
}

// An item of a store written (a string) or removed (null): one of the
// library's, under its prefix, and no other item of the page's (the wire is
// not a way to write the page's stores at large). A store the page refuses
// (its quota, its storage denied) logs it; the program, which has written
// its mirror, does not know.
function halogen_remote_storage(kind, key, value) {
  if (typeof key !== "string" || key.indexOf(halogen_remote_storage_prefix) !== 0) return;
  if (kind !== "local" && kind !== "session") return;
  try {
    var store = halogen_remote_store(kind);
    if (!store) throw new Error("the page has no " + kind + " storage");
    if (typeof value === "string") store.setItem(key, value);
    else store.removeItem(key);
  } catch (e) {
    if (typeof console !== "undefined") console.warn("halogen-remote: storage " + kind + " " + key + " failed:", e);
  }
}

// A property is an accessor inherited from the element's prototype, so
// deleting it from the element does nothing: set it back instead, as
// haskell-halogen-core's own binding does.
function halogen_remote_reset_property(node, name) {
  if (typeof node[name] === "string") node[name] = "";
  else if (name === "rowSpan" || name === "colSpan") node[name] = 1;
  else node[name] = undefined;
}

// A node gone at the end of a batch is forgotten with everything under it,
// and its listeners taken off, as the program's mirror forgets it.
function halogen_remote_forget(client, node) {
  var id = client.ids.get(node);
  if (id !== undefined) {
    client.nodes.delete(id);
    var keys = client.keysOf.get(id) || [];
    for (var i = 0; i < keys.length; i++) {
      var entry = client.listeners.get(keys[i]);
      if (entry) {
        entry.target.removeEventListener(entry.type, entry.handler);
        client.listeners.delete(keys[i]);
      }
    }
    client.keysOf.delete(id);
  }
  for (var c = node.firstChild; c; c = c.nextSibling) halogen_remote_forget(client, c);
}

function halogen_remote_listen(client, id, type, listener) {
  var target = client.nodes.get(id);
  var key = id + ":" + type + ":" + listener;
  if (!target || client.listeners.has(key)) return;
  // What the listener's handler declared, once an options operation says:
  // null for one that declares nothing.
  var entry = { target: target, type: type, handler: null, options: null };
  entry.handler = function (event) {
    // Whether the default was prevented before the client acted: what the
    // program reads as the event's defaultPrevented.
    var prevented = event.defaultPrevented;
    var options = entry.options;
    if (options) {
      // The program hears of the event only once the page is done with
      // it, so what its handler declared is done here, while the page
      // dispatches the event: an event that does not match is not the
      // handler's, and does not cross the wire.
      if (options.match !== null && options.match !== undefined && !halogen_remote_matches(options.match, event, target)) return;
      if (options.preventDefault) event.preventDefault();
      if (options.stopPropagation) event.stopPropagation();
    } else if (type === "submit" || (type === "click" && target.tagName === "A" && halogen_remote_plain_left_click(event, target))) {
      // A handler that declares nothing: the client prevents the two
      // defaults a component that listens never wants, a form's
      // submission reloading the page, and a link it handles itself being
      // followed in this tab. A click the browser would not follow here
      // (with a modifier key, another button, on a link to another target
      // or a download) is left to the browser: a ctrl-click opens a new
      // tab.
      event.preventDefault();
    }
    client.send(JSON.stringify({ listener: listener, node: id, type: type, event: halogen_remote_event(event, prevented) }));
  };
  target.addEventListener(type, entry.handler);
  client.listeners.set(key, entry);
  var keys = client.keysOf.get(id);
  if (keys) keys.push(key);
  else client.keysOf.set(id, [key]);
}

// What the handler of a listener the client has declared, which replaces
// what it declared before. Options that declare nothing (no match, nothing
// prevented or stopped) are a listener's that never had any.
function halogen_remote_options(client, id, type, listener, options) {
  var entry = client.listeners.get(id + ":" + type + ":" + listener);
  if (!entry) return;
  var declares = options && ((options.match !== null && options.match !== undefined) || options.preventDefault || options.stopPropagation);
  entry.options = declares ? options : null;
}

// Whether an event is one a handler's declared match is for (the module
// Halogen.VDom.DOM.HandlerOptions of haskell-halogen-core): "any",
// "plain-left-click" on the listening element, or a list of keys, each with
// the modifiers it says are held or not. A match this client does not know
// (from a newer library than the client) is taken as "any", and the program
// still asks its own handler only about the events that match.
function halogen_remote_matches(match, event, target) {
  if (match === "any") return true;
  if (match === "plain-left-click") return halogen_remote_plain_left_click(event, target);
  if (Array.isArray(match)) {
    for (var i = 0; i < match.length; i++) {
      var k = match[i];
      if (
        event.key === k.key &&
        (typeof k.ctrlKey !== "boolean" || k.ctrlKey === event.ctrlKey) &&
        (typeof k.shiftKey !== "boolean" || k.shiftKey === event.shiftKey) &&
        (typeof k.altKey !== "boolean" || k.altKey === event.altKey) &&
        (typeof k.metaKey !== "boolean" || k.metaKey === event.metaKey)
      )
        return true;
    }
    return false;
  }
  return true;
}

function halogen_remote_unlisten(client, id, type, listener) {
  var key = id + ":" + type + ":" + listener;
  var entry = client.listeners.get(key);
  if (entry) {
    entry.target.removeEventListener(entry.type, entry.handler);
    client.listeners.delete(key);
    var keys = client.keysOf.get(id);
    if (keys) {
      var at = keys.indexOf(key);
      if (at >= 0) keys.splice(at, 1);
    }
  }
}

// A click the browser would follow as it is, in this tab, on the link
// listening for it: the main button, no key held that asks for a new tab or
// window or a download, nothing that prevented it already, a link that names
// no other target (_self, in any case) and is no download. It is what
// haskell-halogen-router's Halogen.Router.isPlainLeftClick asks.
function halogen_remote_plain_left_click(event, link) {
  return (
    event.button === 0 &&
    !event.ctrlKey &&
    !event.metaKey &&
    !event.shiftKey &&
    !event.altKey &&
    !event.defaultPrevented &&
    !(typeof link.target === "string" && link.target !== "" && link.target.toLowerCase() !== "_self") &&
    !(typeof link.hasAttribute === "function" && link.hasAttribute("download"))
  );
}

// What handlers read of an event: its own fields that are strings, numbers
// or booleans, whether its default was prevented before the client acted,
// and its target's and current target's form controls and, of a link, its
// target and whether it is a download.
var halogen_remote_event_fields = [
  "type", "key", "code", "repeat", "altKey", "ctrlKey", "metaKey", "shiftKey",
  "button", "buttons", "clientX", "clientY", "offsetX", "offsetY", "pageX", "pageY",
  "deltaX", "deltaY", "deltaMode", "detail", "inputType", "data", "isComposing", "timeStamp",
];

function halogen_remote_event(event, prevented) {
  var out = {};
  for (var i = 0; i < halogen_remote_event_fields.length; i++) {
    var name = halogen_remote_event_fields[i];
    var value = event[name];
    if (typeof value === "string" || typeof value === "boolean" || (typeof value === "number" && isFinite(value))) out[name] = value;
  }
  if (prevented) out.defaultPrevented = true;
  out.target = halogen_remote_element(event.target);
  out.currentTarget = halogen_remote_element(event.currentTarget);
  return out;
}

function halogen_remote_element(element) {
  if (element === null || typeof element !== "object") return null;
  var out = {};
  if (typeof element.value === "string") out.value = element.value;
  if (typeof element.checked === "boolean") out.checked = element.checked;
  if (typeof element.selectedIndex === "number") out.selectedIndex = element.selectedIndex;
  if (typeof element.tagName === "string") out.tagName = element.tagName;
  if (typeof element.id === "string" && element.id !== "") out.id = element.id;
  // A link's (an a's or an area's, whose download is a string property).
  if (typeof element.download === "string") {
    if (typeof element.target === "string" && element.target !== "") out.target = element.target;
    if (element.hasAttribute("download")) out.download = true;
  }
  return out;
}

// Stop listening, empty the container, and forget every node.
function halogen_remote_detach(client) {
  halogen_remote_unwatch_page(client);
  halogen_remote_forget_all(client);
  while (client.container.firstChild) client.container.removeChild(client.container.firstChild);
  client.container.removeAttribute("data-halogen-remote");
}

// A client whose events go to a callback of the program's in this same
// page (Halogen.Remote.Loopback), as JSON text.
function halogen_remote_loopback(container, callback) {
  return halogen_remote_attach(container, function (text) {
    callback(text);
  });
}

// JSON text as the object a handler reads its event from, with the methods
// a listener may call on an event: a handler declared with
// HE.preventingDefault or HE.stoppingPropagation has the listener call them
// before its action. They do nothing, since the page dispatched the event
// long before (the client did what the handler declared itself), and
// defaultPrevented stays what the client sent: whether something in the
// page had prevented the default before the client acted.
function halogen_remote_parse(text) {
  var event = JSON.parse(text);
  if (event === null || typeof event !== "object") return event;
  event.preventDefault = halogen_remote_ignore;
  event.stopPropagation = halogen_remote_ignore;
  event.stopImmediatePropagation = halogen_remote_ignore;
  event.defaultPrevented = event.defaultPrevented === true;
  return event;
}

function halogen_remote_ignore() {}

// Show, in the container, the session a server at the address keeps for
// this page, over a WebSocket: what Halogen.Remote.Server serves. The
// address may be relative to the page ("/live"). When the connection is
// lost the page goes on showing what it had, without listening, and the
// client connects again after a delay that doubles each time, up to
// options.maxDelay (10 s; options.minDelay is the first, 0.5 s); each
// connection is a new session, which begins by emptying the container.
// With options.reconnect false it connects once, and a connection that
// closes stays closed (a test's page, whose session is the test's). The
// container's data-halogen-remote says "connected" or "disconnected", for
// the page's styling. Returns { close() }, which stops for good.
//
// When the page has a probe (globalThis.__halogenProbe an object, as
// haskell-halogen-playwright's pages have before their scripts run), the
// client gives it what haskell-halogen-core's programs give it
// (core/jsbits/activity.js), in the same list, so that a page with programs
// of both kinds sums them all: its question is forwarded to the session,
// whose answer comes in order with its batches (Halogen.Remote.Protocol),
// so that when it resolves the page shows everything the answer accounts
// for. With no open connection the client answers at once that there is
// nothing: no tree, nothing running.
//
// An answer accounts for the events the page sent before the question. A
// batch can make the page send one after it: a focus or a click the
// program asked for (a "call"), heard by a listener of the program's own.
// So an answer that arrives after the client has sent an event since the
// question is not given to the page: the client asks again, and the page
// has the answer to a question no event followed. A call can also make the
// page dispatch an event a moment later (a dialog's "close" comes with the
// page's next frame), so from a batch with a call until the page has had a
// frame (halogen_remote_after_frame) the client holds the answers it
// receives, and looks whether an event went out only then. A move through
// the page's history the program asked for (back, forward, go) is the same:
// the page goes a moment later, so the answers are held until it has
// (halogen_remote_after_traversal).
function halogen_remote_connect(address, container, options) {
  var settings = options || {};
  var minDelay = typeof settings.minDelay === "number" ? settings.minDelay : 500;
  var maxDelay = typeof settings.maxDelay === "number" ? settings.maxDelay : 10000;
  var reconnect = settings.reconnect !== false;
  var state = {
    socket: null,
    attempts: 0,
    stopped: false,
    retry: null,
    nextQuestion: 1,
    // The questions asked and not answered: what resolves each, and how
    // many events the client had sent as it asked.
    questions: new Map(),
    sent: 0,
    // The frames being waited for after a call, and the answers held
    // until there is none.
    frames: 0,
    held: [],
  };
  var url = new URL(address, location.href);
  if (url.protocol === "http:") url.protocol = "ws:";
  else if (url.protocol === "https:") url.protocol = "wss:";
  // The questions of a connection that ends are answered: nothing is
  // there any more.
  function forgetQuestions() {
    var waiting = state.questions;
    state.questions = new Map();
    waiting.forEach(function (asked) {
      asked.resolve(halogen_remote_nothing());
    });
  }
  function open(socket) {
    return state.socket === socket && socket.readyState === WebSocket.OPEN;
  }
  // Ask the session, noting how many events the client has sent as it
  // asks.
  function question(socket, resolve) {
    var n = state.nextQuestion++;
    state.questions.set(n, { resolve: resolve, sent: state.sent });
    socket.send(JSON.stringify({ probe: n }));
  }
  function ask() {
    var socket = state.socket;
    if (socket === null || socket.readyState !== WebSocket.OPEN) return Promise.resolve(halogen_remote_nothing());
    return new Promise(function (resolve) {
      question(socket, resolve);
    });
  }
  // Give the page the answer to its question, unless the client has sent
  // an event since it asked: then the answer does not account for it, and
  // the client asks again.
  function answered(socket, asked, reading) {
    if (!open(socket)) asked.resolve(halogen_remote_nothing());
    else if (state.sent !== asked.sent) question(socket, asked.resolve);
    else asked.resolve(reading);
  }
  // The answers held, once no frame is waited for.
  function release() {
    if (state.frames > 0) return;
    var held = state.held;
    state.held = [];
    held.forEach(function (answer) {
      answered(answer.socket, answer.asked, answer.reading);
    });
  }
  var asker = halogen_remote_probe_install(ask);
  function connect() {
    state.retry = null;
    if (state.stopped) return;
    var socket = new WebSocket(url.href);
    state.socket = socket;
    var client = halogen_remote_attach(container, function (text) {
      if (socket.readyState === WebSocket.OPEN) {
        state.sent += 1;
        socket.send(text);
      }
    });
    container.setAttribute("data-halogen-remote", "disconnected");
    socket.onopen = function () {
      state.attempts = 0;
      // Before anything else: where the page is and what its stores hold,
      // which the session needs before it mounts the component.
      client.location = halogen_remote_href();
      socket.send(halogen_remote_hello_text());
      container.setAttribute("data-halogen-remote", "connected");
    };
    socket.onmessage = function (message) {
      if (typeof message.data !== "string") return;
      var calls = client.calls;
      var traversals = client.traversals;
      var received = halogen_remote_message(client, message.data);
      if (client.calls !== calls) {
        // The batch held a call: an event it makes the page dispatch
        // later comes with the next frame.
        state.frames += 1;
        halogen_remote_after_frame(function () {
          state.frames -= 1;
          release();
        });
      }
      if (client.traversals !== traversals) {
        // The batch moved through the page's history: the page goes a
        // moment later, and the program hears where it is then.
        state.frames += 1;
        halogen_remote_after_traversal(function () {
          state.frames -= 1;
          release();
        });
      }
      if (received !== null && typeof received.probe === "number" && !Array.isArray(received.ops)) {
        var asked = state.questions.get(received.probe);
        if (asked) {
          state.questions.delete(received.probe);
          var reading = { trees: received.trees, running: received.running, waiting: received.waiting };
          state.held.push({ socket: socket, asked: asked, reading: reading });
          release();
        }
      }
    };
    socket.onclose = function () {
      halogen_remote_unwatch_page(client);
      halogen_remote_unlisten_all(client);
      container.setAttribute("data-halogen-remote", "disconnected");
      if (state.socket === socket) forgetQuestions();
      if (!state.stopped && reconnect) {
        var delay = Math.min(maxDelay, minDelay * Math.pow(2, state.attempts));
        state.attempts += 1;
        state.retry = setTimeout(connect, delay);
      }
    };
  }
  connect();
  return {
    // A reconnection that is waiting for its delay does not happen, and the
    // probe no longer asks this client.
    close: function () {
      state.stopped = true;
      if (state.retry !== null) clearTimeout(state.retry);
      state.retry = null;
      if (state.socket) state.socket.close();
      halogen_remote_probe_remove(asker);
      forgetQuestions();
    },
  };
}

// Run once the page has drawn a frame and the tasks queued by then have
// run: by then it has dispatched the events a call made it queue (a
// dialog's "close" is dispatched with the next frame). A page that draws
// no frames (one in the background) is not waited for longer than a tenth
// of a second.
function halogen_remote_after_frame(run) {
  var done = false;
  function once() {
    if (!done) {
      done = true;
      run();
    }
  }
  if (typeof requestAnimationFrame === "function") {
    requestAnimationFrame(function () {
      setTimeout(once, 0);
    });
  }
  setTimeout(once, 100);
}

// Run once the page has gone where a move through its history takes it:
// it says so with a popstate (a hashchange, for a fragment), after whose
// listeners, the client's own and the program's, the address and the event
// have been sent. A move with nowhere to go (forward from the last entry)
// says nothing, and is not waited for longer than half a second.
function halogen_remote_after_traversal(run) {
  var done = false;
  function once() {
    if (done) return;
    done = true;
    if (typeof window !== "undefined" && typeof window.removeEventListener === "function") {
      window.removeEventListener("popstate", gone);
      window.removeEventListener("hashchange", gone);
    }
    run();
  }
  // After the event's other listeners, whenever they were added.
  function gone() {
    setTimeout(once, 0);
  }
  if (typeof window !== "undefined" && typeof window.addEventListener === "function") {
    window.addEventListener("popstate", gone);
    window.addEventListener("hashchange", gone);
  }
  setTimeout(once, 500);
}

function halogen_remote_nothing() {
  return { trees: 0, running: 0, waiting: 0 };
}

// Add an asker to the page's probe, if the page has one, as
// core/jsbits/activity.js does (the same list under the same symbol, and
// the same sum if this client is the first to come): the asker, or null.
function halogen_remote_probe_install(ask) {
  var probe = globalThis.__halogenProbe;
  if (probe === null || typeof probe !== "object") return null;
  var programs = Symbol.for("halogen.activity.programs");
  if (!Array.isArray(probe[programs])) {
    Object.defineProperty(probe, programs, { value: [], enumerable: false });
    probe.activity = function () {
      return Promise.all(
        probe[programs].map(function (asking) {
          return asking();
        })
      ).then(function (answers) {
        return answers.reduce(
          function (sum, a) {
            return { trees: sum.trees + a.trees, running: sum.running + a.running, waiting: sum.waiting + a.waiting };
          },
          { trees: 0, running: 0, waiting: 0 }
        );
      });
    };
  }
  probe[programs].push(ask);
  return ask;
}

function halogen_remote_probe_remove(ask) {
  var probe = globalThis.__halogenProbe;
  if (ask === null || probe === null || typeof probe !== "object") return;
  var list = probe[Symbol.for("halogen.activity.programs")];
  if (!Array.isArray(list)) return;
  var at = list.indexOf(ask);
  if (at >= 0) list.splice(at, 1);
}
