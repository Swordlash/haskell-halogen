// The client of haskell-halogen-remote: what shows, in a real page, a
// component whose program runs somewhere else. The GHC JavaScript backend
// links this file and calls these by name; the wasm backend embeds it into
// its JSFFI module (Halogen.JSBits.browserJS), for the loopback; a native
// server (Halogen.Remote.Server) serves it to pages as it is, so it is plain
// functions on the global object and nothing a module or a newer engine
// would be needed for.
//
// The program sends batches of operations, which the client applies to real
// nodes it keeps by number; the client sends back, for each event a listener
// of the program's hears, what handlers read of it. Halogen.Remote.Protocol
// describes both. Node 0 is the window and node 1 the container the client
// was attached to; every other node is made by an operation.

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
  };
  halogen_remote_forget_all(client);
  container.setAttribute("data-halogen-remote", "attached");
  return client;
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
}

// Apply one message of the program's: {"n": sequence, "ops": [...]}.
function halogen_remote_apply(client, text) {
  var batch = JSON.parse(text);
  var ops = batch.ops;
  for (var i = 0; i < ops.length; i++) halogen_remote_op(client, ops[i]);
}

function halogen_remote_op(client, op) {
  var nodes = client.nodes;
  var node, parent, child;
  switch (op[0]) {
    case "clear":
      halogen_remote_forget_all(client);
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
      parent = nodes.get(op[1]);
      child = nodes.get(op[2]);
      if (parent && child) {
        if (child.parentNode === parent) parent.removeChild(child);
        halogen_remote_forget(client, child);
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
    default:
      break;
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

// A removed node is forgotten with everything under it, and its listeners
// taken off, as the program's mirror forgets it.
function halogen_remote_forget(client, node) {
  var id = client.ids.get(node);
  if (id !== undefined) {
    client.nodes.delete(id);
    var prefix = id + ":";
    var gone = [];
    client.listeners.forEach(function (entry, key) {
      if (key.indexOf(prefix) === 0) gone.push(key);
    });
    for (var i = 0; i < gone.length; i++) {
      var entry = client.listeners.get(gone[i]);
      entry.target.removeEventListener(entry.type, entry.handler);
      client.listeners.delete(gone[i]);
    }
  }
  for (var c = node.firstChild; c; c = c.nextSibling) halogen_remote_forget(client, c);
}

function halogen_remote_listen(client, id, type, listener) {
  var target = client.nodes.get(id);
  var key = id + ":" + type + ":" + listener;
  if (!target || client.listeners.has(key)) return;
  var handler = function (event) {
    // The program hears of the event only once the page is done with it,
    // too late to prevent its default, so the client prevents the two
    // defaults a component that listens never wants: a form's submission
    // reloading the page, and a link it handles itself being followed.
    if (type === "submit" || (type === "click" && target.tagName === "A")) event.preventDefault();
    client.send(JSON.stringify({ listener: listener, node: id, type: type, event: halogen_remote_event(event) }));
  };
  target.addEventListener(type, handler);
  client.listeners.set(key, { target: target, type: type, handler: handler });
}

function halogen_remote_unlisten(client, id, type, listener) {
  var key = id + ":" + type + ":" + listener;
  var entry = client.listeners.get(key);
  if (entry) {
    entry.target.removeEventListener(entry.type, entry.handler);
    client.listeners.delete(key);
  }
}

// What handlers read of an event: its own fields that are strings, numbers
// or booleans, and its target's and current target's form controls.
var halogen_remote_event_fields = [
  "type", "key", "code", "repeat", "altKey", "ctrlKey", "metaKey", "shiftKey",
  "button", "buttons", "clientX", "clientY", "offsetX", "offsetY", "pageX", "pageY",
  "deltaX", "deltaY", "deltaMode", "detail", "inputType", "data", "isComposing", "timeStamp",
];

function halogen_remote_event(event) {
  var out = {};
  for (var i = 0; i < halogen_remote_event_fields.length; i++) {
    var name = halogen_remote_event_fields[i];
    var value = event[name];
    if (typeof value === "string" || typeof value === "boolean" || (typeof value === "number" && isFinite(value))) out[name] = value;
  }
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
  return out;
}

// Stop listening, empty the container, and forget every node.
function halogen_remote_detach(client) {
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

// JSON text as the object a handler reads its event from.
function halogen_remote_parse(text) {
  return JSON.parse(text);
}

// Show, in the container, the session a server at the address keeps for
// this page, over a WebSocket: what Halogen.Remote.Server serves. The
// address may be relative to the page ("/live"). When the connection is
// lost the page goes on showing what it had, without listening, and the
// client connects again after a delay that doubles each time, up to
// options.maxDelay (10 s; options.minDelay is the first, 0.5 s); each
// connection is a new session, which begins by emptying the container.
// The container's data-halogen-remote says "connected" or "disconnected",
// for the page's styling. Returns { close() }, which stops for good.
function halogen_remote_connect(address, container, options) {
  var settings = options || {};
  var minDelay = typeof settings.minDelay === "number" ? settings.minDelay : 500;
  var maxDelay = typeof settings.maxDelay === "number" ? settings.maxDelay : 10000;
  var state = { socket: null, attempts: 0, stopped: false };
  var url = new URL(address, location.href);
  if (url.protocol === "http:") url.protocol = "ws:";
  else if (url.protocol === "https:") url.protocol = "wss:";
  function connect() {
    var socket = new WebSocket(url.href);
    state.socket = socket;
    var client = halogen_remote_attach(container, function (text) {
      if (socket.readyState === WebSocket.OPEN) socket.send(text);
    });
    container.setAttribute("data-halogen-remote", "disconnected");
    socket.onopen = function () {
      state.attempts = 0;
      container.setAttribute("data-halogen-remote", "connected");
    };
    socket.onmessage = function (message) {
      if (typeof message.data === "string") halogen_remote_apply(client, message.data);
    };
    socket.onclose = function () {
      halogen_remote_unlisten_all(client);
      container.setAttribute("data-halogen-remote", "disconnected");
      if (!state.stopped) {
        var delay = Math.min(maxDelay, minDelay * Math.pow(2, state.attempts));
        state.attempts += 1;
        setTimeout(connect, delay);
      }
    };
  }
  connect();
  return {
    close: function () {
      state.stopped = true;
      if (state.socket) state.socket.close();
    },
  };
}
