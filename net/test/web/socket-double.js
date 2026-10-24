// A stand-in for a WebSocket server and an event-stream server, which the
// runner's file server is neither: the browser suite drives each connection
// from the test (opening it, sending to the page, closing it, failing it)
// and reads back what the page did with it (what it sent, how it closed it).
// The runner loads every .js file in test/web into the page before the suite
// starts, so this replaces the page's WebSocket and EventSource before
// anything calls them.
//
// Only addresses on the host "double" are faked; every other goes to the
// browser's own WebSocket or EventSource, untouched, so the suite also sees
// what the browser itself refuses and how a real connection fails.
//
//   ws://double/<name>       a socket that waits for the test: open(name)
//                            opens it, and the test sends, closes or fails
//                            it from there.
//   ws://double/echo/<name>  a socket that opens by itself, choosing the
//                            first subprotocol asked for, and sends back
//                            every message the page sends it.
//   http://double/<name>     an event source that waits for the test.
//
// Either kind of fake dispatches its events as tasks of their own, as the
// browser does, so a listener never runs inside the call that caused it, and
// closes as a server would when the page closes it: the handshake finishes,
// and the close event says what the page said, cleanly. A fake keeps the
// browser's rules for what the page may do (no send while connecting, the
// close codes a page may use).
//
// What it notes lives in the page, which browser GHCi (toolchain/dev-test.sh)
// keeps from one :main to the next, so the suite calls reset() as it starts:
// every connection still open is closed, and every name forgotten.
(() => {
  const RealWebSocket = globalThis.WebSocket;
  const RealEventSource = globalThis.EventSource;
  const later = (f) => setTimeout(f, 0);
  const doubled = (url) => {
    try {
      const parsed = new URL(url, location.href);
      return parsed.host === "double" ? parsed.pathname.slice(1) : null;
    } catch {
      return null;
    }
  };
  // Every connection made under a name, the last one last.
  const sockets = new Map();
  const sources = new Map();
  const made = (map, name, connection) => {
    if (!map.has(name)) map.set(name, []);
    map.get(name).push(connection);
  };
  const latest = (map, name) => {
    const all = map.get(name);
    if (!all) throw new Error("the double has no connection named " + name);
    return all[all.length - 1];
  };

  // An EventTarget that counts the listeners on it, which is how the suite
  // sees that a binding takes its listeners off a connection that has ended.
  class Counted extends EventTarget {
    listeners = 0;
    addEventListener(type, listener, options) {
      this.listeners++;
      super.addEventListener(type, listener, options);
    }
    removeEventListener(type, listener, options) {
      this.listeners--;
      super.removeEventListener(type, listener, options);
    }
  }

  class FakeWebSocket extends Counted {
    static CONNECTING = 0;
    static OPEN = 1;
    static CLOSING = 2;
    static CLOSED = 3;

    constructor(url, protocols = []) {
      const name = doubled(url);
      if (name === null) return new RealWebSocket(url, protocols);
      super();
      this.url = String(url);
      this.name = name;
      this.asked = typeof protocols === "string" ? [protocols] : Array.from(protocols);
      this.readyState = 0;
      this.protocol = "";
      this.binaryType = "blob";
      this.sent = [];
      this.closedWith = null;
      this.ended = false;
      this.echo = name.startsWith("echo/");
      made(sockets, name, this);
      if (this.echo) later(() => this.accept(this.asked[0] ?? ""));
    }

    accept(protocol) {
      if (this.readyState !== 0) return;
      this.readyState = 1;
      this.protocol = protocol;
      this.dispatchEvent(new Event("open"));
    }

    deliver(data) {
      if (this.readyState !== 1) return;
      if (typeof data !== "string" && this.binaryType === "blob") data = new Blob([data]);
      this.dispatchEvent(new MessageEvent("message", { data }));
    }

    finish(code, reason, wasClean) {
      if (this.ended) return;
      this.ended = true;
      this.readyState = 3;
      this.dispatchEvent(new CloseEvent("close", { code, reason, wasClean }));
    }

    // An error, and then the close of a connection that had no handshake.
    fail() {
      if (this.ended) return;
      this.readyState = 3;
      this.dispatchEvent(new Event("error"));
      this.finish(1006, "", false);
    }

    send(data) {
      if (this.readyState === 0) throw new DOMException("Still in CONNECTING state.", "InvalidStateError");
      if (this.readyState !== 1) return;
      const copy =
        typeof data === "string"
          ? data
          : ArrayBuffer.isView(data)
            ? data.buffer.slice(data.byteOffset, data.byteOffset + data.byteLength)
            : data.slice(0);
      this.sent.push(copy);
      if (this.echo) later(() => this.deliver(copy));
    }

    close(code, reason = "") {
      if (code !== undefined && code !== 1000 && !(code >= 3000 && code <= 4999))
        throw new DOMException("The close code must be either 1000, or between 3000 and 4999.", "InvalidAccessError");
      if (new TextEncoder().encode(reason).length > 123)
        throw new DOMException("The close reason must not be greater than 123 UTF-8 bytes.", "SyntaxError");
      if (this.readyState >= 2) return;
      this.closedWith = { code: code ?? 0, reason };
      if (this.readyState === 0) {
        this.readyState = 2;
        later(() => this.fail());
        return;
      }
      this.readyState = 2;
      later(() => this.finish(code ?? 1005, reason, true));
    }
  }

  class FakeEventSource extends Counted {
    static CONNECTING = 0;
    static OPEN = 1;
    static CLOSED = 2;

    constructor(url, init = {}) {
      const name = doubled(url);
      if (name === null) return new RealEventSource(url, init);
      super();
      this.url = String(url);
      this.withCredentials = Boolean(init.withCredentials);
      this.readyState = 0;
      this.closedByPage = false;
      made(sources, name, this);
    }

    close() {
      this.readyState = 2;
      this.closedByPage = true;
    }
  }

  globalThis.WebSocket = FakeWebSocket;
  globalThis.EventSource = FakeEventSource;

  const socket = (name) => latest(sockets, name);
  const source = (name) => latest(sources, name);
  const bytes = (data) => (typeof data === "string" ? null : new Uint8Array(data));

  globalThis.halogenNetSocketTest = {
    // Sockets: how many were made under the name, and of the last one what
    // it asked for, what the page sent on it, how the page closed it (null
    // if it did not; a code of 0 for a close with none), and how many
    // listeners are on it.
    sockets: (name) => sockets.get(name)?.length ?? 0,
    asked: (name) => socket(name).asked.join(","),
    sent: (name) => socket(name).sent.length,
    sentText: (name, i) => socket(name).sent[i],
    sentBytes: (name, i) => bytes(socket(name).sent[i]),
    closedWith: (name) => socket(name).closedWith,
    socketListeners: (name) => socket(name).listeners,
    // What the server does to the last socket under the name, each in a task
    // of its own, in the order asked.
    accept: (name, protocol) => later(() => socket(name).accept(protocol)),
    sendText: (name, text) => later(() => socket(name).deliver(text)),
    sendBytes: (name, array) => {
      const buffer = array.slice().buffer;
      later(() => socket(name).deliver(buffer));
    },
    closeSocket: (name, code, reason) => {
      const s = socket(name);
      if (s.readyState === 1) s.readyState = 2;
      later(() => s.finish(code, reason, true));
    },
    failSocket: (name) => later(() => socket(name).fail()),

    // Event sources, the same way.
    sources: (name) => sources.get(name)?.length ?? 0,
    withCredentials: (name) => source(name).withCredentials,
    closedByPage: (name) => source(name).closedByPage,
    sourceListeners: (name) => source(name).listeners,
    openSource: (name) =>
      later(() => {
        const s = source(name);
        if (s.readyState === 2) return;
        s.readyState = 1;
        s.dispatchEvent(new Event("open"));
      }),
    sendEvent: (name, type, data, lastEventId) =>
      later(() => {
        const s = source(name);
        if (s.readyState !== 1) return;
        s.dispatchEvent(new MessageEvent(type, { data, lastEventId }));
      }),
    // The connection drops, and the browser will reconnect; or, given up,
    // the source is closed for good.
    failSource: (name, givenUp) =>
      later(() => {
        const s = source(name);
        if (s.readyState === 2) return;
        s.readyState = givenUp ? 2 : 0;
        s.dispatchEvent(new Event("error"));
      }),

    reset: () => {
      for (const all of sockets.values()) for (const s of all) later(() => s.finish(1001, "reset", false));
      for (const all of sources.values()) for (const s of all) s.close();
      sockets.clear();
      sources.clear();
    },
  };
})();
