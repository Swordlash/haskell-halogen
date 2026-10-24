// The browser's WebSocket, for Halogen.Net.WebSocket. The GHC JavaScript
// backend links this file and calls these by name; the wasm backend embeds
// it into its JSFFI module (Halogen.JSBits.browserJS).
//
// A socket is made and given its listeners in one call, so that no event can
// come between the two: the browser dispatches a socket's events as tasks of
// their own, after the one that made it. Each listener calls one Haskell
// callback, and the close listener first takes all of them off the socket:
// close is the last event a socket has, and the Haskell side frees the
// callbacks once it has handled it, which must not leave the socket able to
// call one.
//
// What the constructor, send or close throw is returned in place of their
// result, for Halogen.Net.Internal.refusal to read.

function halogen_net_websocket_protocols() {
  return [];
}

function halogen_net_websocket_protocol_push(protocols, protocol) {
  protocols.push(protocol);
}

function halogen_net_websocket_open(url, protocols, onOpen, onText, onBytes, onError, onClose) {
  let socket;
  try {
    socket = new WebSocket(url, protocols);
  } catch (error) {
    return error;
  }
  // Binary messages arrive as an ArrayBuffer, which is read at once, rather
  // than as a Blob (the browser's default), which would have to be waited
  // for.
  socket.binaryType = "arraybuffer";
  const opened = () => onOpen(null);
  const message = (event) => {
    if (typeof event.data === "string") onText(event.data);
    else onBytes(new Uint8Array(event.data));
  };
  const failed = () => onError(null);
  const closed = (event) => {
    socket.removeEventListener("open", opened);
    socket.removeEventListener("message", message);
    socket.removeEventListener("error", failed);
    socket.removeEventListener("close", closed);
    onClose(event);
  };
  socket.addEventListener("open", opened);
  socket.addEventListener("message", message);
  socket.addEventListener("error", failed);
  socket.addEventListener("close", closed);
  return socket;
}

function halogen_net_websocket_close_code(event) {
  return event.code;
}

function halogen_net_websocket_close_reason(event) {
  return event.reason;
}

function halogen_net_websocket_close_clean(event) {
  return event.wasClean;
}

// A string, or a Uint8Array of its own (Halogen.JSBits.toJSBytes), which
// the socket sends as its bytes. Null once the socket has taken it.
function halogen_net_websocket_send(socket, data) {
  try {
    socket.send(data);
    return null;
  } catch (error) {
    return error;
  }
}

// A negative code closes with none, and with no reason.
function halogen_net_websocket_close(socket, code, reason) {
  try {
    if (code < 0) socket.close();
    else socket.close(code, reason);
    return null;
  } catch (error) {
    return error;
  }
}

function halogen_net_websocket_ready_state(socket) {
  return socket.readyState;
}

function halogen_net_websocket_protocol(socket) {
  return socket.protocol;
}
