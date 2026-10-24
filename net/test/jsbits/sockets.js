// The browser suite's view of test/web/socket-double.js, which the runner
// loads into the page before the suite: driving the connections it fakes,
// reading back what the page did with them, and forgetting all of them at
// the start of a run.
function halogen_net_test_socket_count(name) {
  return globalThis.halogenNetSocketTest.sockets(name);
}

function halogen_net_test_socket_asked(name) {
  return globalThis.halogenNetSocketTest.asked(name);
}

function halogen_net_test_socket_sent(name) {
  return globalThis.halogenNetSocketTest.sent(name);
}

function halogen_net_test_socket_sent_is_text(name, i) {
  return typeof globalThis.halogenNetSocketTest.sentText(name, i) === "string";
}

function halogen_net_test_socket_sent_text(name, i) {
  return globalThis.halogenNetSocketTest.sentText(name, i);
}

function halogen_net_test_socket_sent_bytes(name, i) {
  return globalThis.halogenNetSocketTest.sentBytes(name, i);
}

// -1 if the page has not closed the socket, 0 if it closed it with no code.
function halogen_net_test_socket_closed_code(name) {
  return globalThis.halogenNetSocketTest.closedWith(name)?.code ?? -1;
}

function halogen_net_test_socket_closed_reason(name) {
  return globalThis.halogenNetSocketTest.closedWith(name)?.reason ?? "";
}

function halogen_net_test_socket_listeners(name) {
  return globalThis.halogenNetSocketTest.socketListeners(name);
}

function halogen_net_test_socket_accept(name, protocol) {
  globalThis.halogenNetSocketTest.accept(name, protocol);
}

function halogen_net_test_socket_send_text(name, text) {
  globalThis.halogenNetSocketTest.sendText(name, text);
}

function halogen_net_test_socket_send_bytes(name, bytes) {
  globalThis.halogenNetSocketTest.sendBytes(name, bytes);
}

function halogen_net_test_socket_close(name, code, reason) {
  globalThis.halogenNetSocketTest.closeSocket(name, code, reason);
}

function halogen_net_test_socket_fail(name) {
  globalThis.halogenNetSocketTest.failSocket(name);
}

function halogen_net_test_source_count(name) {
  return globalThis.halogenNetSocketTest.sources(name);
}

function halogen_net_test_source_with_credentials(name) {
  return globalThis.halogenNetSocketTest.withCredentials(name);
}

function halogen_net_test_source_closed(name) {
  return globalThis.halogenNetSocketTest.closedByPage(name);
}

function halogen_net_test_source_listeners(name) {
  return globalThis.halogenNetSocketTest.sourceListeners(name);
}

function halogen_net_test_source_open(name) {
  globalThis.halogenNetSocketTest.openSource(name);
}

function halogen_net_test_source_event(name, type, data, lastEventId) {
  globalThis.halogenNetSocketTest.sendEvent(name, type, data, lastEventId);
}

function halogen_net_test_source_fail(name, givenUp) {
  globalThis.halogenNetSocketTest.failSource(name, givenUp);
}

// Without a page (the binary run as a Node script) there is no double, and
// nothing to forget.
function halogen_net_test_sockets_reset() {
  globalThis.halogenNetSocketTest?.reset();
}
