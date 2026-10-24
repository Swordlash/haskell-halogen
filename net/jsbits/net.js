// What Halogen.Net.Internal reads of an exception that a binding caught and
// returned in place of its result: a browser constructor or method that
// throws (new WebSocket for an address it will not connect to, send on a
// socket still connecting) is caught in that binding's JavaScript, so that
// the program gets it as a value rather than as an exception thrown across
// the FFI. A DOMException is an Error in every engine this runs in.

function halogen_net_refused(value) {
  return value instanceof Error || (typeof DOMException !== "undefined" && value instanceof DOMException);
}

function halogen_net_error_name(error) {
  return String(error.name);
}

function halogen_net_error_message(error) {
  return String(error.message);
}
