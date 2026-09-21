// The event operations in Web.Event.Event. The GHC JavaScript backend calls
// these by name; the wasm backend writes the same calls inline.

function js_prevent_default(event) {
  event.preventDefault();
}

function js_stop_propagation(event) {
  event.stopPropagation();
}

function js_stop_immediate_propagation(event) {
  event.stopImmediatePropagation();
}
