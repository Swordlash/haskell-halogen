// The event operations in Web.Event.Event. The GHC JavaScript backend calls
// these by name; WASM embeds this file into its FFI module.

function js_prevent_default(event) {
  event.preventDefault();
}

function js_stop_propagation(event) {
  event.stopPropagation();
}

function js_stop_immediate_propagation(event) {
  event.stopImmediatePropagation();
}

function js_current_target(e) {
  return e.currentTarget;
}
