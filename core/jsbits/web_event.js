// The event operations in Web.Event.Event, and what Web.UIEvent.KeyboardEvent
// and Web.UIEvent.MouseEvent read of their events. The GHC JavaScript backend
// calls these by name; WASM embeds this file into its FFI module.

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

function js_keyboard_key(e) {
  return e.key;
}

function js_keyboard_code(e) {
  return e.code;
}

function js_mouse_client_x(e) {
  return e.clientX;
}

function js_mouse_client_y(e) {
  return e.clientY;
}
