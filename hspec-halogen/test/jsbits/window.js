// The page's own events, for the Window example's tests: what the browser
// dispatches when the window is resized or the network comes and goes, and
// when the tab is hidden or shown. A test cannot resize a headless window,
// take its network away or switch its tab, so it dispatches the events
// itself. The emitters cannot tell the difference: they read what they
// report as the event arrives (the window's size, the document's
// visibility), or take it from the event's type (online and offline).

function halogen_window_dispatch(type) {
  window.dispatchEvent(new Event(type));
}

// A headless page is always visible, so the document's visibilityState is
// overridden by a getter of its own, on the document itself, where it
// shadows its prototype's, for as long as the test needs; and the change is
// dispatched as the browser dispatches it: to the document, bubbling.
function halogen_window_set_visibility(state) {
  Object.defineProperty(document, "visibilityState", {configurable: true, get: () => state});
  document.dispatchEvent(new Event("visibilitychange", {bubbles: true}));
}

// Take the override away, which uncovers the prototype's getter again.
function halogen_window_restore_visibility() {
  delete document.visibilityState;
}
