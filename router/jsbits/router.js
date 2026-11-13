// The browser half of Halogen.Router: the address and the session history.
// What a click on a link was is haskell-halogen-core's
// (Halogen.VDom.DOM.HandlerOptions, jsbits/events.js). The GHC JavaScript
// backend links this file and calls these by name; the wasm backend embeds
// it into its JSFFI module (Halogen.JSBits).

function js_router_location() {
  return location.pathname + location.search + location.hash;
}

function js_router_push_state(url) {
  history.pushState(null, "", url);
}

function js_router_replace_state(url) {
  history.replaceState(null, "", url);
}

// The path of the page's <base href>, which is where a site served under a
// path of its own says what that path is; "/" for a page without one.
function js_router_page_base() {
  const base = document.querySelector("base[href]");
  return base ? new URL(base.href).pathname : "/";
}
