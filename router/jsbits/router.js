// The browser half of Halogen.Router: the address, the session history and
// what a click on a link was. The GHC JavaScript backend links this file and
// calls these by name; the wasm backend embeds it into its JSFFI module
// (Halogen.JSBits).

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

// A click the browser would follow as it is, in this tab: the main button,
// no key held that asks for a new tab or window or a download, nothing that
// prevented it already, a link that does not name another target (_self,
// in any case, as reserved target names are compared), and one
// that is not a download (a download attribute, even an empty one, asks the
// browser to save what the link points to rather than show it).
function js_router_plain_left_click(event) {
  const link = event.currentTarget;
  return (
    event.button === 0 &&
    !event.ctrlKey &&
    !event.metaKey &&
    !event.shiftKey &&
    !event.altKey &&
    !event.defaultPrevented &&
    !(link && link.target && link.target.toLowerCase() !== "_self") &&
    !(link && link.hasAttribute && link.hasAttribute("download"))
  );
}
