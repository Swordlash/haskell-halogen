// What a handler's declared match (Halogen.VDom.DOM.HandlerOptions) reads
// of an event that is not a plain field: whether a click is one the browser
// would follow in this tab, the element a selector finds around the
// event's target, and the types of data a drag or a paste carries. The GHC
// JavaScript backend links this file and calls it by name; the wasm backend
// embeds it into its JSFFI module (Halogen.JSBits).
//
// Each also reads an event a remote client described (haskell-halogen-remote's
// loopback, where the program runs in the page and its events arrive as
// JSON): a plain object, whose elements are plain objects too
// (Halogen.Remote.Protocol spells them). None of them throws.

// The element whose listener is asking: the match is judged against it.
function halogen_current_target(event) {
  var element = event.currentTarget;
  return element === undefined ? null : element;
}

// A click the browser would follow as it is, in this tab: the main button,
// no key held that asks for a new tab or window or a download, nothing that
// prevented it already, a link that does not name another target (_self,
// in any case, as reserved target names are compared), and one that is not
// a download (a download attribute, even an empty one, asks the browser to
// save what the link points to rather than show it). The link is the
// element given: the listening one, or the one a selector found
// (halogen_within). A described link says download: true for a link with
// the attribute.
function halogen_plain_left_click_on(event, link) {
  return (
    event.button === 0 &&
    !event.ctrlKey &&
    !event.metaKey &&
    !event.shiftKey &&
    !event.altKey &&
    !event.defaultPrevented &&
    !(link && typeof link.target === "string" && link.target !== "" && link.target.toLowerCase() !== "_self") &&
    !(link && (link.download === true || (typeof link.hasAttribute === "function" && link.hasAttribute("download"))))
  );
}

// The element a CSS selector matches that is the event's target or its
// nearest ancestor, if it is the scope or inside it (the scope is the
// listening element, or the element an outer selector found); null for
// none. A target that is not an element (a text node) starts from its
// parent element. A selector that does not parse matches nothing: closest
// throws a SyntaxError for it, which out of a synchronous import would end
// a WebAssembly program.
//
// A described target carries what the client found for each selector of
// the listener's match, under closest; the client has checked already
// that each is where the match says.
function halogen_within(event, scope, selector) {
  var target = event.target;
  if (target === null || typeof target !== "object") return null;
  if (typeof Node === "function" && target instanceof Node) {
    if (target.nodeType !== 1) target = target.parentElement;
    if (!target) return null;
    var found;
    try {
      found = target.closest(selector);
    } catch (e) {
      return null;
    }
    if (!found) return null;
    if (scope && typeof scope.contains === "function" && !scope.contains(found)) return null;
    return found;
  }
  var closest = target.closest;
  if (closest === null || typeof closest !== "object" || !Object.prototype.hasOwnProperty.call(closest, selector)) return null;
  var described = closest[selector];
  return described !== null && typeof described === "object" ? described : null;
}

// Whether a drag's dataTransfer or a paste's clipboardData lists a type
// among its types ("Files", "text/plain", "text/uri-list").
function halogen_carrying(event, type) {
  var transfer = event.dataTransfer || event.clipboardData;
  if (!transfer || !transfer.types) return false;
  var types = transfer.types;
  for (var i = 0; i < types.length; i++) if (types[i] === type) return true;
  return false;
}
