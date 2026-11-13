// What a handler's declared match (Halogen.VDom.DOM.HandlerOptions) reads
// of an event that is not a plain field: whether a click is one the browser
// would follow in this tab. The GHC JavaScript backend links this file and
// calls it by name; the wasm backend embeds it into its JSFFI module
// (Halogen.JSBits).

// A click the browser would follow as it is, in this tab: the main button,
// no key held that asks for a new tab or window or a download, nothing that
// prevented it already, a link that does not name another target (_self,
// in any case, as reserved target names are compared), and one that is not
// a download (a download attribute, even an empty one, asks the browser to
// save what the link points to rather than show it). The link is the
// event's current target. An event a remote client sent
// (haskell-halogen-remote's loopback) is a plain object whose currentTarget
// says download: true for a link with the attribute.
function halogen_plain_left_click(event) {
  var link = event.currentTarget;
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
