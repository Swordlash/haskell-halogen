// Where a node is in the window, for Halogen.DevTools' outline of the
// component a hovered row stands for. The GHC JavaScript backend links this
// file and calls these by name; the wasm backend embeds it into its JSFFI
// module (Halogen.JSBits.browserJS).
//
// A component's node is an element, or a text node for a component that
// renders only text, which has no getBoundingClientRect of its own: a range
// around it gives the box its text takes. A node no longer in the page has
// an empty box.

function halogen_devtools_rect(node) {
  if (!node || !node.isConnected) return { left: 0, top: 0, width: 0, height: 0 };
  if (typeof node.getBoundingClientRect === "function") return node.getBoundingClientRect();
  const range = document.createRange();
  range.selectNode(node);
  return range.getBoundingClientRect();
}

function halogen_devtools_rect_left(node) { return halogen_devtools_rect(node).left; }
function halogen_devtools_rect_top(node) { return halogen_devtools_rect(node).top; }
function halogen_devtools_rect_width(node) { return halogen_devtools_rect(node).width; }
function halogen_devtools_rect_height(node) { return halogen_devtools_rect(node).height; }
