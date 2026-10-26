// The mouse moving onto an element, for the suite (Test.Panel): the
// harness clicks and types, but has no hover, and the panel outlines a
// component when its row hears mouseenter.

function halogen_devtools_test_hover(selector) {
  const element = document.querySelector(selector);
  if (!element) throw new Error(`no element matches ${selector}`);
  element.dispatchEvent(new MouseEvent("mouseenter", { bubbles: false }));
}
