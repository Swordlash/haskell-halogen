// Synthetic events for Test.Delegated, dispatched in the page, each saying
// whether a listener prevented its default. A click's default is a link
// followed: a listener on the window, which hears the click last, notes
// whether it was prevented and then prevents it itself, so that no link the
// test clicks this way is followed, whatever the component did.

function halogen_delegated_target(selector, text) {
  var element = document.querySelector(selector);
  if (!element) throw new Error("no element for " + selector);
  // The text node inside, for a click whose target is not an element.
  return text ? element.firstChild : element;
}

function halogen_delegated_click(selector, text, ctrlKey) {
  var target = halogen_delegated_target(selector, text);
  var prevented = null;
  var last = function (event) {
    prevented = event.defaultPrevented;
    event.preventDefault();
  };
  window.addEventListener("click", last);
  try {
    target.dispatchEvent(new MouseEvent("click", { bubbles: true, cancelable: true, button: 0, ctrlKey: ctrlKey }));
  } finally {
    window.removeEventListener("click", last);
  }
  return prevented === true;
}

// A transfer of the test's own: a file, or a text.
function halogen_delegated_transfer(withFile) {
  var transfer = new DataTransfer();
  if (withFile) transfer.items.add(new File(["abc"], "a.txt", { type: "text/plain" }));
  else transfer.setData("text/plain", "words");
  return transfer;
}

function halogen_delegated_paste(selector, withFile) {
  var event = new ClipboardEvent("paste", { bubbles: true, cancelable: true, clipboardData: halogen_delegated_transfer(withFile) });
  halogen_delegated_target(selector, false).dispatchEvent(event);
  return event.defaultPrevented;
}

function halogen_delegated_drag(selector, type, withFile) {
  var event = new DragEvent(type, { bubbles: true, cancelable: true, dataTransfer: halogen_delegated_transfer(withFile) });
  halogen_delegated_target(selector, false).dispatchEvent(event);
  return event.defaultPrevented;
}

function halogen_delegated_hash() {
  return location.hash;
}
