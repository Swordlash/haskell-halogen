// What the Dispatch example's synchronous effects dispatch, to elements of
// its own: the browser runs each event's handlers before these return, so
// what they report is what the handlers decided.

// A custom event, cancelable and bubbling; whether a handler cancelled it.
function halogen_dispatch_poke(element) {
  return !element.dispatchEvent(new Event('poke', {bubbles: true, cancelable: true}));
}

// A checkbox clicked as a script clicks it; whether it is checked once the
// click's handlers have run (a handler that prevents the default unchecks
// it again).
function halogen_dispatch_click_box(element) {
  element.click();
  return element.checked;
}
