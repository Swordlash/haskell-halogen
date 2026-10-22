// Dispatches a click to the Busy example's link and reports what the
// browser decided once the dispatch was over: 0 followed (nothing prevented
// the default), 1 prevented, 2 the click never reached the window (its
// propagation was stopped), 3 no such link. The window listener runs last,
// after every listener on the link and its ancestors, so it sees the final
// defaultPrevented; it then prevents the default itself, so the test page
// never navigates away whatever the component did.
function halogen_busy_click_link(n) {
  const link = document.querySelector('.busy-link-' + n);
  if (!link) return 3;
  let prevented = null;
  const last = (e) => { prevented = e.defaultPrevented; e.preventDefault(); };
  window.addEventListener('click', last);
  link.dispatchEvent(new MouseEvent('click', {bubbles: true, cancelable: true, button: 0}));
  window.removeEventListener('click', last);
  return prevented === null ? 2 : prevented ? 1 : 0;
}
