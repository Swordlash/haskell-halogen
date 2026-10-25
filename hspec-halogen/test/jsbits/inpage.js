// Takes the runner's actions away from the page for a test, and gives them
// back after it, so that the harness performs them itself with synthetic
// events, as in a page with no runner behind it (page.js's in-page mode).
// What tells the runner the suite is done stays.
function halogen_inpage_hide_runner() {
  const bridge = globalThis.__halogenTest;
  globalThis.__halogenTestHidden = bridge;
  globalThis.__halogenTest = bridge ? {done: bridge.done} : undefined;
}

function halogen_inpage_restore_runner() {
  globalThis.__halogenTest = globalThis.__halogenTestHidden;
  delete globalThis.__halogenTestHidden;
}
