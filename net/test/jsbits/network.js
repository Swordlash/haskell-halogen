// The browser suite's view of test/web/network-double.js, which the runner
// loads into the page before the suite: whether a held or streamed request
// has reached the network, whether it was aborted there, letting it go, and
// forgetting every request at the start of a run.
function halogen_net_test_started(name) {
  return globalThis.halogenNetTest.started(name);
}

function halogen_net_test_aborted(name) {
  return globalThis.halogenNetTest.aborted(name);
}

function halogen_net_test_release(name) {
  globalThis.halogenNetTest.release(name);
}

// Without a page (the binary run as a Node script) there is no double, and
// nothing to forget.
function halogen_net_test_reset() {
  globalThis.halogenNetTest?.reset();
}
