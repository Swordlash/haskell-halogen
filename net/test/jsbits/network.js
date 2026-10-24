// The browser suite's view of test/web/network-double.js, which the runner
// loads into the page before the suite: whether a held or streamed request
// has reached the network, whether it was aborted there, and letting it go.
function halogen_net_test_started(name) {
  return globalThis.halogenNetTest.started(name);
}

function halogen_net_test_aborted(name) {
  return globalThis.halogenNetTest.aborted(name);
}

function halogen_net_test_release(name) {
  globalThis.halogenNetTest.release(name);
}
