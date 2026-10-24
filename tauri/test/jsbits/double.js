// The browser suite's view of test/web/tauri-double.js, which the runner
// loads into the page before the suite: the Rust side's emits, what the
// double saw, letting a held command answer, being outside Tauri, and
// forgetting everything at the start of a run.

// An event from the Rust side, to any target ("") or to a window's label.
function halogen_tauri_test_fire(target, event, payload) {
  globalThis.halogenTauriTest.fire(target === "" ? null : target, event, JSON.parse(payload));
}

// How many listeners there are for the event, or for any event ("").
function halogen_tauri_test_listening(event) {
  return globalThis.halogenTauriTest.listening(event === "" ? null : event);
}

function halogen_tauri_test_unlistens() {
  return globalThis.halogenTauriTest.unlistens();
}

// The emits the page made, as JSON: [{ target, event, payload }].
function halogen_tauri_test_emitted() {
  return JSON.stringify(globalThis.halogenTauriTest.emitted());
}

function halogen_tauri_test_destroyed() {
  return globalThis.halogenTauriTest.destroyed();
}

function halogen_tauri_test_release(name) {
  globalThis.halogenTauriTest.release(name);
}

function halogen_tauri_test_outside(hidden) {
  globalThis.halogenTauriTest.outside(hidden);
}

// Without a page (the binary run as a Node script) there is no double, and
// nothing to forget.
function halogen_tauri_test_reset() {
  globalThis.halogenTauriTest?.reset();
}
