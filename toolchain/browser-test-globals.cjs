// The JS backend reaches storage through window; wasm uses globalThis.
// Both refer to Node's real Web Storage implementations in these tests.
globalThis.window = globalThis;
