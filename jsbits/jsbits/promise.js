// Promises, for Halogen.JSBits.Promise. The GHC JavaScript backend links this
// file and calls these by name; the wasm backend embeds it into its JSFFI
// module (Halogen.JSBits.browserJS).
//
// A wait hands the promise two Haskell callbacks. Promise.resolve lets a
// value that is no promise at all stand for one that has already resolved
// with it. A rejection reaches the second callback as { ok: false, error },
// the outcome Halogen.JSBits.Value.checkThrown reads, so its name and
// message are read the way a synchronous call's exception is.
function halogen_promise_then(promise, resolve, reject) {
  Promise.resolve(promise).then(resolve, (error) => reject({ ok: false, error: error }));
}

// A resolved value read as a number or a boolean: the FFI converts what an
// import returns, so each type has an import over this.
function halogen_promise_number(value) {
  return Number(value);
}

function halogen_promise_boolean(value) {
  return value === true;
}

function halogen_promise_absent(value) {
  return value === null || value === undefined;
}

// An array a promise resolved with, read element by element. Anything that
// is not an array reads as an empty one.
function halogen_promise_array_length(value) {
  return Array.isArray(value) ? value.length : 0;
}

function halogen_promise_array_index(value, i) {
  return value[i];
}
