// Promises, for Halogen.JSBits.Promise. The GHC JavaScript backend links this
// file and calls these by name; the wasm backend embeds it into its JSFFI
// module (Halogen.JSBits.browserJS).
//
// Waiting for a promise is one awaited import, which on the JavaScript
// backend may only return nothing. So a wait is in two halves: an outcome,
// an object made at once that holds the promise, and
// halogen_promise_settle, which resolves once the promise has settled and
// never rejects, leaving what it settled with on the outcome for the
// Haskell side to read afterwards. A rejection is reported the same way on
// both backends that way, rather than as whatever each makes of a rejected
// promise, and the Haskell side reads it as a value.

function halogen_promise_outcome(promise) {
  return { promise: promise, ok: false, value: undefined, name: "", message: "" };
}

// What a rejection says, as a name and a message: an Error's (a
// DOMException is one), and for anything else, which a promise may be
// rejected with too, "Error" and the value as a string. Turning a value into
// a string can itself throw (an object whose toString does); then the
// message is left empty rather than the settling rejected.
function halogen_promise_rejected(outcome, error) {
  outcome.ok = false;
  try {
    if (error !== null && typeof error === "object" && typeof error.name === "string") {
      outcome.name = error.name;
      outcome.message = String(error.message ?? "");
    } else {
      outcome.name = "Error";
      outcome.message = String(error);
    }
  } catch (_) {
    outcome.name = outcome.name || "Error";
    outcome.message = "";
  }
}

// Resolve once the outcome's promise has settled, never rejecting.
// Promise.resolve lets a value that is no promise at all stand for one that
// has already resolved with it.
function halogen_promise_settle(outcome) {
  return Promise.resolve(outcome.promise).then(
    (value) => {
      outcome.ok = true;
      outcome.value = value;
      outcome.promise = undefined;
    },
    (error) => {
      halogen_promise_rejected(outcome, error);
      outcome.promise = undefined;
    },
  );
}

// An array to collect outcomes in, and one more outcome in it.
function halogen_promise_outcomes() {
  return [];
}

function halogen_promise_push(outcomes, outcome) {
  outcomes.push(outcome);
}

// Resolve once every outcome in the array has settled, never rejecting: one
// wait for all of them.
function halogen_promise_settle_all(outcomes) {
  return Promise.all(outcomes.map(halogen_promise_settle)).then(() => {});
}

function halogen_promise_ok(outcome) {
  return outcome.ok;
}

function halogen_promise_value(outcome) {
  return outcome.value;
}

function halogen_promise_name(outcome) {
  return outcome.name;
}

function halogen_promise_message(outcome) {
  return outcome.message;
}
