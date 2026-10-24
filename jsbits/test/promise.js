// Promises for test/Promise.hs to await. Each settles on a later turn of the
// event loop, after the Haskell side has started awaiting it, as most work
// a binding starts does; a promise rejected before anything handles it is
// reported as an unhandled rejection, which ends a Node process.
function halogen_promise_test_text(ms, text) {
  return new Promise((resolve) => setTimeout(() => resolve(text), ms));
}

function halogen_promise_test_number(ms, n) {
  return new Promise((resolve) => setTimeout(() => resolve(n), ms));
}

function halogen_promise_test_error(ms, name, message) {
  return new Promise((_, reject) =>
    setTimeout(() => {
      const error = new Error(message);
      error.name = name;
      reject(error);
    }, ms),
  );
}

function halogen_promise_test_reject_with(ms, value) {
  return new Promise((_, reject) => setTimeout(() => reject(value), ms));
}

// A value that is no promise at all.
function halogen_promise_test_plain(text) {
  return text;
}

function halogen_promise_test_read_number(value) {
  return typeof value === "number" ? value : -1;
}

function halogen_promise_test_is_string(value) {
  return typeof value === "string";
}
