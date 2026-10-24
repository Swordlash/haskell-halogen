// Work that takes this many milliseconds, for the Promised example: a
// promise that resolves with the text, or one rejected with an Error named
// NotFoundError. Each returns the promise at once, the work under way.
function halogen_promised_later(ms, text) {
  return new Promise((resolve) => setTimeout(() => resolve(text), ms));
}

function halogen_promised_failing(ms) {
  return new Promise((_, reject) =>
    setTimeout(() => {
      const error = new Error("nothing here");
      error.name = "NotFoundError";
      reject(error);
    }, ms),
  );
}
