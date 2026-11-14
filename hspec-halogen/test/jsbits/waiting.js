// Example.Waiting's request: a promise resolved after this many
// milliseconds.
function halogen_waiting_after(ms) {
  return new Promise((resolve) => setTimeout(() => resolve(null), ms));
}
