// A promise that settles after this many milliseconds: what an island of the
// Islands example waits for when it starts, as an island of a real page
// waits for what draws it.
function halogen_islands_wait(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}
