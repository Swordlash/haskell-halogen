// Asks the network for something itself, so that the test sees a request
// no page made, and says what it got.
fetch("plain.html")
  .then((response) => response.text())
  .then((text) => postMessage(text.includes("No program at all") ? "fetched" : "fetched something else"));
