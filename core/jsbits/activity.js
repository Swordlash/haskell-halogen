// The probe a page can ask whether its Halogen program is busy
// (Halogen.Activity). A page that wants to ask sets
//
//   globalThis.__halogenProbe = {};
//
// before the program starts, and once a tree is mounted the object has
//
//   await __halogenProbe.activity()   // {trees, running, waiting, asleep}
//
// A page can run more than one program (the tutorial runs the reader's
// beside its own): each adds its answer to a list kept on the object under
// a symbol, and activity() sums what every program answers. The GHC
// JavaScript backend links this file and calls these by name; the wasm
// backend embeds it into its JSFFI module (Halogen.JSBits).
//
// asleep is how many of the waiting fibers are in a counted pause asleep
// on a test's clock (Halogen.Activity), which quiet does not wait for. A
// program built before it answers without it, which counts as 0.

const js_activity_programs = Symbol.for("halogen.activity.programs");

function js_activity_probe_wanted() {
  const probe = globalThis.__halogenProbe;
  return probe !== null && typeof probe === "object";
}

// The callback is called with a function to resolve the answer's promise
// with, and calls it (through js_activity_answer) once it has read the
// program's trees: a Haskell callback returns nothing to its caller, and on
// the JavaScript backend it runs only after the call has returned.
//
// The probe is shared by every runtime in the page, of whatever version,
// and by remote.js: whichever comes first makes the list, and the others
// add to it. The sum is put on the probe by every runtime that adds to it,
// not only by the first, and keeps the askers already there: a runtime
// from before asleep that came first had a sum that dropped it. That one
// coming after leaves this sum in place (it sets its own only when it makes
// the list), and its answers, which lack asleep, add 0.
function js_activity_probe_install(callback) {
  const probe = globalThis.__halogenProbe;
  if (!Array.isArray(probe[js_activity_programs])) {
    Object.defineProperty(probe, js_activity_programs, { value: [], enumerable: false });
  }
  probe.activity = () => js_activity_sum(probe[js_activity_programs]);
  probe[js_activity_programs].push(() => new Promise((resolve) => callback(resolve)));
}

// What every asker answers, summed: each of the reading's counts, 0 where
// an answer lacks it, and any other count an answer has, so that a sum of
// a later version's put here by this one drops nothing it adds.
async function js_activity_sum(askers) {
  const answers = await Promise.all(askers.map((ask) => ask()));
  const sum = { trees: 0, running: 0, waiting: 0, asleep: 0 };
  for (const a of answers) {
    for (const [key, value] of Object.entries(a)) {
      if (typeof value === "number") sum[key] = (sum[key] || 0) + value;
    }
  }
  return sum;
}

function js_activity_answer(resolve, trees, running, waiting, asleep) {
  resolve({ trees, running, waiting, asleep });
}
