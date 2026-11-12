// The probe a page can ask whether its Halogen program is busy
// (Halogen.Activity). A page that wants to ask sets
//
//   globalThis.__halogenProbe = {};
//
// before the program starts, and once a tree is mounted the object has
//
//   await __halogenProbe.activity()   // {trees, running, waiting}
//
// A page can run more than one program (the tutorial runs the reader's
// beside its own): each adds its answer to a list kept on the object under
// a symbol, and activity() sums what every program answers. The GHC
// JavaScript backend links this file and calls these by name; the wasm
// backend embeds it into its JSFFI module (Halogen.JSBits).

const js_activity_programs = Symbol.for("halogen.activity.programs");

function js_activity_probe_wanted() {
  const probe = globalThis.__halogenProbe;
  return probe !== null && typeof probe === "object";
}

// The callback is called with a function to resolve the answer's promise
// with, and calls it (through js_activity_answer) once it has read the
// program's trees: a Haskell callback returns nothing to its caller, and on
// the JavaScript backend it runs only after the call has returned.
function js_activity_probe_install(callback) {
  const probe = globalThis.__halogenProbe;
  if (!Array.isArray(probe[js_activity_programs])) {
    Object.defineProperty(probe, js_activity_programs, { value: [], enumerable: false });
    probe.activity = async () => {
      const answers = await Promise.all(probe[js_activity_programs].map((ask) => ask()));
      return answers.reduce(
        (sum, a) => ({ trees: sum.trees + a.trees, running: sum.running + a.running, waiting: sum.waiting + a.waiting }),
        { trees: 0, running: 0, waiting: 0 },
      );
    };
  }
  probe[js_activity_programs].push(() => new Promise((resolve) => callback(resolve)));
}

function js_activity_answer(resolve, trees, running, waiting) {
  resolve({ trees, running, waiting });
}
