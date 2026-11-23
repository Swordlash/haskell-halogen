// The page's activity probe when runtimes of different versions meet on
// one page: core/jsbits/activity.js and remote/jsbits/remote.js each add an
// asker to the shared list on globalThis.__halogenProbe, and whichever came
// first made the list. Each runtime runs in a context of its own, as each
// program's JavaScript has a scope of its own in a page, around the one
// probe object. The older runtime is core's activity.js as it was before
// asleep, whose sum knew three counts and was set only by the runtime that
// made the list.
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import test from "node:test";
import vm from "node:vm";

const source = (path) => readFileSync(new URL(path, import.meta.url), "utf8");
const activityJs = source("../../jsbits/activity.js");
const remoteJs = source("../../../remote/jsbits/remote.js");

// core/jsbits/activity.js before asleep.
const olderActivityJs = `
const js_activity_programs = Symbol.for("halogen.activity.programs");
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
`;

// A runtime of a page with the probe, answering with this reading. Core's
// runtimes answer through their install's callback, as the Haskell side
// does; remote.js adds an asker of its own.
const runtimes = {
  older: (probe, reading) => {
    const context = vm.createContext({ __halogenProbe: probe });
    vm.runInContext(olderActivityJs, context);
    context.js_activity_probe_install((resolve) => resolve(reading));
  },
  newer: (probe, reading) => {
    const context = vm.createContext({ __halogenProbe: probe });
    vm.runInContext(activityJs, context);
    context.js_activity_probe_install((resolve) =>
      context.js_activity_answer(resolve, reading.trees, reading.running, reading.waiting, reading.asleep),
    );
  },
  remote: (probe, reading) => {
    const context = vm.createContext({ __halogenProbe: probe });
    vm.runInContext(remoteJs, context);
    context.halogen_remote_probe_install(() => Promise.resolve(context.halogen_remote_reading(reading)));
  },
};

// An idle program of the older runtime (its answer has no asleep), and a
// program of the newer one with a fiber asleep on a test's clock.
const idleOlder = { trees: 1, running: 0, waiting: 0 };
const asleepNewer = { trees: 1, running: 0, waiting: 1, asleep: 1 };
const expected = { trees: 2, running: 0, waiting: 1, asleep: 1 };

const sumOf = async (installs) => {
  const probe = {};
  for (const [runtime, reading] of installs) runtimes[runtime](probe, reading);
  return { ...(await probe.activity()) };
};

test("an older runtime that made the list first: the newer one's sum has asleep", async () => {
  assert.deepEqual(await sumOf([["older", idleOlder], ["newer", asleepNewer]]), expected);
});

test("the newer runtime first, an older one after: the older's answers add no asleep", async () => {
  assert.deepEqual(await sumOf([["newer", asleepNewer], ["older", idleOlder]]), expected);
});

test("remote.js after an older runtime, and before one, sums asleep as core does", async () => {
  assert.deepEqual(await sumOf([["older", idleOlder], ["remote", asleepNewer]]), expected);
  assert.deepEqual(await sumOf([["remote", asleepNewer], ["older", idleOlder]]), expected);
  assert.deepEqual(await sumOf([["remote", asleepNewer], ["newer", { ...idleOlder, asleep: 0 }]]), expected);
});

test("a remote answer without asleep, from an older session, reads as none asleep", async () => {
  assert.deepEqual(await sumOf([["remote", idleOlder]]), { trees: 1, running: 0, waiting: 0, asleep: 0 });
});
