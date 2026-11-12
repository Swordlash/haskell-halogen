// The page's side of core's activity probe (Halogen.Activity), as a test
// driving the page from outside would use it: set the probe up, and ask it.
function halogen_probe_set() {
  if (globalThis.__halogenProbe === undefined) globalThis.__halogenProbe = {};
}

// "trees running waiting", or a rejection if no program has answered.
async function halogen_probe_ask() {
  const probe = globalThis.__halogenProbe;
  if (typeof probe?.activity !== "function") throw new Error("no program has answered the probe");
  const a = await probe.activity();
  return `${a.trees} ${a.running} ${a.waiting}`;
}
