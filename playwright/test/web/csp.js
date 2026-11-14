// The script of csp.html and csp-meta.html, the only one their policy lets
// run: it plays a program's side of the probe, as program.html's does.
window.fake = { trees: 1, running: 0, waiting: 0 };
// What the policy says of eval in the page's own code: what Playwright's
// own waitForFunction does.
try {
  eval("1");
  window.evalRefused = "no";
} catch (error) {
  window.evalRefused = error.name;
}
if (globalThis.__halogenProbe) {
  __halogenProbe.activity = async () => ({ trees: fake.trees, running: fake.running, waiting: fake.waiting });
}
const byId = (id) => document.getElementById(id);
// Busy for a while after a click, as a long render would be.
byId("slow").addEventListener("click", () => {
  fake.running = 1;
  setTimeout(() => {
    byId("state").textContent = "done";
    fake.running = 0;
  }, 300);
});
// The page's own timer, not a program's: what the waits are for.
byId("later").addEventListener("click", () => {
  setTimeout(() => {
    byId("later").textContent = "later";
    byId("shown").hidden = false;
    console.log("later");
  }, 300);
});
// A fiber waiting in a liftIO, for a moment.
byId("wait").addEventListener("click", () => {
  fake.waiting = 1;
  setTimeout(() => {
    byId("state").textContent = "answered";
    fake.waiting = 0;
  }, 300);
});
