// Sets, for one test, the page globals the runner fills from its
// environment, and gives them back after it: the deadline of the actions and
// settles (HSPEC_HALOGEN_DEADLINE_MS) and the retry report
// (HSPEC_HALOGEN_RETRIES). The report's lines are kept in a list the test
// reads, rather than written to standard error.
function halogen_settings_save() {
  globalThis.__halogenSettingsSaved = {
    deadline: globalThis.__halogenTestDeadlineMs,
    retries: globalThis.__halogenTestRetries,
  };
  globalThis.__halogenTestReportInto = [];
}

function halogen_settings_restore() {
  const saved = globalThis.__halogenSettingsSaved;
  delete globalThis.__halogenSettingsSaved;
  delete globalThis.__halogenTestReportInto;
  if ('__halogenSettingsBridge' in globalThis) {
    globalThis.__halogenTest = globalThis.__halogenSettingsBridge;
    delete globalThis.__halogenSettingsBridge;
    delete globalThis.__halogenSettingsSent;
  }
  if (saved.deadline === undefined) delete globalThis.__halogenTestDeadlineMs;
  else globalThis.__halogenTestDeadlineMs = saved.deadline;
  if (saved.retries === undefined) delete globalThis.__halogenTestRetries;
  else globalThis.__halogenTestRetries = saved.retries;
}

function halogen_settings_deadline(ms) {
  globalThis.__halogenTestDeadlineMs = ms;
}

// A boolean, as the runner sets it: wasm passes a Haskell Bool as a number.
function halogen_settings_retries(on) {
  globalThis.__halogenTestRetries = Boolean(on);
}

// The report's lines go to the runner, as they do outside these tests,
// through a runner whose report keeps them in a list of its own.
function halogen_settings_to_runner() {
  delete globalThis.__halogenTestReportInto;
  const bridge = globalThis.__halogenTest;
  globalThis.__halogenSettingsBridge = bridge;
  globalThis.__halogenSettingsSent = [];
  globalThis.__halogenTest = {...(bridge ?? {}), report: (line) => { globalThis.__halogenSettingsSent.push(line); }};
}

// What the runner was handed, one line each.
function halogen_settings_sent() {
  return (globalThis.__halogenSettingsSent ?? []).join('\n');
}

// The report's lines so far, one per line.
function halogen_settings_report() {
  return (globalThis.__halogenTestReportInto ?? []).join('\n');
}
