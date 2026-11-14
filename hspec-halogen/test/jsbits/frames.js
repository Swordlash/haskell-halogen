// For Test.Late's page that draws no frames: requestAnimationFrame that
// never calls back, and a document that says it is hidden, each until
// restored (restoring what was not changed does nothing).
function halogen_frames_stop() {
  if (globalThis.__halogenFramesKept) return;
  globalThis.__halogenFramesKept = window.requestAnimationFrame;
  window.requestAnimationFrame = () => 0;
}

function halogen_frames_restore() {
  if (!globalThis.__halogenFramesKept) return;
  window.requestAnimationFrame = globalThis.__halogenFramesKept;
  delete globalThis.__halogenFramesKept;
}

function halogen_frames_hide() {
  Object.defineProperty(document, 'visibilityState', {get: () => 'hidden', configurable: true});
}

function halogen_frames_unhide() {
  delete document.visibilityState;
}

// Another harness's wrap of history.back(), put over the harness's own (as
// haskell-halogen-playwright's driver wraps it, though from an init script,
// under the harness's): it counts the calls and calls what it replaced.
function halogen_frames_wrap_back() {
  if (globalThis.__halogenFramesBack) return;
  const original = History.prototype.back;
  globalThis.__halogenFramesBack = {original, calls: 0};
  History.prototype.back = function back(...args) {
    globalThis.__halogenFramesBack.calls++;
    return original.apply(this, args);
  };
}

function halogen_frames_unwrap_back() {
  const kept = globalThis.__halogenFramesBack;
  if (!kept) return;
  History.prototype.back = kept.original;
  delete globalThis.__halogenFramesBack;
}

function halogen_frames_back_calls() {
  return globalThis.__halogenFramesBack?.calls ?? 0;
}

// A page that never stops moving through its history. It pushes four
// entries and goes back twice from the last; each popstate then asks for
// one more move, towards the third entry and away from it in turn, so that
// as one move arrives another is already asked for: there is never a moment
// with no move under way, and every move has somewhere to go.
function halogen_frames_restless_start() {
  for (let n = 1; n <= 4; n++) history.pushState({halogenRestless: n}, '');
  // Where the moves asked for so far lead.
  let heading = 2;
  globalThis.halogenRestless = () => {
    if (heading >= 3) {
      heading--;
      history.back();
    } else {
      heading++;
      history.forward();
    }
  };
  addEventListener('popstate', globalThis.halogenRestless);
  history.back();
  history.back();
}

// Stop it, let the moves it had asked for land, and go back to the entry
// it started from.
async function halogen_frames_restless_stop() {
  if (!globalThis.halogenRestless) return;
  removeEventListener('popstate', globalThis.halogenRestless);
  delete globalThis.halogenRestless;
  await new Promise((resolve) => setTimeout(resolve, 700));
  const over = history.state?.halogenRestless ?? 0;
  if (over > 0) {
    await new Promise((resolve) => {
      addEventListener('popstate', resolve, {once: true});
      setTimeout(resolve, 700);
      history.go(-over);
    });
  }
}
