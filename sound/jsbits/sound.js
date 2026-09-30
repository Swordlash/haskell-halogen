// The browser backend of haskell-halogen-sound, for the javascript backend
// (wasm has the same inline, in Halogen/Sound/Browser.hs).

function halogen_sound_fetch(url, done) {
  fetch(url, { priority: "low" })
    .then(function (r) { return r.ok ? r.blob() : null; })
    .then(function (b) { done(b ? URL.createObjectURL(b) : null); })
    .catch(function () { done(null); });
}

function halogen_sound_release(url) { URL.revokeObjectURL(url); }

// Before the page's first click or key a browser refuses to play; the voice
// then waits for one (unless it has been stopped by then).
function halogen_sound_start(url, volume, looping, heard, ended) {
  var a = new Audio(url);
  a.volume = volume;
  a.loop = looping;
  a.onplaying = function () { heard(null); };
  a.onended = function () { ended(null); };
  // A file that cannot be played ends at once, so that the album goes on.
  a.onerror = function () { if (!a.__halogenStopped) ended(null); };
  var go = function () {
    if (a.__halogenStopped || a.__halogenPaused) return;
    a.play().catch(function (e) {
      // Only the page's want of a first click is waited out.
      // A pause right after the start aborts the play: not an end.
      if (!e || e.name !== "NotAllowedError") { if (!a.__halogenStopped && !a.__halogenPaused) ended(null); return; }
      var retry = function () {
        removeEventListener("pointerdown", retry, true);
        removeEventListener("keydown", retry, true);
        go();
      };
      addEventListener("pointerdown", retry, true);
      addEventListener("keydown", retry, true);
    });
  };
  go();
  return a;
}

function halogen_sound_stop(a) {
  a.__halogenStopped = true;
  a.onplaying = null;
  a.onended = null;
  a.onerror = null;
  a.pause();
  a.removeAttribute("src");
  a.load();
}

function halogen_sound_volume(a, volume) { a.volume = Math.min(1, Math.max(0, volume)); }

// A voice held before it could start (the page's first click) starts on
// resuming.
function halogen_sound_pause(a) { a.__halogenPaused = true; a.pause(); }

function halogen_sound_resume(a) {
  a.__halogenPaused = false;
  if (!a.__halogenStopped) a.play().catch(function () {});
}
