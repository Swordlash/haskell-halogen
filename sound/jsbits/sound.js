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
// then waits for one (unless it has been stopped by then). A voice started
// held plays nothing until resumed.
function halogen_sound_start(url, volume, looping, held, offset, heard, ended) {
  var a = new Audio(url);
  a.volume = volume;
  a.loop = looping;
  a.__halogenPaused = held;
  // A file that cannot be played ends at once, so that the album goes on;
  // while held, once resumed (a held voice does not end).
  var fail = function () {
    if (a.__halogenStopped) return;
    if (a.__halogenPaused) { a.__halogenFailed = true; return; }
    ended(null);
  };
  a.onplaying = function () { heard(null); };
  a.onended = function () { ended(null); };
  a.onerror = fail;
  var seek = Number.isFinite(offset) ? Math.max(0, offset) : 0;
  var place = function () {
    if (a.__halogenStopped || a.readyState < 1) return;
    if (seek > 0) {
      a.currentTime = Math.min(seek, Number.isFinite(a.duration) ? Math.max(0, a.duration - 0.001) : seek);
      seek = 0;
    }
  };
  a.onloadedmetadata = function () { place(); go(); };
  var go = function () {
    if (a.__halogenStopped || a.__halogenPaused) return;
    if (seek > 0 && a.readyState < 1) return;
    place();
    a.play().catch(function (e) {
      // A pause right after the play aborts it: not an end, the resume
      // plays again.
      if (a.__halogenStopped || a.__halogenPaused) return;
      // Only the page's want of a first click is waited out, by one
      // listener pair however often it is refused.
      if (!e || e.name !== "NotAllowedError") { fail(); return; }
      if (a.__halogenWaiting) return;
      a.__halogenWaiting = true;
      var retry = function () {
        removeEventListener("pointerdown", retry, true);
        removeEventListener("keydown", retry, true);
        a.__halogenWaiting = false;
        go();
      };
      addEventListener("pointerdown", retry, true);
      addEventListener("keydown", retry, true);
    });
  };
  a.__halogenGo = go;
  a.__halogenFail = fail;
  go();
  return a;
}

function halogen_sound_stop(a) {
  a.__halogenStopped = true;
  a.onplaying = null;
  a.onended = null;
  a.onerror = null;
  a.onloadedmetadata = null;
  a.pause();
  a.removeAttribute("src");
  a.load();
}

function halogen_sound_volume(a, volume) { a.volume = Math.min(1, Math.max(0, volume)); }

// A resume starts the voice as a start does: waiting for the page's first
// click if it is refused, and ending it if its file failed while held.
function halogen_sound_pause(a) { a.__halogenPaused = true; a.pause(); }

function halogen_sound_resume(a) {
  a.__halogenPaused = false;
  if (a.__halogenStopped) return;
  if (a.__halogenFailed) { a.__halogenFailed = false; a.__halogenFail(); } else a.__halogenGo();
}

function halogen_sound_time(audio) { return audio.currentTime; }
function halogen_sound_duration(audio) { return audio.duration; }
