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
function halogen_sound_start(url, volume, looping, ended) {
  var a = new Audio(url);
  a.volume = volume;
  a.loop = looping;
  a.onended = function () { ended(null); };
  var go = function () {
    if (a.__halogenStopped) return;
    a.play().catch(function () {
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
  a.onended = null;
  a.pause();
  a.removeAttribute("src");
  a.load();
}
