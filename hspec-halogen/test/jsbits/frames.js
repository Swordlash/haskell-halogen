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
