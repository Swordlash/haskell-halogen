// The tutorial app's bindings (Learn.Browser), which call into what the page's
// learn.js bundle (examples/learn/js/learn.js) publishes on globalThis: the
// compiler and the editor. The JavaScript backend links this file through
// js-sources; the wasm backend embeds it (Halogen.JSBits.browserJS). What
// travels between the two sides that is more than a string is JSON.

function halogen_learn_api() {
  const api = globalThis.halogenLearn;
  if (!api) throw new Error("learn.js has not been loaded: the page must load it before the program");
  return api;
}

// Where the rootfs is: rootfs/ under the page's base.
function halogen_learn_rootfs() {
  return new URL("rootfs/", document.baseURI).href;
}

// Whether the screen is too small for the compiler to load by itself.
function halogen_learn_narrow() {
  return globalThis.matchMedia ? globalThis.matchMedia("(max-width: 900px)").matches : false;
}

function halogen_learn_confirm(message) {
  return globalThis.confirm(message);
}

function halogen_learn_scroll_to_top() {
  globalThis.scrollTo(0, 0);
}

// A JSON string: {"ghc":..., "base":bytes|null, "layers":[{"name":..., "size":...}]}.
function halogen_learn_describe(root) {
  return halogen_learn_api()
    .playground.describe(root)
    .then((d) => JSON.stringify(d));
}

// onProgress gets a JSON string each time the compiler's loading moves on.
function halogen_learn_start(root, layers, onProgress) {
  return halogen_learn_api().playground.start(root, JSON.parse(layers), (p) => onProgress(JSON.stringify(p)));
}

function halogen_learn_ensure_layers(layers) {
  return halogen_learn_api().playground.ensureLayers(JSON.parse(layers));
}

// request and the result are JSON strings; see playground.js's compile.
function halogen_learn_compile(request) {
  return halogen_learn_api()
    .playground.compile(JSON.parse(request))
    .then((result) => JSON.stringify(result));
}

function halogen_learn_end_run() {
  return halogen_learn_api().playground.endRun();
}

function halogen_learn_editor_create(element, text, readOnly, onEvent) {
  return halogen_learn_api().editor.create(element, text, readOnly, (event) => onEvent(event));
}

function halogen_learn_editor_text(view) {
  return halogen_learn_api().editor.text(view);
}

function halogen_learn_editor_set_text(view, text) {
  halogen_learn_api().editor.setText(view, text);
}

function halogen_learn_editor_set_read_only(view, readOnly) {
  halogen_learn_api().editor.setReadOnly(view, readOnly);
}

function halogen_learn_editor_diagnostics(view, diagnostics) {
  halogen_learn_api().editor.showDiagnostics(view, JSON.parse(diagnostics));
}

function halogen_learn_editor_goto(view, line, column) {
  halogen_learn_api().editor.goto(view, line, column);
}

function halogen_learn_editor_destroy(view) {
  halogen_learn_api().editor.destroy(view);
}
