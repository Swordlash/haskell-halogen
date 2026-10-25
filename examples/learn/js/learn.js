// The tutorial's JavaScript, bundled by examples/learn/bundle.sh into the
// learn.js the page loads before app.wasm: the compiler (playground.js) and
// the code editor (editor.js). It publishes them on globalThis, where the
// app's bindings (examples/learn/jsbits/learn.js) find them.
import * as editor from "./editor.js";
import * as playground from "./playground.js";

globalThis.halogenLearn = { editor, playground };
