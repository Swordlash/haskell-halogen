// The browser globals bound by the storage half of MonadBrowserDOM,
// Web.HTML.Window and the cookie half of Web.HTML.HTMLDocument. The GHC JavaScript backend calls these
// by name; the wasm backend writes the same expressions inline.

function js_storage_read(kind, key) {
  const store = kind === "local" ? window.localStorage : window.sessionStorage;
  return store.getItem(key) ?? "";
}

function js_storage_write(kind, key, value) {
  const store = kind === "local" ? window.localStorage : window.sessionStorage;
  store.setItem(key, value);
}

function js_window_inner_width(window) {
  return window.innerWidth;
}

function js_window_inner_height(window) {
  return window.innerHeight;
}

function js_document_cookie(document) {
  return document.cookie;
}

function js_document_set_cookie(value, document) {
  document.cookie = value;
}
