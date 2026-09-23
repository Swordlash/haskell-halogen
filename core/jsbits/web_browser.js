// The browser globals bound by the storage half of MonadBrowserDOM,
// Web.HTML.Window and the cookie half of Web.HTML.HTMLDocument. The GHC JavaScript backend calls these
// by name; the wasm backend writes the same expressions inline.

function js_storage_of(kind) {
  return kind === "local" ? window.localStorage : window.sessionStorage;
}

function js_storage_read(kind, key) {
  return js_storage_of(kind).getItem(key);
}

function js_storage_write(kind, key, value) {
  js_storage_of(kind).setItem(key, value);
}

function js_storage_remove(kind, key) {
  js_storage_of(kind).removeItem(key);
}

function js_storage_length(kind) {
  return js_storage_of(kind).length;
}

function js_storage_key(kind, index) {
  return js_storage_of(kind).key(index);
}

function js_window_inner_width(window) {
  return window.innerWidth;
}

function js_window_inner_height(window) {
  return window.innerHeight;
}

function js_window_location_hash(window) {
  return window.location.hash;
}

function js_document_cookie(document) {
  return document.cookie;
}

function js_document_set_cookie(value, document) {
  document.cookie = value;
}
