// The browser globals bound by Web.Storage.Storage, Web.HTML.Window and the
// cookie half of Web.HTML.HTMLDocument. The GHC JavaScript backend calls these
// by name; the wasm backend writes the same expressions inline.

function js_storage_get_item(key, storage) {
  return storage.getItem(key);
}

function js_storage_set_item(key, value, storage) {
  storage.setItem(key, value);
}

function js_storage_remove_item(key, storage) {
  storage.removeItem(key);
}

function js_storage_clear(storage) {
  storage.clear();
}

function js_storage_length(storage) {
  return storage.length;
}

function js_storage_key(index, storage) {
  return storage.key(index);
}

function js_window_local_storage(window) {
  return window.localStorage;
}

function js_window_session_storage(window) {
  return window.sessionStorage;
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
