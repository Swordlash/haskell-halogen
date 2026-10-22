// The browser globals bound by the storage half of MonadBrowserDOM,
// Web.HTML.Window and the cookie half of Web.HTML.HTMLDocument. The GHC
// JavaScript backend links this file and calls these by name; the wasm
// backend embeds it into its JSFFI module (Halogen.JSBits).

function js_storage_of(kind) {
  return kind === "local" ? globalThis.localStorage : globalThis.sessionStorage;
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

// crypto.randomUUID exists only in a secure context (HTTPS or localhost); a
// page served over plain HTTP, say on a private network, has only
// getRandomValues, which is enough to make the same thing: an RFC 4122
// version 4 UUID, 122 random bits with the version and variant set. The page's
// crypto is left as it is.
function js_crypto_random_uuid() {
  const crypto = globalThis.crypto;
  if (typeof crypto.randomUUID === "function") {
    return crypto.randomUUID();
  }
  const b = crypto.getRandomValues(new Uint8Array(16));
  b[6] = (b[6] & 0x0f) | 0x40;
  b[8] = (b[8] & 0x3f) | 0x80;
  const h = Array.from(b, (x) => x.toString(16).padStart(2, "0")).join("");
  return [h.slice(0, 8), h.slice(8, 12), h.slice(12, 16), h.slice(16, 20), h.slice(20)].join("-");
}
