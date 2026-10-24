// The browser's fetch, for Halogen.Net.Fetch. The GHC JavaScript backend
// links this file and calls these by name; the wasm backend embeds it into
// its JSFFI module (Halogen.JSBits.browserJS).
//
// A request is described in Haskell and made here in one call: the
// description is an object the Haskell side fills in, setter by setter, and
// halogen_net_fetch_start hands it to fetch with an AbortController's signal
// of its own. The description is then the fetch's handle, holding the
// controller that halogen_net_fetch_abort aborts.
//
// Every promise returned here has a rejection handler attached that does
// nothing. A fetch is often started and then dropped (a component that goes
// away aborts its request, and nobody waits to hear the AbortError), and the
// engine would otherwise report each such rejection as unhandled. Whoever
// does await the promise still gets the rejection: the handler is attached
// to it, not put in its place.

function halogen_net_quiet(promise) {
  promise.catch(() => {});
  return promise;
}

function halogen_net_fetch_request(method, url, credentials, mode) {
  return { method, url, credentials, mode, headers: [], body: undefined, controller: null };
}

// Headers are kept as pairs and checked by fetch itself, so that a name or
// value it refuses is a rejection of the fetch rather than an exception here.
function halogen_net_fetch_header(request, name, value) {
  request.headers.push([name, value]);
}

// A string or a Uint8Array.
function halogen_net_fetch_body(request, body) {
  request.body = body;
}

// Start the request and return the promise of its response, which resolves
// once the response's status and headers have arrived. Whatever fetch
// refuses, including what it would throw for, comes back as a rejection.
function halogen_net_fetch_start(request) {
  request.controller = new AbortController();
  try {
    return halogen_net_quiet(
      fetch(request.url, {
        method: request.method,
        headers: request.headers,
        body: request.body,
        credentials: request.credentials,
        mode: request.mode,
        signal: request.controller.signal,
      }),
    );
  } catch (error) {
    return halogen_net_quiet(Promise.reject(error));
  }
}

// Abort the request, if it was started: its response, or its body if the
// response has arrived, is then rejected with an AbortError. After the body
// has been read it does nothing.
function halogen_net_fetch_abort(request) {
  if (request.controller !== null) request.controller.abort();
}

function halogen_net_response_status(response) {
  return response.status;
}

function halogen_net_response_status_text(response) {
  return response.statusText;
}

function halogen_net_response_ok(response) {
  return response.ok;
}

function halogen_net_response_url(response) {
  return response.url;
}

// The headers as lines of name:value. A header's name has no colon in it and
// its value no line break (fetch refuses both), so the Haskell side splits
// each line at its first colon.
function halogen_net_response_headers(response) {
  return Array.from(response.headers, ([name, value]) => name + ":" + value).join("\n");
}

// The body, read to its end: a body can be read once, and reading it again
// is a rejection (a TypeError), as is reading it after it was aborted (an
// AbortError).
function halogen_net_response_text(response) {
  return halogen_net_quiet(response.text());
}

function halogen_net_response_bytes(response) {
  return halogen_net_quiet(response.arrayBuffer().then((buffer) => new Uint8Array(buffer)));
}
