// A stand-in for the network, for what the browser suite needs and the
// runner's file server cannot give: a request that stays pending until the
// test lets it go or aborts it, a body that stops halfway, and a response
// that says what its request was. The runner loads every .js file in
// test/web into the page before the suite starts, so this wraps the page's
// fetch before anything calls it. Every other address goes to the real
// fetch, and so to the runner's server, untouched; so do held requests once
// they are let go.
//
//   held/<name>/<path>  waits until release(name), then fetches <path>; an
//                       abort meanwhile rejects it with the signal's reason
//                       (an AbortError) and is noted.
//   stream/<name>       a 200 at once, whose body sends a part and then
//                       waits until release(name) to end; an abort
//                       meanwhile fails the body with the signal's reason
//                       and is noted.
//   echo                a 200 whose body is the request's, with headers
//                       x-echo-method, x-echo-credentials and x-echo-mode,
//                       and an x-echo-<name> for each header the request
//                       had.
//
// What it notes lives in the page, which browser GHCi (toolchain/dev-test.sh)
// keeps from one :main to the next, so the suite calls reset() as it starts:
// every name is forgotten, and a request still held from the run before is
// let go first, its answer to nobody.
(() => {
  const realFetch = globalThis.fetch.bind(globalThis);
  const names = new Map();
  const named = (name) => {
    if (!names.has(name)) names.set(name, { started: false, aborted: false, released: false, waiting: [] });
    return names.get(name);
  };

  globalThis.halogenNetTest = {
    started: (name) => named(name).started,
    aborted: (name) => named(name).aborted,
    release: (name) => {
      const entry = named(name);
      entry.released = true;
      entry.waiting.splice(0).forEach((go) => go());
    },
    reset: () => {
      for (const entry of names.values()) entry.waiting.splice(0).forEach((go) => go());
      names.clear();
    },
  };

  // Run go once the name is released, or abandon once the signal aborts,
  // whichever comes first.
  const whenReleased = (entry, signal, go, abandon) => {
    const onAbort = () => {
      entry.aborted = true;
      abandon(signal.reason);
    };
    if (signal?.aborted) return onAbort();
    signal?.addEventListener("abort", onAbort, { once: true });
    const released = () => {
      signal?.removeEventListener("abort", onAbort);
      go();
    };
    if (entry.released) released();
    else entry.waiting.push(released);
  };

  globalThis.fetch = (input, init = {}) => {
    const address = String(input instanceof Request ? input.url : input);
    const held = address.match(/(?:^|\/)held\/([^/]+)\/(.*)$/);
    if (held) {
      const entry = named(held[1]);
      entry.started = true;
      return new Promise((resolve, reject) =>
        whenReleased(entry, init.signal, () => resolve(realFetch(held[2], init)), reject),
      );
    }
    const stream = address.match(/(?:^|\/)stream\/([^/]+)$/);
    if (stream) {
      const entry = named(stream[1]);
      entry.started = true;
      const body = new ReadableStream({
        start(controller) {
          controller.enqueue(new TextEncoder().encode("partial"));
          whenReleased(entry, init.signal, () => controller.close(), (reason) => controller.error(reason));
        },
      });
      return Promise.resolve(new Response(body, { status: 200 }));
    }
    if (/(?:^|\/)echo$/.test(address)) {
      let request;
      try {
        request = new Request(input, init);
      } catch (error) {
        return Promise.reject(error);
      }
      return request.arrayBuffer().then((body) => {
        const headers = new Headers({
          "x-echo-method": request.method,
          "x-echo-credentials": request.credentials,
          "x-echo-mode": request.mode,
        });
        for (const [name, value] of request.headers) headers.append("x-echo-" + name, value);
        return new Response(body, { status: 200, statusText: "Echoed", headers });
      });
    }
    return realFetch(input, init);
  };
})();
