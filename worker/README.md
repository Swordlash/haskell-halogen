# haskell-halogen-worker

Run expensive Haskell functions in Web Workers. The browser can keep drawing
and handling input while a worker computes the result.

`forkIO` and a component's `liftIO` let other Haskell threads run, but browser
Haskell still shares the page's JavaScript thread. A long calculation on that
thread can freeze the UI. A Web Worker runs on another browser thread and has
its own Haskell heap.

## Register a job and run it

A job has a unique name, an argument type and a result type. Register every job
in `main`, then create a pool and pass it to the components that need it:

```haskell
import Halogen.Worker qualified as Worker

primes :: Worker.Job Int Int
primes = Worker.job "primes" (\n -> pure (countPrimes n))

main :: IO ()
main = Worker.program [Worker.some primes] $ runBrowserDOM $ do
  pool <- Worker.newPool 2
  body <- HA.awaitBody
  socket <- runUI (component pool) () body
  HA.keepForNextRun (socket.dispose >> Worker.terminate pool)
```

Call the job from a component:

```haskell
handleAction Count = do
  n <- liftIO (Worker.run pool primes 2000000)
  modify (\st -> st {primes = Just n})
```

`run` waits for the result. To use both workers at once, make concurrent calls
from separate fibers or Haskell threads. Two consecutive calls in one `do`
block wait for one another.

The worker loads another copy of the same program and calls `main`.
`Worker.program` detects that it is running in a worker, registers the jobs
and skips the UI code. On the page, it registers the same jobs and starts the UI.
A worker looks up a job by name; closures, IORefs and component state do not
cross between the two heaps. Pass everything the calculation needs as its
argument. Use a tuple or a record for several arguments.

## Configure the loader

Before starting the Haskell program, set the URLs of the files a worker must
load. For WebAssembly:

```js
globalThis.halogenWorker = {
  wasm: new URL("./app.wasm", import.meta.url).href,
  jsffi: new URL("./ghc_wasm_jsffi.js", import.meta.url).href,
  wasi: new URL("./wasi.js", import.meta.url).href,
};
```

`wasi.js` must export `@bjorn3/browser_wasi_shim` as an ES module. The binary
must export `hs_start`, as the repository's examples do. Workers are module
workers created from a Blob; the site's content security policy must allow
them. See `examples/worker` for a complete loader and bundle script.

For the GHC JavaScript backend:

```js
globalThis.halogenWorker = {
  js: new URL("./all.js", location.href).href,
};
```

These are classic workers that load the program with `importScripts`.

## Lifetime, failures and fallback

- `newPool n` creates a pool with at least one worker. Each worker starts on
  its first job. Startup loads and initializes a complete copy of the program;
  a pool of four workers has four additional Haskell heaps.
- `run pool job argument` assigns a job to the next worker in round-robin order.
  It throws `WorkerError {job, message}` if startup, decoding or execution fails.
  Exceptions raised while encoding a lazy result are job failures too.
- `terminate pool` stops all workers and fails outstanding requests. The next
  call to `run` starts workers again.
- Cancelling a thread waiting in `run` cancels the wait. The worker continues
  computing. Use `terminate` to stop its computation; that affects the whole pool.
- `available` reports whether workers can be used. Without the loader
  configuration, outside a browser, in GHCi or inside a worker, `run` executes
  the registered job on the caller's thread. It uses the same encoding and
  lookup rules. This fallback keeps the API usable, but does not prevent a
  long calculation from blocking the page.

`serve jobs` is the lower-level entry point used by `program`. It returns
`False` after installing a worker's message handler, or `True` when the caller
should start the UI. Job names must be unique in the registration list.

## JSON and binary data

The current API encodes arguments and results as JSON text using aeson. It is
suited to expensive calculations with small inputs and results. JavaScript
only routes the text; the Haskell job decodes it. Sending a large payload costs
encoding, copying and decoding on both sides.

A binary codec such as `cereal` could reduce the size and parsing cost for some
payloads. It would still need a byte transport: Haskell bytes must become a
JavaScript `Uint8Array` and be copied into the receiving Haskell heap. A
transferable `ArrayBuffer` can avoid an extra copy between JavaScript contexts;
it does not share the two Haskell heaps. The existing JSBits byte conversions
provide the first step, but the worker API currently accepts JSON text only.

Keep JSON for the existing typed jobs. A future binary API should accept a
codec and transferable bytes, so an application can choose `cereal`, another
format or raw bytes. Changing the default codec without measuring representative
payloads would add a dependency and a protocol change without establishing a
speedup. There is no measured JSON-versus-binary benchmark in this package yet.

## Browser tests need pages

A [Web Worker](https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API/Using_web_workers)
has no DOM, focused element or browser input. It can test a pure
calculation, but cannot run an hspec-halogen component test. Moving the DOM
through `Halogen.Remote` would test another backend and would still require
pages for real browser behavior. Use hspec-halogen's isolated browser contexts
for parallel UI tests; reserve this package for the calculations those
components call.

## Tests

`Halogen-worker-test` checks registration, encoding, decoding, failures and the
fallback natively and under Node on both browser backends. Real workers are
checked in Chromium through `examples/worker`:

```sh
sh toolchain/build-wasm.sh worker
npm run test-worker
```

That test checks results, UI responsiveness during a calculation, failures,
termination and the fallback without loader configuration.
