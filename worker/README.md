# haskell-halogen-worker

Run expensive Haskell functions in Web Workers. The browser can keep drawing
and handling input while a worker computes the result.

`forkIO` and a component's `liftIO` let other Haskell threads run, but browser
Haskell still shares the page's JavaScript thread. A long calculation on that
thread can freeze the UI. A Web Worker runs on another browser thread and has
its own Haskell heap.

## Define a job and run it

A job is a function from an argument to a result, both with `Binary` instances
(`Data.Binary`; derive one with `deriving anyclass (Binary)` and `Generic`, and
use a tuple or a record for several arguments). Make it with `Worker.job` under
`static`, from GHC's `StaticPointers` extension:

```haskell
{-# LANGUAGE StaticPointers #-}

import GHC.StaticPtr (StaticPtr)
import Halogen.Worker qualified as Worker

primes :: StaticPtr (Worker.Job Int Int)
primes = static (Worker.job (\n -> pure (countPrimes n)))
```

`main` starts with `Worker.program`; then create a pool and pass it to the
components that need it:

```haskell
main :: IO ()
main = Worker.program $ runBrowserDOM $ do
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

`run` waits for the result. `start` gives a `Halogen.JSBits.Promise` of it at
once instead, so several jobs can be given to the pool before anything waits,
and the pool's workers compute them side by side. `Halogen.JSBits.Promise`'s
vocabulary then says how to wait:

```haskell
counts <- liftIO (mapConcurrently (Worker.start pool primesBetween) parts)
```

`mapConcurrently` gives the results in the order of `parts`, whichever worker
finished first. Two consecutive `run`s in one `do` block wait for one another.

### Why a static pointer

The worker loads another copy of the same program and calls `main`.
`Worker.program` detects that it is running in a worker, answers the page's
requests and skips the UI code; on the page it starts the UI. A function cannot
be sent to the worker: it is a closure, code with whatever it captured, and what
it captured is in the page's heap. What the two copies share is the code, and
`static e` names a piece of it with a key that is the same in every copy of the
binary. The page sends that key; the worker finds the job in GHC's static
pointer table. There is no table of jobs to register or keep in step.

`static e` requires `e` to be closed: it may name top-level functions and
values, but no variable bound by a lambda, a `do` or a function's argument. The
compiler rejects anything else, so a job cannot depend on component state,
IORefs or other values that only exist on the page. Pass everything the
calculation needs as its argument. `Worker.job` resolves the `Binary` instances
where it is written and keeps the codecs inside the job, so the static value
carries everything needed to encode the argument and decode the result.

Export each static pointer binding from its module. When optimising, GHC 9.14
can rename a static pointer binding that its module keeps private while the
module's static pointer table still refers to the old name, and the program
then fails to link with an undefined reference to the binding's closure. An
exported binding keeps its name.

A job can call `Worker.whichWorker` to learn which of its pool's workers runs
it (`Just 0`, `Just 1`, ...), or `Nothing` when it runs on the page.

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
- `start pool job argument` and `run pool job argument` give the job to the
  next worker in round-robin order.
- Failures take two shapes. `run` throws `WorkerError {job, message}`, where
  `job` is where the job's `static` is (`Module:line:column`). The promise from
  `start` is rejected with a `JSError` whose `name` is `"WorkerError"` and whose
  `message` is that `WorkerError`'s text (`the job at Module:line:column failed:
  ...`), since a `Promise` is rejected with a `JSError`; `wait` throws it and
  `waitCatch` returns it. Both cover the job's own exception (one raised while
  its lazy result is encoded included), an argument the worker cannot decode, a
  result the page cannot decode, a key the worker's program does not have, a
  worker that did not start and a pool terminated while the job ran. In the
  worker the job's exception is caught and sent back; it does not stop the
  worker.
- `terminate pool` stops all workers and fails outstanding requests. The next
  job starts workers again.
- Cancelling a thread waiting in `run` cancels the wait. The worker continues
  computing. Use `terminate` to stop its computation; that affects the whole pool.
- `available` reports whether workers can be used. Without the loader
  configuration, outside a browser, in GHCi or inside a worker, the job runs on
  the caller's thread, through the same encoding, and `start`'s promise has
  settled by the time `start` returns. This fallback keeps the API usable, but
  does not prevent a long calculation from blocking the page.

`serve` is the lower-level entry point used by `program`. It returns `False`
after installing a worker's message handler, or `True` when the caller should
start the UI.

## Data on the wire

The argument and the result are encoded with `Data.Binary`. The page sends the
job's static key and the argument's bytes in an `ArrayBuffer`, which is
transferred to the worker rather than copied, and the worker answers with the
result's bytes the same way. A job is worth sending when it is seconds of work:
each call still encodes on one side and decodes on the other, and the bytes are
copied between each Haskell heap and its `ArrayBuffer`.

## Browser tests need pages

A [Web Worker](https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API/Using_web_workers)
has no DOM, focused element or browser input. It can test a pure
calculation, but cannot run an hspec-halogen component test. Moving the DOM
through `Halogen.Remote` would test another backend and would still require
pages for real browser behavior. Use hspec-halogen's isolated browser contexts
for parallel UI tests; reserve this package for the calculations those
components call.

## Tests

`Halogen-worker-test` checks encoding, decoding, the lookup by static key,
failures, `start`'s promises and the fallback natively and under Node on both
browser backends. Real workers are
checked in Chromium through `examples/worker`:

```sh
sh toolchain/build-wasm.sh worker
npm run test-worker
```

That test checks results, UI responsiveness during a calculation, a range
counted in parts by both workers through `start` and `mapConcurrently`,
failures, termination and the fallback without loader configuration.
