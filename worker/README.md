# haskell-halogen-worker

Run expensive Haskell functions in Web Workers so the browser can keep drawing and handling input while a worker computes the result.

`forkIO` and a component's `H.async` let other Haskell threads run, but those threads still share the page's JavaScript thread. A long calculation can therefore freeze the UI. A Web Worker runs on a separate browser thread with its own Haskell heap.

## Define a job and run it

A job wraps a function that returns its result in `IO`. It can take any number of arguments, specified in a type-level list (first type argument of `Worker.Job`). For instance, `Worker.Job '[Int, Int] Int` is a job wrapping `Int -> Int -> IO Int` action, `Worker.Job '[Int] Int` takes one argument and `Worker.Job '[] Int` takes none. The page and the worker communicate through `postMessage`, and this library uses `Data.Binary` to encode the values they exchange. The arguments and the result must therefore have `Binary` instances. You can derive these with `Generic` and `deriving anyclass (Binary)`.

Create the job with `Worker.job`, then use `static` to give it a static pointer. This requires GHC's `StaticPointers` extension:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StaticPointers #-}

import GHC.StaticPtr (StaticPtr)
import Halogen.Worker qualified as Worker

primes :: StaticPtr (Worker.Job '[Int] Int)
primes = static (Worker.job (\n -> pure (countPrimes n)))

primesBetween :: StaticPtr (Worker.Job '[Int, Int] Int)
primesBetween = static (Worker.job (\lo hi -> pure (countBetween lo hi)))
```

The list of argument types needs the `DataKinds` extension. The type signature is optional: GHC can infer the job's type from the function. For example, `static (Worker.job countSlowly)` with `countSlowly :: Int -> IO Int` has type `StaticPtr (Worker.Job '[Int] Int)`.

Wrap your `main` in `Worker.program`. Inside it, create a pool and pass it to the components that need workers:

```haskell
main :: IO ()
main = Worker.program $ runBrowserDOM $ do
  pool <- Worker.newPool 2
  body <- HA.awaitBody
  socket <- runUI (component pool) () body
  HA.keepForNextRun (socket.dispose >> Worker.terminate pool)
```

A component can now run the job and use its result:

```haskell
handleAction Count = do
  n <- H.async (liftIO (Worker.run pool primes 2000000))
  modify (\st -> st {primes = Just n})
```

Pass the job's arguments after the job, in order: `Worker.run pool primesBetween 0 1000000`. `run` and `start` return in `IO`. `run` waits, so call it in `H.async` in a component. `start` does not wait, so it can also go in `H.sync`.

`run` submits a job and waits for its result. `start` submits a job and immediately returns a `Halogen.JSBits.Promise`. This lets you submit several jobs before waiting, so the pool can run them in parallel. Use the functions from `Halogen.JSBits.Promise` to wait for their results:

```haskell
counts <- H.async (liftIO (mapConcurrently (uncurry (Worker.start pool primesBetween)) parts))
```

`mapConcurrently` returns results in the same order as `parts`, regardless of which worker finishes first. By contrast, two consecutive `run` calls in a `do` block run sequentially: the second is submitted only after the first returns.

### Why a static pointer

Each worker loads a copy of the same program and calls `main`. `Worker.program` checks where it is running. On the page, it starts your UI. In a worker, it handles job requests and skips the UI code.

The page cannot send a Haskell function directly to a worker. A function can capture values from the page's heap, which the worker cannot access. Instead, `static e` gives an expression a key that is the same in every copy of the binary. The page sends this key, and the worker uses GHC's static pointer table to find the job. You do not need to register jobs manually.

The expression inside `static` can refer to top-level functions and values, but it cannot capture local variables, such as a function argument or a value bound in a `do` block. GHC checks this at compile time. Pass component state and any other data the calculation needs as the job's arguments. A worker cannot access the page's IORefs or other values in its heap.

`Worker.job` also stores the `Binary` codecs in the job. Both copies of the program therefore have the code needed to encode and decode its arguments and results.

Export each static pointer binding from its module. GHC versions before 10.0 have a linker bug that can affect unexported static bindings used more than once. With optimisation enabled, the static pointer table may refer to a missing closure symbol. See [GHC issue #24773](https://gitlab.haskell.org/ghc/ghc/-/issues/24773).

Inside a job, `Worker.whichWorker` returns the worker's index in the pool, such as `Just 0` or `Just 1`. It returns `Nothing` if the job is running on the page.

## Configure the loader

Before starting the Haskell program, configure the URLs that workers use to load it. For WebAssembly:

```js
globalThis.halogenWorker = {
  wasm: new URL("./app.wasm", import.meta.url).href,
  jsffi: new URL("./ghc_wasm_jsffi.js", import.meta.url).href,
  wasi: new URL("./wasi.js", import.meta.url).href,
};
```

`wasi.js` must export `@bjorn3/browser_wasi_shim` as an ES module, and the binary must export `hs_start`. These workers are module workers created from a Blob, so the site's Content Security Policy must allow them. `examples/worker` includes a complete loader and bundle script.

For the GHC JavaScript backend:

```js
globalThis.halogenWorker = {
  js: new URL("./all.js", location.href).href,
};
```

This configuration creates classic workers that load the program with `importScripts`.

If your program needs a custom loader, provide your own module script. This works with either backend:

```js
globalThis.halogenWorker = {
  script: new URL("./worker-script.js", import.meta.url).href,
  data: { anything: "JSON-serialisable" },
};
```

Before importing your script, the worker sets `globalThis.halogenWorker.worker` to its worker index and copies the optional `data` value into `globalThis.halogenWorker.data`. Your script then loads the program and runs its `main`. The program must call `Worker.serve`, either directly or through `Worker.program`, before the script finishes importing. If the script throws or the program does not call `serve` in time, the worker's jobs fail with a `WorkerError`. When `script` is set, it takes precedence over `wasm` and `js`.

The tutorial uses this option because it runs lessons in GHC's interpreter. Its worker script starts a second compiler, compiles the same lesson and runs its `main`. You can also try a custom loader in `examples/worker`: open it with `?workers=script` to load `app.wasm` through `web/worker-script.js`.

## Lifetime, failures and fallback

`newPool n` creates a pool with at least one worker. Each worker starts when it receives its first job and loads a complete copy of the program. A pool of four workers therefore uses four additional Haskell heaps. `start` and `run` assign jobs to workers in round-robin order.

`terminate pool` stops all workers, stops any fallback jobs running in the current program, and fails all outstanding requests. The pool can be reused: submitting another job starts workers again.

### Failures

`run` throws `WorkerError {job, message}` when a job fails. The `job` field identifies the location of its `static` expression as `Module:line:column`, and `message` explains the failure.

`start` reports failures through its promise. The promise is rejected with a `JSError` whose `name` is `"WorkerError"` and whose `message` contains the formatted error: `the job at Module:line:column failed: ...`. Calling `wait` throws this error; `waitCatch` returns it.

Both APIs report exceptions from the job, including exceptions raised while encoding a lazy result. They also report decoding failures, missing static keys, worker startup failures and pool termination. An exception in a job does not stop its worker: the worker catches it and sends the error back to the page.

Cancellation rejects the promise with a `JSError` named `"Cancelled"` or `"Killed"`. If you used `run`, it throws a `WorkerError` with that message instead.

### Fallback

`available` reports whether Web Workers can be used. Without loader configuration, outside a browser, in GHCi or inside a worker, the library runs jobs on a Haskell thread in the current program. This fallback uses the same encoding and decoding as a real worker.

The fallback lets you use the API in these environments, but it does not move computation off the page's JavaScript thread. A long calculation can still block the UI between waits and checkpoints.

## Stop a job

Keep the promise returned by `start` if you want to stop the job later:

```haskell
p <- H.sync (liftIO (Worker.start pool primesSlowly 6000000))
-- Later, from another action:
H.sync (Worker.cancel pool p)   -- Stop the job at its next checkpoint.
H.sync (Worker.kill pool p)     -- Terminate the job's worker immediately.
```

`cancel pool promise` rejects the promise immediately and sends a cancellation request to the worker. If the job has not started, the worker skips it. If it is already running, its next call to `checkpoint` throws `Cancelled`. The worker can then process its next job.

Call `checkpoint` between chunks of work. A calculation occupies the worker's thread, so the worker cannot receive cancellation messages until the calculation yields to its event loop. `checkpoint` yields with `setTimeout(0)` and then checks for cancellation. Call it once per chunk, rather than once per element. A job that never calls it runs to completion even if you call `cancel`.

`kill pool promise` terminates the worker immediately, even if the job never calls `checkpoint`. The pool creates a replacement worker and resubmits that worker's other outstanding requests in their original order. To make this possible, the page keeps a copy of each request's bytes until it receives a reply. Killing a job costs more than cancelling it because the replacement worker must load and initialise the program again. The binary can come from the browser's cache, but `main` still has to run up to `serve`.

`run` automatically cancels its job if the wait is interrupted. This includes killing the component's fiber, losing a `race`, or reaching a `timeout`. Waiting on a promise returned by `start` does not have this behaviour: a promise can have several waiters, so you must cancel its job explicitly.

Both `cancel` and `kill` do nothing if the promise has already settled or was not created by that pool. In fallback mode, both stop the job's Haskell thread at its next wait or checkpoint.

`serve` is the lower-level entry point used by `program`. In a worker, it installs the message handler and returns `False`. On the page, it returns `True` to tell the caller to start the UI.

## Many tasks at once: `Halogen.Worker.Par`

Use `Par` when a computation needs to call many jobs. It collects independent calls and sends them to the pool in batches:

```haskell
{-# LANGUAGE ApplicativeDo #-}

import Halogen.Worker.Par (runPar, task)

-- Run a job for each list element and return results in the same order.
counts <- H.async (liftIO (runPar pool (traverse (uncurry (task primesBetween)) parts)))

-- Run a and b together, then use their results to run c.
(a, b, c) <- H.async $ liftIO $ runPar pool $ do
  a <- task primes 1000000
  b <- task primes 2000000
  c <- task primes (a + b)
  pure (a, b, c)
```

`task job arg1 arg2 ...` describes one job call. It takes the same arguments as `Worker.run`. Applicative composition (`<*>`) collects independent tasks so they can be submitted together. Monadic composition (`>>=`) waits for earlier results before creating dependent tasks. With `ApplicativeDo`, GHC can group independent statements in a `do` block automatically. Results are not cached: requesting the same task twice runs it twice.

`runPar pool computation` executes the computation in rounds. In each round, it collects the tasks that are ready to run, groups them by job, and splits each group into batches. It creates up to four batches per worker for each job, with fewer batches when there are fewer calls. Each batch is one request, and its calls run sequentially inside a worker.

Workers take batches from a shared queue. When a worker finishes a batch, it takes the next one, so workers with faster calls can process more batches. Once a round finishes, `runPar` uses its results to continue the computation and collect the next round of tasks.

If any call fails, the whole `runPar` fails with a `WorkerError`, and the remaining batches are cancelled. The error identifies the job and the failed call's index among that job's calls in the round, for example `its call at index 5: ...`. For a `traverse` over one job, this is the list index. Each worker stops its batch at the first failure. Interrupting the thread running `runPar` also cancels its outstanding batches.

When workers are unavailable, batches run sequentially in the current program. Each batch uses the pool's fallback mechanism, including encoding and decoding, and runs on its own Haskell thread. `terminate` stops these jobs too, and interrupting `runPar` cancels the current batch.

Batching is most useful when the calls in each batch add up to seconds of work, enough to justify the cost of sending the request.

## Data on the wire

The library encodes arguments and results with `Data.Binary`. A request contains the job's static key and the encoded arguments of each call. The arguments of one call are encoded one after another. The page sends it in an `ArrayBuffer` and transfers ownership of that buffer to the worker, avoiding a copy during `postMessage`. The worker sends the encoded results back the same way.

A reply contains the results up to the first failed call, followed by the failure if there was one. This lets `Par` send several calls in a single request.

Transferring the buffer does not remove all overhead. Values still need to be encoded and decoded, and bytes are copied between each Haskell heap and its `ArrayBuffer`. Workers are most useful for calculations that take seconds, where this cost is small compared with the work itself.

## Browser tests need pages

[Web Workers](https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API/Using_web_workers) have no DOM, focused element or browser input. They can test calculations, but they cannot run hspec-halogen component tests. Using `Halogen.Remote` to access a DOM would test a different backend and would still require a page for real browser behaviour.

Use hspec-halogen's isolated browser contexts to run UI tests in parallel. Use this package for the calculations those components call.

## Tests

`Halogen-worker-test` covers encoding, decoding, jobs with zero to ten arguments, type inference, static key lookup, errors, promises, cancellation, the `Par` planner, `runPar` and fallback execution. It runs natively and under Node on both browser backends.

The native test suite in `examples/worker/test` uses haskell-halogen-playwright to test real workers in Chromium. Build the example and run it with:

```sh
sh toolchain/build-wasm.sh worker
npm run test-worker
```

The browser suite checks results and UI responsiveness during calculations. It exercises parallel calls through `start` and `mapConcurrently`, as well as sixteen tasks submitted through `runPar`. It also covers cancellation at checkpoints, killing jobs without checkpoints, cancellation of a losing `race`, failures, pool termination, the custom script loader (`?workers=script`) and fallback execution without loader configuration.
