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

Export every binding that holds a static from its module. GHC before 10.0
([issue #24773](https://gitlab.haskell.org/ghc/ghc/-/issues/24773)) can fail to
link a module whose static binding is not exported and is used more than once:
with optimisation the binding stays internal to the module, and the module's
static pointer table refers to a symbol that is not there (an undefined
reference to the binding's closure).

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
  worker. A job stopped with `cancel` or `kill` is rejected with a `JSError`
  named `"Cancelled"` or `"Killed"` (and `run` throws a `WorkerError` with that
  message).
- `terminate pool` stops all workers, and the jobs the pool runs in the program
  itself, and fails outstanding requests. The next job starts workers again.
- `available` reports whether workers can be used. Without the loader
  configuration, outside a browser, in GHCi or inside a worker, `start` runs the
  job on a thread it forks, through the same encoding. This fallback keeps the
  API usable, but does not prevent a long calculation from blocking the page:
  the thread holds the page's thread between its waits and checkpoints.

## Stop a job

```haskell
p <- liftIO (Worker.start pool primesSlowly 6000000)
-- later, from another action:
Worker.cancel pool p   -- the job stops at its next checkpoint
Worker.kill pool p     -- the job's worker is ended, now
```

- `cancel pool promise` rejects the promise at once and tells the worker. A job
  the worker has not begun is never run. A running job learns of the cancel at
  its next `checkpoint`, which throws `Cancelled` there, and the worker is then
  free for its next job.
- `checkpoint` is what a job calls between pieces of its work. A worker has one
  thread, which a computation keeps until it waits, so a cancel the page sends
  arrives only when the job hands the worker's event loop a turn: `checkpoint`
  does that (a `setTimeout` of 0, a macrotask) and then checks. Call it once per
  chunk of work, not per element. A job that never calls it cannot be
  cancelled; it runs to the end, holding its worker.
- `kill pool promise` ends the worker that runs the job, at once, whatever the
  job is doing. The other requests that worker had are sent again, in order,
  to a new worker in its place, so only the killed job is lost; the page keeps a
  copy of every request's bytes until it is answered for this. The new worker
  loads the program again (the binary from the browser's cache, `main` run up
  to `serve`), which is what makes `kill` the dearer way to stop a job.
- `run` cancels its job when its wait is interrupted: a component's fiber that
  is killed, the losing side of `race` and a `timeout` stop the job instead of
  leaving it to finish unread. Waiting for `start`'s promise does not: a promise
  can have several waiters or none, so stopping its job is `cancel`'s business.
- A promise the pool did not start, or one that has settled, is left alone by
  both.
- Where no worker can be made, `cancel` and `kill` kill the job's thread, which
  stops at its next wait or checkpoint.

`serve` is the lower-level entry point used by `program`. It returns `False`
after installing a worker's message handler, or `True` when the caller should
start the UI.

## Many tasks at once: `Halogen.Worker.Par`

For a computation that calls jobs at many independent points, `Par` collects
the calls and sends them to the pool's workers in batches:

```haskell
{-# LANGUAGE ApplicativeDo #-}

import Halogen.Worker.Par (runPar, task)

-- a job mapped over a list, on every worker, the results in the list's order
counts <- liftIO (runPar pool (traverse (task primesBetween) parts))

-- independent tasks sent together, a dependent one after them
(a, b, c) <- liftIO $ runPar pool $ do
  a <- task primes 1000000
  b <- task primes 2000000
  c <- task primes (a + b)
  pure (a, b, c)
```

- `task job argument` is one call of the job. `Par`'s `<*>` runs both sides
  before anything is sent, so their tasks go out together; `>>=` waits for the
  left side's tasks before it runs the right. With `ApplicativeDo` a `do` block
  is taken apart where its statements do not depend on one another. There is
  no cache: the same task twice is two calls.
- `runPar pool computation` runs the computation until all of it waits for
  tasks, then sends that round's calls: grouped by job, each job's cut into
  batches (four for each worker, or fewer for fewer calls) that are each one
  request, handed out from a queue to whichever worker has answered its last
  batch. So a worker that drew the quick calls takes more batches while another
  is still at slow ones. Then it goes on with the next round.
- A failing call fails the whole `runPar` with a `WorkerError` naming the job,
  whose message gives the call's index among that job's calls in the round (for
  a `traverse`, the list's index): `its call at index 5: ...`. The batches still
  running are cancelled, as they are when `runPar`'s thread is interrupted.
  A worker stops a batch at its first failing call.
- Where no worker can be made, the batches run on the calling thread, one after
  the other, through the same codecs.

A batch is worth sending when its calls add up to seconds of work: the calls in
a batch run one after another in one worker.

## Data on the wire

The argument and the result are encoded with `Data.Binary`. The page sends the
job's static key and the argument's bytes in an `ArrayBuffer`, which is
transferred to the worker rather than copied, and the worker answers with the
result's bytes the same way. A request holds a list of arguments and a reply the
list of results, up to the first call that failed and that call's failure, which
is how `Par` sends a batch as one request. A job is worth sending when it is seconds of work:
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
failures, `start`'s promises, stopping jobs, `Par`'s planner and `runPar` and
the fallback natively and under Node on both browser backends. Real workers are
checked in Chromium through `examples/worker`:

```sh
sh toolchain/build-wasm.sh worker
npm run test-worker
```

That test checks results, UI responsiveness during a calculation, a range
counted in parts by both workers through `start` and `mapConcurrently` and in
sixteen tasks through `runPar`,
cancelling a job that checkpoints, killing one that does not, a race whose
loser is cancelled, failures, termination and the fallback without loader
configuration.
