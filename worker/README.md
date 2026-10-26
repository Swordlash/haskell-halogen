# haskell-halogen-worker

Heavy work off the page's thread, in Web Workers, for a
[haskell-halogen](https://github.com/Swordlash/haskell-halogen) program, or any Haskell program
compiled for the browser.

A page has one thread for its JavaScript, its layout, its drawing and its Haskell, and a
computation that takes two seconds takes them from the page, whichever thread of the Haskell
runtime it runs on: GHC's WebAssembly has no threads that share memory, and the runtime's own
take turns on the page's. A Web Worker is another thread, with memory of its own. Here a worker
is a second instance of the program itself, loaded from the same `app.wasm`, which runs only the
functions the program offers it, named in a table, with arguments and results passed as JSON.

```haskell
import Halogen.Worker qualified as Worker

primes :: Worker.Job Int Int
primes = Worker.job "primes" (\n -> pure (countPrimes n))

main :: IO ()
main = Worker.program [Worker.some primes] $ runBrowserDOM $ do
  pool <- Worker.newPool 2
  app <- HA.awaitBody
  socket <- runUI (component pool) () app
  HA.keepForNextRun (socket.dispose >> Worker.terminate pool)
```

and in the component, a wait like any other, in `liftIO`:

```haskell
handleAction Count = do
  n <- liftIO (Worker.run pool primes 2000000)
  modify (\st -> st {primes = Just n})
```

## The API

- `job :: (FromJSON a, ToJSON b) => Text -> (a -> IO b) -> Job a b`: a function under a name.
  Several arguments are a tuple or a record. The result is evaluated completely as it is encoded,
  so a lazy error in it is the job's error.
- `some :: Job a b -> SomeJob`, for the table, a list of them.
- `serve :: [SomeJob] -> IO Bool`, what `main` does first: in a worker of a pool it answers the
  pool's messages with the table and returns `False`, and `main` does nothing else; anywhere else
  it returns `True`. `program jobs onPage` is `serve` and then `onPage` where it says to.
- `newPool :: Int -> m Pool`: that many workers (at least one), each started when it is first
  given a job. `terminate :: Pool -> m ()` ends them; what they were running fails, and the next
  job starts them again.
- `run :: (ToJSON a, FromJSON b) => Pool -> Job a b -> a -> m b`: the job in the pool's next
  worker (they take turns), waiting for its result. A job that throws, an argument or a result
  that does not decode, a name that is not in the table, a worker that did not start and a pool
  terminated meanwhile each throw a `WorkerError {job, message}`.
- `available :: m Bool`: whether `run` would use a worker here.

Killing the thread that waits in `run` (a component's fiber, say) stops the wait and not the
job: there is no cancelling a job in this version, and the worker finishes it, its result
unread. `terminate` ends a worker and everything in it.

## Telling the workers where the program is

The page's loader says where the program's files are, before it starts Haskell, in a global the
library reads. A WebAssembly program names its binary, its JSFFI glue and the WASI shim module
the page itself uses (a worker imports all three, as the page did):

```js
globalThis.halogenWorker = {
  wasm: new URL("./app.wasm", import.meta.url).href,
  jsffi: new URL("./ghc_wasm_jsffi.js", import.meta.url).href,
  wasi: new URL("./wasi.js", import.meta.url).href, // @bjorn3/browser_wasi_shim, as an ES module
};
```

A worker is a module worker made from a script the pool writes into a `Blob`: it imports the
shim and the glue, instantiates the binary with a WASI of its own (its stdout and stderr go to
the console, each line prefixed with `[worker n]`), and calls `hs_start`, the export every
example's binary has. The program's `main` then reaches `serve`, which finds itself in a worker
(the script told it its number) and answers messages: `{id, job, args}` in, `{id, result}` or
`{id, error}` out, the arguments and the result as JSON text, after a `{ready: true}` that lets
the pool send what it kept until then. A program that does not call `serve` is reported as a
worker that did not start.

A program built with the GHC JavaScript backend names its `all.js` instead,
`globalThis.halogenWorker = { js: new URL("./all.js", location.href).href }`, and gets classic
workers that `importScripts` it, which runs `main` as the page did.

Where no worker can be made, `run` runs the job in the program itself, on the calling thread,
through the same JSON and the same table, so that the same code works everywhere and fails the
same way: natively, under Node (the test suites), in browser GHCi (`npm run dev`), in a page
whose loader says nothing (the tutorial, whose programs run in GHC's interpreter and have no
binary of their own), in a browser without `Worker`, inside a worker, and before `serve` has been
called.

## What to know

- Each worker is another copy of the program, its memory too: a pool of four is four more heaps,
  and starting one takes about what the page took to load the program (the browser has the
  binary cached by then).
- Arguments and results cross as JSON text, so large ones cost their encoding, copying and
  decoding: a job is worth sending when it is much work on little data. (A later version may move
  bytes as transferables.)
- As far as the page is concerned a job is a pure function of its argument: it runs with a heap
  of its own, sees none of the page's state and has no DOM.
- On the page, a computation in `liftIO` without a worker leaves the component's loop free, but
  not the page's thread, which it holds until it is done (`examples/worker` measures it): only a
  worker gives the page its thread back.

## Tests

`Halogen-worker-test` runs natively and under Node on both browser backends, where there is no
worker: the JSON both ways, a job's exception, an error in its result, arguments and a result
that do not decode, a name that is not in the table, the job run in the program itself, and
`serve` returning `True` outside a worker. The workers themselves are tested through
`examples/worker` in headless Chromium (`npm run test-worker`, after
`sh toolchain/build-wasm.sh worker`): the counts in each worker and on the page, the frames the
page drew meanwhile, a failing job, `terminate`, and the page without a configuration.
