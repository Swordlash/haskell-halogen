title: Work that must not freeze the page
layers: worker
args: -package haskell-halogen-worker

Press "Count the primes below 50000". The spinner stops during the calculation, and the result reports no frames drawn while it ran.

Browser Haskell shares the page's JavaScript thread. Haskell threads can take turns, but a long calculation can still block rendering and input. A Web Worker moves that work to another browser thread with its own memory.

This tutorial runs interpreted code, so its workers start a second GHC instance and compile the same lesson. The first job after each Compile takes about twelve seconds to start, longer on slow machines. Later jobs reuse that worker. A compiled application loads another copy of its `app.wasm` instead. The API is the same; try the compiled example with `npm run serve-wasm -- worker`.

## A second copy of the program

`Halogen.Worker` starts workers that run another instance of your program. They cannot read the page's Haskell heap, so the page cannot send an arbitrary function closure with captured local values.

Instead, GHC's `StaticPointers` extension gives a static expression the same key in every copy of the program. The worker uses that key to find the job:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StaticPointers #-}

primes :: StaticPtr (Worker.Job '[Int] Int)
primes = static (Worker.job (\n -> pure (countPrimes n)))
```

`Worker.job` wraps a function returning its result in `IO`. The function can take any number of arguments, specified as a type-level list (needs `-XDataKinds`). An action with no arguments uses `'[]`. All the arguments and result must have `Binary` instances from `Data.Binary`; the job stores their codecs. Encoded bytes travel in transferred buffers.

`static` expressions may use top-level functions such as `countPrimes`, but cannot capture local variables. GHC checks this. Pass runtime data as the job's arguments.

No manual job registry is needed. Wrap `main` with `Worker.program`, as the lesson wrapper does:

```haskell
main = Worker.program $ runBrowserDOM $ do
  ...
  pool <- Worker.newPool 1
  socket <- runUI (component pool) () result
```

On the page, `Worker.program` runs the UI setup. Inside a worker, it serves job requests instead. The pool starts each worker on its first job using `globalThis.halogenWorker` configuration. Here the tutorial configures a custom compiler-loading script, so `Worker.available` reports `True`.

`Worker.run pool primes 50000` submits the job and waits for its result. The job's arguments follow it in order, one by one. It waits, so call it in `H.async`: the component loop goes on handling actions meanwhile. `H.sync` must not wait.

When workers are unavailable, the job runs on a Haskell thread in the current program using the same encoding. That fallback does not move computation off the browser thread. For concurrent jobs, `Worker.start` returns a promise without waiting; the compiled example uses it across several workers.

## Exercise

Define `primes` as above, import `GHC.StaticPtr (StaticPtr)` and export `primes` alongside `component`. Exporting static bindings avoids a GHC linker issue described in the worker documentation.

Replace the calculation in `Count` with:

```haskell
outcome <- H.async (liftIO (try (Worker.run pool primes 50000)))
```

Import `try` from `Control.Exception`. Display the count for `Right n` and the error message for `Left (e :: Worker.WorkerError)`. Worker errors include the job's source location and failure message, including exceptions and decoding failures.

Compile and count. The spinner should keep moving during worker startup and computation. Count again to reuse the worker: this takes roughly the calculation time, about a second. Without Web Workers, fallback execution can still make the spinner stall.

Now try capturing a local value. After `before <- gets (.frame)` in `Count`, use:

```haskell
outcome <- H.async (liftIO (try (Worker.run pool (static (Worker.job (pure (countPrimes (50000 + before))))))))
```

Compilation fails because `before` is local to the handler. The worker cannot access it. Pass any such value as an argument instead.

## Stopping a job

`Worker.cancel pool promise` rejects the promise returned by `Worker.start` immediately and requests cancellation. A running job must call `Worker.checkpoint` to yield to the worker's event loop and receive that request. The checkpoint throws `Worker.Cancelled` if cancellation was requested. Call it between chunks, not for every element, because each checkpoint takes an event-loop turn.

`Worker.kill` can stop a job without checkpoints by terminating its worker. A replacement worker must load the program again, which here means another compiler startup.

`Worker.run` cancels its job if its wait is interrupted, including component disposal, a losing `race` or a timeout. The Cancel, Kill and race buttons in `examples/worker` demonstrate these behaviours.

## What a worker changes

In the compiled example, counting primes below two million takes about two seconds in each mode. Running in `H.sync` draws no frames during that time. Fallback execution frees the component loop but still occupies the browser thread, drawing about one frame. A real worker allows roughly 120 frames, keeping the spinner responsive.

Workers also cost startup time and memory. Here each one adds about half a gigabyte for the compiler and lesson until the next compile or lesson change disposes it. Arguments and results require encoding and decoding, so workers are most useful for work lasting seconds rather than milliseconds.

## Many tasks at once

Use `Halogen.Worker.Par` when a calculation contains many independent job calls. Each call is a `task`:

```haskell
{-# LANGUAGE ApplicativeDo #-}

import Halogen.Worker.Par (runPar, task)

counts <- H.async (liftIO (runPar pool (traverse (uncurry (task primesBetween)) parts)))

both <- H.async $ liftIO $ runPar pool $ do
  a <- task primes 100000
  b <- task primes 200000
  c <- task primes (a + b)
  pure (a, b, c)
```

`task` takes the same arguments as `Worker.run`; `primesBetween` here takes two - start and end of a part. `traverse` creates one task per part. `runPar` groups calls by job and splits them into batches. Workers take the next batch when ready, so faster workers can process more batches. Results preserve input order.

With `ApplicativeDo`, independent statements run together: `a` and `b` can start at once, while `c` waits for both. A failed call ends `runPar` with a `Worker.WorkerError` identifying the job and call index, and cancels remaining batches.

The same code works in the tutorial's worker. With a one-worker pool, batches run there sequentially. The compiled example demonstrates sixteen tasks across its pool.
