title: Work that must not freeze the page
layers: worker
args: -package haskell-halogen-worker

First, what this page cannot show you. The tutorial runs your code in GHC's interpreter, inside this page, not from an `app.wasm` of its own, so a program here cannot start a Web Worker of itself: under the button, the component says that no worker can be made, and every job runs on the page. What this lesson teaches is the code an application ships. The same program, compiled, runs its jobs in workers in the repository's `examples/worker` (`npm run serve-wasm -- worker`), which is where the numbers at the end come from.

Press "Count the primes below 50000". For a moment the spinner stops, and the frame count beside it, and the answer says how many frames the component drew while it counted: none. A page has one thread for its JavaScript, its layout and its drawing, and a Halogen program compiled to WebAssembly runs on that thread too. The Haskell runtime's threads take turns on it, and a computation that has it keeps it until it is done: meanwhile the page draws nothing and answers no click.

## A second copy of the program

A Web Worker is another thread, with memory of its own, and GHC's WebAssembly cannot share its heap with another thread. So a worker that `Halogen.Worker` (from the package `haskell-halogen-worker`) starts is a second instance of the program itself, loaded from the same `app.wasm`, which runs only the functions the program offers it. Each is a job, a function under a name, from an argument to a result that both have JSON instances:

```haskell
primes :: Worker.Job Int Int
primes = Worker.job "primes" (\n -> pure (countPrimes n))

jobs :: [Worker.SomeJob]
jobs = [Worker.some primes]
```

Only the name crosses to the worker, with the argument as JSON, and the result comes back as JSON: the worker finds the function in its own copy of the table. So a program's `main` starts with its table, as this lesson's `Main.hs` does:

```haskell
main = Worker.program jobs $ runBrowserDOM $ do
  ...
  pool <- Worker.newPool 1
  socket <- runUI (component pool) () result
```

`Worker.program` looks at where it runs. In a worker it answers the jobs' messages and does nothing else; on the page it runs the rest, which here makes a pool of one worker and mounts the component with it. The pool starts its worker when it is first given a job, from the files the page's loader named in `globalThis.halogenWorker` before it started Haskell. The tutorial's page names none, and so here `Worker.available` is `False`, which is what the component shows.

A component runs a job with `Worker.run pool primes 50000`, which waits for the result. Waiting belongs in `liftIO`, which runs on a thread of its own while the component's loop goes on rendering and handling other actions; `H.liftEffect` is for what must happen at once, in the loop, and must not wait. Where no worker can be made, `Worker.run` calls the function itself, through the same JSON and the same table, so the same code runs everywhere, and fails the same way.

## Exercise

Make the count a job. Define `primes` as above, put `Worker.some primes` in `jobs`, and in `Count` count with

```haskell
outcome <- liftIO (try (Worker.run pool primes 50000))
```

with `try` from `Control.Exception`. A job that throws, whose argument or result does not decode, or whose name is not in the table, fails with a `Worker.WorkerError`, whose `job` and `message` say which and why: show the count for `Right n`, and for `Left (e :: Worker.WorkerError)` the message.

Compile and count. The answer comes from the job now, and the spinner no longer stands still, but it does not turn either: it jerks on by a frame now and then, a few times a second. Here the job runs on the page, on the thread of the `liftIO`, which leaves the component's loop free but not the page's thread. GHC's interpreter, which runs your code in this page, hands the page its thread back now and then while it counts, and in those moments the loop, being free, draws a frame; before, the loop was busy counting, and had nothing new to draw when the page got its moment. Then take `Worker.some primes` out of `jobs` and compile again: the answer is what a program is told when its table lacks a job, here and in a worker alike.

## What a worker changes

In `examples/worker`, whose page names its `app.wasm`, the same count of the primes below two million takes about two seconds every way. Counted in the component's loop with `H.liftEffect`, the page draws no frame until it is done. Counted with `Worker.run` where no worker can be made, the loop is free but the page's thread is not, and compiled code hardly ever gives it back: the page draws one frame. Counted with `Worker.run` in a worker, the page draws some 120 frames, one every sixtieth of a second, and the spinner turns throughout.

A worker costs something too: it takes the time the page took to load the program, the first time it is given a job, and it is another copy of the program's memory. Arguments and results are copied as JSON text, so a job is worth sending when it is seconds of work on little data, not milliseconds of work on a lot.
