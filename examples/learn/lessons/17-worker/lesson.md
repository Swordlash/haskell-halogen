title: Work that must not freeze the page
layers: worker
args: -package haskell-halogen-worker

First, what this page cannot show you. The tutorial runs your code in GHC's interpreter, inside this page, not from an `app.wasm` of its own, so a program here cannot start a Web Worker of itself: under the button, the component says that no worker can be made, and every job runs on the page. What this lesson teaches is the code an application ships. The same program, compiled, runs its jobs in workers in the repository's `examples/worker` (`npm run serve-wasm -- worker`), which is where the numbers at the end come from.

Press "Count the primes below 50000". For a moment the spinner stops, and the frame count beside it, and the answer says how many frames the component drew while it counted: none. A page has one thread for its JavaScript, its layout and its drawing, and a Halogen program compiled to WebAssembly runs on that thread too. The Haskell runtime's threads take turns on it, and a computation that has it keeps it until it is done: meanwhile the page draws nothing and answers no click.

## A second copy of the program

A Web Worker is another thread, with memory of its own, and GHC's WebAssembly cannot share its heap with another thread. So a worker that `Halogen.Worker` (from the package `haskell-halogen-worker`) starts is a second instance of the program itself, loaded from the same `app.wasm`, which runs the functions the page asks it for.

How does the page say which function? Not by handing it over: a Haskell function is a closure, code together with whatever it captured, and what it captured lives in the page's heap, which the worker cannot see. What both copies have is the code. GHC's `static` (the `StaticPointers` extension) names a piece of it: `static e` is a `StaticPtr` whose key is the same in every copy of the program, and a worker finds `e` by that key in its own copy. That is all a job is:

```haskell
{-# LANGUAGE StaticPointers #-}

primes :: StaticPtr (Worker.Job Int Int)
primes = static (Worker.job (\n -> pure (countPrimes n)))
```

`Worker.job` wraps the function in the codecs of its argument and its result, which must have `Binary` instances (`Data.Binary`, a tuple or a record for several arguments), so the codecs travel inside the static value and the worker needs nothing else. `static` demands that what it is given is closed: it may name top-level functions such as `countPrimes`, never a local variable, since that would be something only the page's heap has. The compiler checks it, so a job that would need something the worker cannot have does not compile. The argument goes to the worker as bytes and the result comes back as bytes, in a buffer that is handed over rather than copied.

There is nothing to register, then, and `main` starts with `Worker.program`, as this lesson's `Main.hs` does:

```haskell
main = Worker.program $ runBrowserDOM $ do
  ...
  pool <- Worker.newPool 1
  socket <- runUI (component pool) () result
```

`Worker.program` looks at where it runs. In a worker it answers the page's requests and does nothing else; on the page it runs the rest, which here makes a pool of one worker and mounts the component with it. The pool starts its worker when it is first given a job, from the files the page's loader named in `globalThis.halogenWorker` before it started Haskell. The tutorial's page names none, and so here `Worker.available` is `False`, which is what the component shows.

A component runs a job with `Worker.run pool primes 50000`, which waits for the result. Waiting belongs in `liftIO`, which runs on a thread of its own while the component's loop goes on rendering and handling other actions; `H.liftEffect` is for what must happen at once, in the loop, and must not wait. Where no worker can be made, `Worker.run` calls the function itself, on a thread of its own, through the same bytes, so the same code runs everywhere, and fails the same way. (`Worker.start` gives a promise of the result instead of waiting for it, so that several jobs can run in several workers at once; `examples/worker` counts in both of its pool's workers that way.)

## Exercise

Make the count a job. Define `primes` as above, with `import GHC.StaticPtr (StaticPtr)`, export it beside `component` (a static pointer should be exported from its module, which the `Halogen.Worker` documentation explains), and in `Count` count with

```haskell
outcome <- liftIO (try (Worker.run pool primes 50000))
```

with `try` from `Control.Exception`. A job that throws, or whose argument or result does not decode, fails with a `Worker.WorkerError`, whose `job` says which (where its `static` is: the module, line and column) and `message` why: show the count for `Right n`, and for `Left (e :: Worker.WorkerError)` the message.

Compile and count. The answer comes from the job now, and the spinner no longer stands still, but it does not turn either: it jerks on by a frame now and then, a few times a second. Here the job runs on the page, on a thread `Worker.run` started for it, which leaves the component's loop free but not the page's thread. GHC's interpreter, which runs your code in this page, hands the page its thread back now and then while it counts, and in those moments the loop, being free, draws a frame; before, the loop was busy counting, and had nothing new to draw when the page got its moment. Then see what `static` will not take: in `Count`, after `before <- gets (.frame)`, count with a job written on the spot that names `before`,

```haskell
outcome <- liftIO (try (Worker.run pool (static (Worker.job (\() -> pure (countPrimes (50000 + before))))) ()))
```

and compile. The compiler refuses: `before` is a variable of the handler, bound as it runs, in the page's heap, and a worker could never have it. What a job needs it is given as its argument.

## Stopping a job

A job someone no longer wants should not go on holding its worker. `Worker.cancel pool promise` (the promise is the one `Worker.start` gave) rejects the promise at once and tells the worker, but a worker, like the page, has one thread, and a computation keeps it until it is done: the worker hears of the cancel only when the job calls `Worker.checkpoint`, which hands the worker's event loop a turn and throws `Worker.Cancelled` if the job was cancelled. A job that counts in chunks calls it once a chunk, not once a number, since each call costs a turn of the event loop. For a job that never checkpoints there is `Worker.kill`, which ends the worker it runs in and starts another in its place, which has to load the program again. And `Worker.run` cancels its job when its wait is interrupted, so a component that goes away, the losing side of `race`, and a `timeout` stop their jobs without anything more. In this page, with no worker, cancelling kills the thread the job runs on; in `examples/worker` the Cancel, Kill and race buttons show it in workers.

## What a worker changes

In `examples/worker`, whose page names its `app.wasm`, the same count of the primes below two million takes about two seconds every way. Counted in the component's loop with `H.liftEffect`, the page draws no frame until it is done. Counted as a job where no worker can be made, the loop is free but the page's thread is not, and compiled code hardly ever gives it back: the page draws one frame. Counted as a job in a worker, the page draws some 120 frames, one every sixtieth of a second, and the spinner turns throughout.

A worker costs something too: it takes the time the page took to load the program, the first time it is given a job, and it is another copy of the program's memory. Arguments and results are encoded and decoded on each side, so a job is worth sending when it is seconds of work, not milliseconds.
