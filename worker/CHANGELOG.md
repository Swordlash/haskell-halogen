# Changelog for `haskell-halogen-worker`

## Unreleased

The README now explains registration, concurrent calls, worker configuration,
cancellation and fallback with direct examples. It also describes the costs
of JSON and what a future binary transport would need. The protocol is unchanged.

The first version: `Halogen.Worker` runs a program's functions in Web Workers, so that work of
seconds does not take those seconds from the page. A function is offered as a `Job`, under a
name, with JSON for its argument and result; the program's `main` starts with `program jobs`
(or `serve jobs`), which in a worker answers the jobs' messages and on the page goes on to the
UI. A `Pool` starts its workers, each a second instance of the program's own `app.wasm`, when it
is first given a job; `run` waits for a result as any `liftIO` waits, and a job that throws, does
not decode or is not in the table fails with a `WorkerError`; `terminate` ends the workers. The
page's loader says where the program is in `globalThis.halogenWorker`. Where no worker can be made
(natively, in Node, in GHCi, in a page that says nothing) `run` runs the job in the program itself,
through the same JSON and the same table, and `available` says which it will be. On the GHC
JavaScript backend a loader that names the program's `all.js` gets classic workers that import it.
There is no cancelling a job yet: killing the waiting fiber abandons the result.
