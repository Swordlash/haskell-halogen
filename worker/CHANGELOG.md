# Changelog for `haskell-halogen-worker`

## Unreleased

The first version: `Halogen.Worker` runs a program's functions in Web Workers, so that work of
seconds does not take those seconds from the page. A function is offered as a `Job`, made with
`job f`, which wraps it in the `Data.Binary` codecs of its argument and result, under GHC's
`static` (the `StaticPointers` extension): `static (job f)` has a key that is the same in every
copy of the program, by which a worker finds the function in GHC's own static pointer table, so
there is no table of jobs to register and keep in step, and the compiler checks that what is sent
captures nothing a worker could not have. The program's `main` starts with `program` (or
`serve`), which in a worker answers the pool's requests and on the page goes on to the UI. A
`Pool` starts its workers, each a second instance of the program's own `app.wasm`, when it is
first given a job. The argument and the result cross as bytes in an `ArrayBuffer` that is
transferred to the worker and back rather than copied. `start` gives a `Halogen.JSBits.Promise`
of the result at once, so that `mapConcurrently`, `concurrently` and `race` spread calls over the
pool's workers, and rejects with a `JSError` named `WorkerError`; `run` is `start` and the wait,
and throws a `WorkerError`, whose `job` is where the job's `static` is (its module, line and
column), for a job that throws, an argument or result that does not decode, a key the worker's
program lacks, a worker that did not start and a pool terminated meanwhile. `terminate` ends the
workers, and `whichWorker` tells a job which worker runs it. The page's loader says where the
program is in `globalThis.halogenWorker`. Where no worker can be made (natively, in Node, in GHCi,
in a page that says nothing) the job runs in the program itself, through the same bytes, and
`available` says which it will be. On the GHC JavaScript backend a loader that names the program's
`all.js` gets classic workers that import it.

A job can be stopped. `cancel pool promise` rejects the promise at once (a `JSError` named
`Cancelled`) and tells the worker, where the job stops at its next `checkpoint`, a call a job
makes between pieces of its work that hands the worker's event loop a turn and throws `Cancelled`
there; a job not yet begun is never run. A job that does not checkpoint cannot be stopped that way,
and `kill pool promise` ends the worker that runs it instead, rejecting the promise (`Killed`) and
sending what else that worker had to a new one in its place, which loads the program again.
`run` cancels its job when its wait is interrupted, so a component's fiber that is killed, the
losing side of a `race` and a `timeout` stop the job rather than leave it to finish unread. Where
no worker can be made, `start` runs the job on a thread it forks, which `cancel`, `kill` and
`terminate` kill.

Code written against the earlier, unreleased form of the package, in which a job was a function
under a name in a table given to `program`, with aeson JSON for its data, changes in three
places. Where jobs are defined, add `{-# LANGUAGE StaticPointers #-}`, give each the type
`StaticPtr (Job a b)` and the body `static (Worker.job f)` in place of `Worker.job "name" f`, and
export it from its module, since GHC before 10.0 ([issue
#24773](https://gitlab.haskell.org/ghc/ghc/-/issues/24773)) can fail to link a module whose static
binding is not exported and is used more than once. The types a job takes and gives derive `Binary` (`deriving anyclass
(Binary)` with `Generic`) in place of `FromJSON` and `ToJSON`. And `Worker.program` and
`Worker.serve` take no table: `SomeJob` and `some` are gone.
