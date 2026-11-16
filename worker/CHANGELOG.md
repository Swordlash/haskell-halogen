# Changelog for `haskell-halogen-worker`

## Unreleased

The first version: `Halogen.Worker` runs a program's functions in Web Workers, so that work of
seconds does not take those seconds from the page. A function is offered as a `Job`, made with
`job f`, which wraps it in the `Data.Binary` codecs of its arguments and result, under GHC's
`static` (the `StaticPointers` extension): `static (job f)` has a key that is the same in every
copy of the program, by which a worker finds the function in GHC's own static pointer table, so
there is no table of jobs to register and keep in step, and the compiler checks that what is sent
captures nothing a worker could not have. The program's `main` starts with `program` (or
`serve`), which in a worker answers the pool's requests and on the page goes on to the UI. A
`Pool` starts its workers, each a second instance of the program's own `app.wasm`, when it is
first given a job. The arguments and the result cross as bytes in an `ArrayBuffer` that is
transferred to the worker and back rather than copied. `start` gives a `Halogen.JSBits.Promise`
of the result at once, so that `mapConcurrently`, `concurrently` and `race` spread calls over the
pool's workers, and rejects with a `JSError` named `WorkerError`; `run` is `start` and the wait,
and throws a `WorkerError`, whose `job` is where the job's `static` is (its module, line and
column), for a job that throws, arguments or a result that do not decode, a key the worker's
program lacks, a worker that did not start and a pool terminated meanwhile. `terminate` ends the
workers, and `whichWorker` tells a job which worker runs it. The page's loader says where the
program is in `globalThis.halogenWorker`. Where no worker can be made (natively, in Node, in GHCi,
in a page that says nothing) the job runs in the program itself, through the same bytes, and
`available` says which it will be. On the GHC JavaScript backend a loader that names the program's
`all.js` gets classic workers that import it. A program that is not a binary such a script could
load (the tutorial's lessons, which run in GHC's interpreter) names a module script of its own in
`halogenWorker.script`, with `halogenWorker.data` for it to read: each worker imports it, and it
loads the program however it likes and runs its `main`.

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

`Halogen.Worker.Par` runs a computation whose independent parts go to the pool's workers at once:
a `task` is one call of a job, `Par`'s `<*>` runs both sides together and its `>>=` waits for
the left, so that with `ApplicativeDo` a `do` block sends what does not depend on what came before
in one round, and `traverse (task job) xs` maps a job over a list on every worker. `runPar` cuts
each round's calls of a job into batches, several for each worker, each one request, and gives
the next batch to whichever worker has answered its last, so that uneven calls do not leave a
worker idle. A failing call fails the whole computation with a `WorkerError` naming its job and
its index, and the batches still running are cancelled, as they are when `runPar`'s thread is
interrupted. Where no worker can be made the batches run here, as jobs of the pool's own, so that
`terminate` ends them. `size` gives a
pool's number of workers. A worker now answers a request with the results of its calls up to the
first that failed, and that call's failure, rather than one failure for the whole request.

Code written against the earlier, unreleased form of the package, in which a job was a function
under a name in a table given to `program`, with aeson JSON for its data, changes in three
places. Where jobs are defined, add `{-# LANGUAGE StaticPointers #-}`, give each the type
`StaticPtr (Job a b)` and the body `static (Worker.job f)` in place of `Worker.job "name" f`, and
export it from its module, since GHC before 10.0 ([issue
#24773](https://gitlab.haskell.org/ghc/ghc/-/issues/24773)) can fail to link a module whose static
binding is not exported and is used more than once. The types a job takes and gives derive `Binary` (`deriving anyclass
(Binary)` with `Generic`) in place of `FromJSON` and `ToJSON`. And `Worker.program` and
`Worker.serve` take no table: `SomeJob` and `some` are gone.

A job is a function of any number of arguments, curried: `Job '[Int, Text] Int` is the job of an
`Int -> Text -> IO Int`, and `run pool job 1 "a"`, `start pool job 1 "a"` and `task job 1 "a"`
take the arguments one by one after the job. The list of a job's arguments is a type-level list,
which needs `DataKinds`. Code written for jobs of one argument, `Job a b` of an `a -> IO b`,
changes its types: a `Job Int Int` is now a `Job '[Int] Int`, and a job that took `()` for want of
an argument can be a `Job '[] b` of an `IO b`, run with `run pool job`. A tuple or a record that
only packed several arguments into one can become arguments of their own: the job's function takes
them as a function does (`\lo hi -> ...` for `\(lo, hi) -> ...`), and a call that had them in a
pair, as in `mapConcurrently (start pool job) pairs`, uncurries it (`uncurry (start pool job)`).
`job` infers the job's type from its function when there is no signature, and a lambda that ends in
`pure` runs in `IO`. The bytes on the wire do not change: a call's arguments are encoded one after
another, which for one argument is what `encode` gave before.
