# Changelog

## Unreleased

`Halogen.JSBits.Fields` binds the members of JavaScript objects as record
fields, with the JavaScript inline in each import, so a type needs no `.js`
file. `jsFields ''T [...]` writes a `HasField` instance for each member of
`T` (a newtype over a `JSVal`): a `Fixed` member is read purely
(`w.navigator`), a `Live` one in `IO` (`nav.onLine :: IO Bool`), a
`Writable` one also has a field named with `_set` after it
(`doc.title_set :: Text -> IO ()`), a `Method` holds the call
(`history.go :: Int -> IO ()`; one that returns a Promise gives it as a typed
`Promise a`). A JavaScript exception out of a `Live`,
`Writable` or `Method` member is thrown as a `JSError` too, so the caller
catches it as any `IO` exception (on WebAssembly an exception out of a
synchronous import would otherwise end the program). Strings, numbers, booleans, `Maybe` for `null` and
`undefined`, newtypes over a `JSVal`, a `ByteString` result and a
`Promise a` cross as the FFI passes them. Natively every field is `inert`,
or what `Natively` gives it. `jsFieldFunctions ''T [...]` writes the same
fields as plain functions (`go :: History -> Int -> IO ()`, `setTitle` for
`title_set`), for a module meant to be imported qualified; `jsCast` writes a
checked cast that asks `instanceof`; `inlineJS` declares one import of an
expression over `$1`, `$2`, ..., for constructors and static methods.

`Inert` has an instance for `ByteString` (empty).

**Breaking: `Promise` takes the type of what it resolves with**, `Promise a`,
and `await :: Promise a -> m (Either JSError a)` reads the resolved value as
that type through the new class `FromJS` (`JSVal`, `()`, `Text`,
`ByteString`, `Int`, `Double`, `Bool`, `Maybe a` for `null` and `undefined`,
`[a]` for an array;
a newtype over a `JSVal` derives it). `Promise JSVal` is the old behaviour.
`awaitOrThrow` throws a rejection instead of returning it.

**Breaking: `Safety` is this package's own**, with three constructors:
`Unsafe`, `Unchecked` and `Safe`. An `Unsafe` binding whose type ends in `IO`
now catches what its JavaScript throws and throws it in Haskell as a
`JSError`, which moved to `Halogen.JSBits.Value` (re-exported by
`Halogen.JSBits` and `Halogen.JSBits.Promise`). On WebAssembly an exception
out of a synchronous import cannot be caught in Haskell: it ends the program,
whatever handler the caller has. The catching costs a small object and one
more crossing per call, so `Unchecked` keeps the old behaviour, for a
function that cannot throw for the values it is given and is called very
often (core's DOM operations during a render). A binding of a pure type is
not guarded. `Safety` was Template Haskell's before, so a module that
imported it from `Language.Haskell.TH` imports it from `Halogen.JSBits` now.
`checkThrown` (in `Halogen.JSBits.Value`) reads the outcome the generated
code makes.

The `wasi` flag builds inert browser bindings and checked Haskell values on
WebAssembly, for standalone commands without JavaScript imports.

Natively a value carries what it was made from, so an event built from JSON
has properties to read. A native `JSVal` is a `Dynamic` (`newtype JSVal =
JSVal Dynamic`, with its constructor in a module compiled only natively,
`Halogen.JSBits.Native`): `jsNull` and every stub's result hold `()`, which
`isNull` asks for, `toJSInt`, `toJSDouble`, `toJSBool` and `toJSBytes` hold
what they are given, `jsValText` gives the `Text` a value holds and
`fromJSBytes` its `ByteString`, and a value that holds something else reads as
the stubs did (empty). A native `Callback` is a value holding its function
(`newtype Callback = Callback JSVal`, also in `Halogen.JSBits.Native`), as on
WebAssembly a callback is the JavaScript function, so that a backend can keep
something of its own in one, as haskell-halogen-core's in-memory DOM keeps its
listeners; `invokeCallback` calls a callback that holds a function and does
nothing with one that does not. The browser backends do not change.

`Halogen.JSBits.Value` has `inSyncCallback`, which says whether the calling
thread is running the body of a callback made with `mkSyncCallback`: one
JavaScript called, perhaps from the middle of a synchronous import the program
made, rather than a thread the program forked or an asynchronous callback's.
haskell-halogen-core asks it to nest an event the page dispatches in the work
that dispatched it, and only such an event. The thread is noted for as long as
the body runs, in a list kept for the purpose, which costs each call of a sync
callback two atomic updates.

`Halogen.JSBits.Promise` has `Promise`, a JavaScript promise kept to be
awaited later, so that starting asynchronous work and waiting for it are two
steps: a binding may now return `IO Promise` as an `Unsafe` binding, which
starts the work and returns at once, and `await` waits for it then (or never),
giving `Either JSError JSVal`, where `JSError` holds the rejection's `name`
and `message`. A rejection is a value on both browser backends alike, and
`await` forces what WebAssembly's awaited import returns itself, so callers
need no `evaluate`. `awaitAll` waits for several promises at once, keeping
their order. Natively `await` waits for nothing and gives
`JSError "NotInBrowserError" _`. Waiting can be stopped by killing the thread
that waits, but the work cannot: a binding over an API that takes an
`AbortSignal` should take one. `Promise` resolves with an untyped `JSVal` for
now, which the binding's wrapper decodes. The module is not re-exported from
`Halogen.JSBits`, since it declares its own bindings with `browserJS`.

A new test suite, `jsbits-promise-test`, runs on all three backends. On
WebAssembly it is a reactor whose test is an async export, `hs_test`, which
the repository's Node test runner now calls and waits for: a WASI command's
`main` cannot wait for a promise.

`Halogen.JSBits.Value` has `toJSBytes` and `fromJSBytes`, which copy a
`ByteString` into a `Uint8Array` of its own and back, in one call each rather
than a byte at a time: on WebAssembly through the instance's memory, on the
JavaScript backend through the array the bytes are kept in. Natively they
give the placeholder and no bytes. The package now depends on `bytestring`.

`Halogen.JSBits.Value` has `jsNull`, `toJSInt`, `toJSDouble` and `toJSBool`,
the values a binding passes where JavaScript takes any kind. On WebAssembly
`mkSyncCallback` is now a sync export that runs its callback on a thread of
its own until it waits, and arranges for the rest to run afterwards: what
haskell-halogen-core's DOM listeners did for themselves, so that a callback
runs on the spot even when re-entered from a synchronous JSFFI import.

`browserJS` needs no CPP around it: natively it declares each binding as an
`inert` stub. `Halogen.JSBits.Value` (re-exported from `Halogen.JSBits`) gives
the types that differ between the backends one name on all of them: `JSVal`,
`JSText` (`toJSText`, `fromJSText`, `jsValText`), `Callback` (`mkCallback`,
`mkSyncCallback`, `freeCallback`, `invokeCallback`), with `isNull`, `inBrowser`
and the `Inert` class. Packages using it now depend on it on every architecture.

`browserJS` takes one list of bindings for both browser backends: `wasmJS` on
WebAssembly, `foreign import`s by name on the JavaScript backend (an awaited one
`interruptible`, its rejection thrown as an `IOError`).

Embedded scripts are separated with explicit semicolons so expression boundaries
retain their meaning across files. Package metadata declares Apache-2.0, matching
the bundled license and the other Halogen packages.

A compile-time bridge embeds shared jsbits into WebAssembly FFI, keeping browser
logic in one source across backends without requiring applications to load extra
scripts. Sources are tracked for recompilation and initialization is retained
once per splice. WASM consumers must enable shared libraries for the compiler's
Template Haskell interpreter.
