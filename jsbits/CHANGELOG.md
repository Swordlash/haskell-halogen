# Changelog

## Unreleased

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
