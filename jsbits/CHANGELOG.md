# Changelog

## Unreleased

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
