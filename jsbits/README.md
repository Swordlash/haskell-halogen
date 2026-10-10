# Shared JavaScript FFI

`Halogen.JSBits.browserJS` declares a package's JavaScript functions once for
every backend, with no CPP in the module that splices it. On WebAssembly it is
`wasmJS`, which embeds the files; on the JavaScript backend, which links the
same files through `js-sources`, each binding is a `foreign import javascript`
of the function by name; natively, with no engine to call, each binding is
`inert`: an `IO` one does nothing and returns `False`, `0`, `""`, `Nothing`,
`()` or a placeholder handle. Keep browser logic in `jsbits/`; Haskell
declarations specify only the function name, safety and typed signature.

```haskell
$(browserJS ["jsbits/sound.js"]
  [ ("js_fetch", "halogen_sound_fetch", Unsafe, [t| JSText -> Callback -> IO () |])
  , ("js_pause", "halogen_sound_pause", Unsafe, [t| JSVal -> IO () |]) ])
```

The types that differ between the backends are this package's, the same name
on all of them (`Halogen.JSBits.Value`, re-exported here):

- `JSVal`: the engine's value; natively an opaque placeholder.
- `JSText`, a string as a binding passes it (a `JSVal` holding one in a
  browser, `Text` natively), with `toJSText` and `fromJSText`;
  `jsValText` reads a `JSVal` known to hold a string.
- `Callback`, a Haskell function JavaScript calls with one `JSVal`:
  `mkCallback`, `mkSyncCallback` (runs at once, so a handler can still prevent
  an event's default), `freeCallback`, `invokeCallback`.
- `isNull`, and `inBrowser` for the rare wrapper that must do something else
  natively than nothing.

A binding's result type needs an `Inert` instance: there is one for `()`,
`Bool`, `Int`, `Double`, `Text`, `Maybe`, `JSVal`, functions and `IO`; a
newtype over a `JSVal` derives it (`deriving newtype (Inert)`).

A `Safe` binding awaits the function's Promise. On WebAssembly that is an async
import; on the JavaScript backend an `interruptible` one, whose rejection is
thrown as an `IOError`. There it must return `IO ()`, and the module splicing it
needs `InterruptibleFFI`. The package depends on `haskell-halogen-jsbits` on
every architecture.

The splice reads sources relative to the package, records them with
`addDependentFile`, and emits an initialization import plus small typed wrappers.
The source is evaluated once per module through a `NOINLINE` unit CAF; the API
is stored under a splice-specific Symbol rather than named global functions.
Applications do not copy assets, load scripts, use `eval`, or install named
globals. Node, browser builds and browser GHCi use the same code.

Sources must be ordinary JavaScript, without CPP directives or static ES module
imports. Keep embedded sources ASCII (use JavaScript `\u` escapes for Unicode
strings), because this toolchain mis-escapes non-ASCII text in inline assembly.
Libraries that already bundle external modules, such as Material,
continue to load their shared jsbits through their existing bundler.

The WASM compiler's Template Haskell interpreter needs dynamic dependencies:
set `shared: True` in the consumer's WASM Cabal project (or `--enable-shared`).
This does not change the final application's WASM linking model. Include `jsbits`
in `source-repository-package` subdirectories when consuming this repository.
Include each source in `extra-source-files` for source distributions.

The unit CAF is deliberate: a lazily cached `JSVal` can reach the WASM FFI as an
updated thunk rather than a direct handle. Generated wrappers force initialization
and then call the Symbol-keyed API without passing such a handle.
