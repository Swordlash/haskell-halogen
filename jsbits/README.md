# Shared JavaScript FFI

`Halogen.JSBits.browserJS` declares a package's JavaScript functions once for
both browser backends. On WebAssembly it is `wasmJS`, which embeds the files; on
the JavaScript backend, which links the same files through `js-sources`, each
binding is a `foreign import javascript` of the function by name. Keep browser
logic in `jsbits/`; Haskell declarations specify only the function name, safety
and typed signature. Backend-specific value conversions and callback adapters
remain in Haskell: where a type differs between the backends (a string is a
`JSString` on one, a `JSVal` on the other), name it with a type synonym defined
per backend before the splice.

```haskell
#if defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH)
$(browserJS ["jsbits/sound.js"]
  [("js_pause", "halogen_sound_pause", Unsafe, [t| JSVal -> IO () |])])
#endif
```

A `Safe` binding awaits the function's Promise. On WebAssembly that is an async
import; on the JavaScript backend an `interruptible` one, whose rejection is
thrown as an `IOError`. There it must return `IO ()`, and the module splicing it
needs `InterruptibleFFI`. The package depends on `haskell-halogen-jsbits` for
both architectures.

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
