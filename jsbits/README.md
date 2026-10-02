# Shared JavaScript FFI

`Halogen.JSBits.wasmJS` embeds the same ordinary JavaScript files that the GHC
JavaScript backend links through `js-sources`. Keep browser logic in `jsbits/`;
Haskell declarations specify only the function name, safety and typed signature.
Backend-specific value conversions and callback adapters remain in Haskell.

```haskell
$(wasmJS ["jsbits/sound.js"]
  [("js_pause", "halogen_sound_pause", Unsafe, [t| JSVal -> IO () |])])
```

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
