# Shared JavaScript FFI

Declare JavaScript bindings once with `Halogen.JSBits.browserJS`. It generates the appropriate implementation for each backend, without CPP in the calling module. WebAssembly embeds the source through `wasmJS`; the JavaScript backend links the same files with `js-sources` and imports functions by name.

Native bindings are inert: IO operations do nothing and return defaults such as `False`, `0`, `""`, `Nothing`, `()` or placeholder handles. Keep browser logic in `jsbits/` files. Haskell declarations specify the function name, safety and type.

```haskell
$(browserJS ["jsbits/sound.js"]
  [ ("js_fetch", "halogen_sound_fetch", Unsafe, [t| JSText -> Callback -> IO () |])
  , ("js_pause", "halogen_sound_pause", Unsafe, [t| JSVal -> IO () |]) ])
```

Use the types from `Halogen.JSBits.Value`, re-exported by this module, to keep binding signatures consistent across backends:

- `JSVal`: the engine's value; natively an opaque placeholder.
- `JSText`, a string as a binding passes it (a `JSVal` holding one in a browser, `Text` natively), with `toJSText` and `fromJSText`; `jsValText` reads a `JSVal` known to hold a string.
- `Callback`, a Haskell function JavaScript calls with one `JSVal`: `mkCallback`, `mkSyncCallback` (runs at once, so a handler can still prevent an event's default), `freeCallback`, `invokeCallback`.
- `toJSBytes` and `fromJSBytes`, a `ByteString` copied into a `Uint8Array` and back, in one call rather than a byte at a time.
- `isNull`, and `inBrowser` for the rare wrapper that must do something else natively than nothing.

A binding's result type needs an `Inert` instance: there is one for `()`, `Bool`, `Int`, `Double`, `Text`, `Maybe`, `JSVal`, functions and `IO`; a newtype over a `JSVal` derives it (`deriving newtype (Inert)`).

An asynchronous browser operation returns `IO (Promise a)` from `Halogen.JSBits.Promise`. JavaScript starts the work and returns a promise immediately. Generated bindings attach handlers immediately too, so rejections are never left unhandled.

`Promise` provides an API similar to `async`'s `Async`: `wait`, `waitCatch`, `poll`, STM variants, `waitAny`, `waitEither`, `waitBoth`, `waitAll`, `mapConcurrently`, `concurrently` and `race`. `wait` throws a `JSError` on rejection; `waitCatch` returns `Either JSError a`. Successful results are decoded with `FromJS`.

A component can start an operation during event dispatch with `H.sync` and wait later in `H.async`. Interrupting a waiter does not cancel the operation. If the browser API supports `AbortSignal`, expose it in your binding so callers can cancel explicitly. On native GHC, promises are rejected with `JSError "NotInBrowserError" _`.

```haskell
$(browserJS ["jsbits/net.js"]
  [ ("js_fetch_text", "halogen_net_fetch_text", Unsafe, [t| JSText -> IO Promise |]) ])
```

For simple property reads and method calls, use `Halogen.JSBits.Fields` without creating a `.js` file. `jsFields` generates `HasField` instances for a newtype over `JSVal`, allowing `OverloadedRecordDot`. `jsFieldFunctions` generates ordinary functions suitable for qualified imports. Both embed the JavaScript directly in the binding:

```haskell
$( declarations
     [ jsFields ''Navigator [Live "onLine" ''Bool, Fixed "clipboard" [t| Maybe Clipboard |]]
     , jsFields ''HTMLDocument [Writable "title" ''Text]
     , jsFields ''History [Method "go" [t| Int -> IO () |]]
     , jsFields ''Blob [Method "text" [t| IO (Promise Text) |]]
     ]
 )
```

`declarations` runs several generators as one splice. Give each module one splice. On the JavaScript backend the compiler keeps about 110 MiB for each top-level splice until the package is compiled. The splice goes after the types it names and before the code that uses what it writes.

`Fixed` provides a pure field read, so accesses can chain as in `w.navigator.onLine`. `Live` reads the value in `IO` each time. `Writable` adds a setter such as `title_set :: Text -> IO ()`; `_set` does not conflict with Web API names. `Method` wraps a call, including typed promise results such as `Promise Text`. JavaScript exceptions become `JSError`s.

Native fields are inert unless `Natively` supplies an implementation. `jsCast` generates a checked `instanceof` cast returning `Maybe`, and `inlineJS` imports a single expression, such as a constructor or static method. Core's `Web.*` modules use these helpers.

All FFI calls are synchronous. Asynchronous operations return a `Promise` that callers can await later. `Safe` bindings catch JavaScript exceptions and convert them to `JSError`; `Unsafe` bindings do not catch them. Packages using these bindings depend on `haskell-halogen-jsbits` on every architecture.

The splice reads sources relative to the package, records them with `addDependentFile`, and emits an initialization import plus small typed wrappers. The source is evaluated once per module through a `NOINLINE` unit CAF; the API is stored under a splice-specific Symbol rather than named global functions. Applications do not copy assets, load scripts, use `eval`, or install named globals. Node, browser builds and browser GHCi use the same code.

Sources must be ordinary JavaScript, without CPP directives or static ES module imports. Keep embedded sources ASCII (use JavaScript `\u` escapes for Unicode strings), because this toolchain mis-escapes non-ASCII text in inline assembly. Libraries that already bundle external modules, such as Material, continue to load their shared jsbits through their existing bundler.

The WASM compiler's Template Haskell interpreter needs dynamic dependencies: set `shared: True` in the consumer's WASM Cabal project (or `--enable-shared`). This does not change the final application's WASM linking model. Include `jsbits` in `source-repository-package` subdirectories when consuming this repository. Include each source in `extra-source-files` for source distributions.

The unit CAF is deliberate: a lazily cached `JSVal` can reach the WASM FFI as an updated thunk rather than a direct handle. Generated wrappers force initialization and then call the Symbol-keyed API without passing such a handle.
