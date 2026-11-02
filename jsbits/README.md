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
- `toJSBytes` and `fromJSBytes`, a `ByteString` copied into a `Uint8Array`
  and back, in one call rather than a byte at a time.
- `isNull`, and `inBrowser` for the rare wrapper that must do something else
  natively than nothing.

A binding's result type needs an `Inert` instance: there is one for `()`,
`Bool`, `Int`, `Double`, `Text`, `Maybe`, `JSVal`, functions and `IO`; a
newtype over a `JSVal` derives it (`deriving newtype (Inert)`).

A binding that starts asynchronous work returns `IO (Promise a)`
(`Halogen.JSBits.Promise`): the JavaScript starts the work and returns its
promise at once, and the generated code listens to it straight away, so a
rejection is never left unhandled. `Promise` is to a JavaScript promise what
`async`'s `Async` is to a thread, with the same vocabulary: `wait` (throws a
rejection, a `JSError`), `waitCatch` (gives `Either JSError a`), `poll`, their
`STM` forms, `waitAny`, `waitEither`, `waitBoth`, `waitAll`, and
`mapConcurrently`, `concurrently` and `race`, which start several pieces of
work and wait for them together. What it resolves with is read as `a`
through `FromJS`. A component can start the work in `H.liftEffect`, inside
the dispatch of the event that asked for it, and wait for it later in `lift`,
or never. A promise cannot be cancelled: killing the thread that waits stops
the wait, not the work, so a binding for an API that takes an `AbortSignal`
should take one and hand it on. Natively every promise has been rejected
with `JSError "NotInBrowserError" _`.

```haskell
$(browserJS ["jsbits/net.js"]
  [ ("js_fetch_text", "halogen_net_fetch_text", Unsafe, [t| JSText -> IO Promise |]) ])
```

A plain property read or method call needs no `.js` file at all:
`Halogen.JSBits.Fields.jsFields` writes a `HasField` instance for each member
of a newtype over a `JSVal`, with the JavaScript inline in its import, so
that `OverloadedRecordDot` reads it, and `jsFieldFunctions` writes the same
members as plain functions, for a module imported qualified.

```haskell
$(jsFields ''Navigator [Live "onLine" [t| IO Bool |], Fixed "clipboard" [t| Maybe Clipboard |]])
$(jsFields ''HTMLDocument [Writable "title" [t| IO Text |]])
$(jsFields ''History [Method "go" [t| Int -> IO () |]])
$(jsFields ''Blob [Method "text" [t| IO (Promise Text) |]])
```

`Fixed` is read purely (`w.navigator`, so `w.navigator.onLine` chains),
`Live` in `IO` each time, `Writable` also has a field `title_set :: Text ->
IO ()` (no Web API name has `_set` in it), and `Method` holds the call; a
method that returns a Promise gives it typed (`Promise Text`), for the caller
to `await`. What a member's JavaScript throws comes back as a `JSError`.
Natively every field is `inert`, unless `Natively` gives it
an answer of its own. `jsCast` writes a checked cast (`instanceof`) giving
`Maybe`, and `inlineJS` one import of an expression, for a constructor or a
static method. haskell-halogen-core's `Web.*` modules are written with them.

Every binding is synchronous. One whose function starts asynchronous work
returns its JavaScript promise as `IO (Promise a)` (`Halogen.JSBits.Promise`):
the generated code listens to the promise at once, and the program `await`s
it where it wants the value. A `Safe` binding's JavaScript exception is
thrown as a `JSError`; an `Unsafe` one's is not caught. The package
depends on `haskell-halogen-jsbits` on every architecture.

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
