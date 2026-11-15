title: A component that arrives later
layers: lazy
args: -package haskell-halogen-lazy -package haskell-halogen-lazy-demo

Large applications can avoid loading features that a visitor may never use, such as an editor or settings screen. `haskell-halogen-lazy` shows a placeholder while a component loads, then mounts it. This is similar to React's `lazy` and `Suspense`.

## The wrapper

`Lazy.lazy loading loadFeature` is a wrapper component. It shows `Lazy.placeholder`, runs `loadFeature`, then mounts the returned component. If loading fails, it shows `Lazy.failed` with the error.

The loader runs in the component's monad, here `BrowserDOM (Demo.Feature BrowserDOM)`, on a separate Haskell thread so the page can continue while it waits.

Once loaded, the child receives the wrapper's inputs, forwards its outputs and answers queries through the wrapper. Changing the name therefore updates its greeting. The parent's slot type remains `H.Slot Demo.Query Demo.Output ()`, just like a directly mounted child.

## Where the code comes from

`loadFeature` calls `liftIO (Dyld.load library)`. `Dyld.library` specifies `liblazy-demo.so` and its exported entry point, `Demo.entry`. The page fetches the compiled library, GHC's dynamic linker links it, and the entry point returns the component. The loader checks that its type matches the requested type.

Hide and show the feature. The library is linked only once, but its entry point runs again and a fresh component starts. Shared interface types, including `Demo.Query` and `Demo.Output`, live in `haskell-halogen-lazy-demo`, which both the lesson and library depend on.

Dynamic loading works here because the tutorial already runs GHC as a shared library under dyld. A normal standalone `app.wasm`, a JavaScript-backend program or a native program does not have that linker, so `Dyld.load` is unavailable. `Lazy.lazy` itself works everywhere with other loaders, including data-fetching actions or `pure` components.

A dynamically linked application must ship shared dependencies too. For comparison, the tutorial app is 2.5 MB as one binary, while core and its dependencies beyond GHC occupy 84.5 MB as shared libraries, or 7.2 MB compressed. `ghc-internal` adds 25.7 MB. This tradeoff is most useful for large applications with rarely used features. See `lazy/README.md` for setup.

## Exercise

1. Handle the feature's outputs. Replace `HH.slot_` with `HH.slot "feature" () (Lazy.lazy loading loadFeature) state.name Heard`. Add `Heard Demo.Output` and a `heard :: Text` field. When `Demo.Waved` arrives, display its wave count below the feature.
2. Add "Wave at it" and "Ask it" buttons. Use `H.tell "feature" () Demo.Wave` and `H.request "feature" () Demo.GetWaves`. Queries pass through the wrapper; requests made before loading finishes return `Nothing`.
3. Cache the loader with `Lazy.memo loadFeature` in `initialState`. Store it as `feature :: BrowserDOM (Demo.Feature BrowserDOM)` and pass `state.feature` to `Lazy.lazy`. Later mounts reuse the loaded component definition instead of calling the loader again. Failed loads are not cached and can be retried.
