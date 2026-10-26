title: A component that arrives later
layers: lazy
args: -package haskell-halogen-lazy -package haskell-halogen-lazy-demo

Every component so far was part of the program from the start: compiled with it, and downloaded with it before anything showed. A large application may not want that for all of it. An editor few readers open, a settings page, a part with a large library of its own: each is code that every visit downloads, whether it is used or not. `haskell-halogen-lazy` lets a tree show a component it does not have yet. The component arrives later, when it is first shown, and the tree shows a placeholder until then, as React's `lazy` and `Suspense` do.

## The wrapper

`Lazy.lazy loading loadFeature` is a component that stands for the one it waits for. When it is mounted, it shows `Lazy.placeholder`, runs the loader `loadFeature`, and then shows the component the loader gave in its place; if the loader fails, it shows `Lazy.failed` with why. The loader is a program of the component's monad, here `BrowserDOM (Demo.Feature BrowserDOM)`, and it runs on a thread of its own, as `liftIO` does: the page goes on while it waits.

Once it is there, the feature is an ordinary child of the wrapper. It has the wrapper's input: type another name and it greets that one, since the wrapper hands every new input on. Its output is the wrapper's output, and the queries sent to the wrapper are sent on to it. So in the parent's `Slots` it is `H.Slot Demo.Query Demo.Output ()`, as a child that was there all along would be.

## Where the code comes from

`loadFeature` is `liftIO (Dyld.load library)`. `Dyld.library` names a shared library, `liblazy-demo.so`, and the function it exports, `Demo.entry`. `Dyld.load` has the page fetch the library, a file of compiled Haskell, and GHC's dynamic linker link it into the running program; then it calls the entry, which hands over the component with its type, and checks the type is the one asked for. Click Hide, then Show: the second time the library is not fetched again, since a library is linked once, but the entry is called again and the feature starts over.

The program and the library must agree on the component's type, every type in it: a type only the library defines is one the program cannot name. So `Demo.Query` and `Demo.Output` are in `haskell-halogen-lazy-demo`, a package both this lesson and the library are built against.

This works here because the tutorial's page already runs on GHC's dynamic linker: the compiler is a shared library itself, and your program runs inside it. An application of the usual kind is linked into one `app.wasm`, with no linker left in it to link more, and there `Dyld.load` fails, saying so; so it does on the JavaScript backend and natively. `Lazy.lazy` itself works on every backend, with any loader: a program that fetches what the component needs before it can show anything, or `pure` of a component the program has. An application built to load its parts this way ships every library it links against as a shared library too, rather than one file trimmed to what it uses: this tutorial's app is 2.5 MB as one file, while under your program here `haskell-halogen-core` and the libraries it needs beyond GHC's own are 84.5 MB of shared libraries, 7.2 MB compressed, and GHC's `ghc-internal` is another 25.7 MB. That pays for an application large enough that most of it is rarely needed; `lazy/README.md` in the repository says what it takes.

## Exercise

1. Hear the feature. It raises `Demo.Waved` with how many times it has waved, which `HH.slot_` drops. Use `HH.slot "feature" () (Lazy.lazy loading loadFeature) state.name Heard` instead, with an action `Heard Demo.Output` that puts a sentence into a field `heard :: Text`, and show it under the feature. Press the feature's Wave button.
2. Talk to it. Add a button "Wave at it", whose action sends `H.tell "feature" () Demo.Wave`, and a button "Ask it", whose action asks with `H.request "feature" () Demo.GetWaves`. Both go through the wrapper. A query that arrives before the feature is there is answered with nothing, as one sent to an empty slot is: `H.request` gives `Nothing` then.
3. Load it once. Each time the feature is shown, a new wrapper runs the loader again. Make the loader in `initialState` with `Lazy.memo loadFeature`, keep it in a field `feature :: BrowserDOM (Demo.Feature BrowserDOM)`, and give `state.feature` to `Lazy.lazy`: the first wrapper loads, and every later one gets what that load gave, without loading again. A load that fails is not kept, so the next wrapper tries again.
