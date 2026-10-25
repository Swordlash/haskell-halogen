title: Prerendering and hydration

A Halogen page is empty until its program has started. A prerendered page is not: the build renders the component to HTML ahead of time, and the program, once it runs, takes that HTML over instead of drawing its own. This tutorial is prerendered: `Prerender.hs`, in `examples/learn`, wrote this page.

## Rendering natively

`Halogen.Static.renderToHtml` mounts a component, with the ordinary driver, into an in-memory document (`MemDOM`, which exists only natively), lets its `Initialize` run to the end, and returns the HTML. For this lesson's `Start.hs`, a native program

```haskell
main = renderToHtml Lesson.component () >>= T.putStrLn
```

prints

```
<div><p>Count: <!---->0</p><button>+</button></div>
```

`Prerender.hs` does that for every route, and puts the HTML into the template's empty `<div id="app"></div>`. The build runs it as `halogen-example-learn-prerender`, compiled by the native GHC; `Main.hs` is compiled to WebAssembly.

## Adopting the page

In the browser, `Main.hs` calls `hydrateUI` where it would call `runUI`. The component's first render, from `H.initialState`, is matched with the page's HTML in order: an element with the same name is kept and gets its listeners and properties, a text node is kept and its text corrected. A node that does not match is built afresh in place of the page's. That is never an error, but it is work, and loses what the reader had selected there. Hence the rules:

- The first render must be the page: `initialState` reads nothing the build did not have, and the render depends on the state alone.
- Two text nodes side by side, `"Count: "` and the count, are written with `<!---->` between them, which the parser would merge otherwise.
- `Initialize` runs after the adoption, and its renders patch the adopted nodes. But `renderToHtml` ran it too, before taking the HTML, so a state it sets is in the page and not in the client's first render.

The program around your module does what `Main.hs` does: it puts the HTML above into the page, with a dashed outline on each element, and hydrates your component over it. The nodes it adopts keep their outlines: write `HH.h2_` for the `HH.p_`, and the heading, built afresh, has none.

## Exercise

The button does nothing until the program runs, so the page should not offer it. Make the state `State {count, ready}`, with `ready = False`; render `HP.disabled (not st.ready)`, and `"Loading..."` until ready; add an `Initialize` that sets `ready`, guarded by `when inBrowser` (from `Halogen.JSBits`), as the tutorial's own `Initialize` is. Without the guard the build would write a ready button. With it, the build writes `<button disabled>Loading...</button>` and only the browser makes it ready.
