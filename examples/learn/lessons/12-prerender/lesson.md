title: Prerendering and hydration

Prerendering puts component HTML in the page before its browser program starts. When the program loads, hydration adopts that HTML and attaches behaviour. The tutorial uses this approach: `examples/learn/Prerender.hs` generated the page you are reading.

## Rendering natively

`Halogen.Static.renderToHtml` mounts a component in the in-memory DOM using the normal driver. It waits for initialization to finish and returns HTML. For this lesson's starting component, a native program

```haskell
main = renderToHtml Lesson.component () >>= T.putStrLn
```

prints

```
<div><p>Count: <!---->0</p><button>+</button></div>
```

The tutorial's native prerender executable does this for every route and inserts the output into `<div id="app"></div>`. The browser entry point, `Main.hs`, is compiled to WebAssembly.

## Adopting the page

Use `hydrateUI` instead of `runUI` to adopt existing HTML. It compares the first render with the page in order, retains matching elements, attaches listeners and updates properties or text. Mismatched nodes are replaced.

Repairs are allowed, but cost work and can lose the user's selection. To adopt the page cleanly:

- Make the initial browser render match the prerendered HTML. Initial state must not depend on browser-only information, and rendering should depend on state alone.
- Keep separators between adjacent text nodes. The renderer inserts `<!---->` so the HTML parser does not merge `"Count: "` and the count.
- Remember that native prerendering waits for `H.onInitialize`, while browser initialization happens after adoption. Unconditional state changes during initialization can therefore make their initial HTML differ.

The lesson wrapper inserts the HTML above and gives each element a dashed outline before hydration. Retained nodes keep their outlines. Replace `HH.p_` with `HH.h2_` to see a newly created heading without one.

## Exercise

Disable the button until the browser program is ready. Add `State {count, ready}` with `ready = False`. Use `HP.disabled (not st.ready)` and display `"Loading..."` while it is false.

Add an `H.onInitialize` program that sets `ready` only `when inBrowser`, importing `inBrowser` from `Halogen.JSBits`. Without that guard, native prerendering would already enable the button. With it, the build produces `<button disabled>Loading...</button>`, and the browser enables it after adoption.
