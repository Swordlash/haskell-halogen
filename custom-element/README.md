# haskell-halogen-custom-element

A [haskell-halogen](https://github.com/Swordlash/haskell-halogen) component as a custom element
(a Web Component), for any page to use as it uses a built-in element: from plain HTML, React,
Vue, Svelte, Astro or HTMX, none of which needs to know that Halogen is behind it. And the other
way, what a Halogen component needs to use custom elements made elsewhere.

```haskell
import Halogen.CustomElement (Definition (..), Registration (..))
import Halogen.CustomElement qualified as CE

main :: IO ()
main = runBrowserDOM $ do
  HA.endPreviousRun
  registration <-
    CE.define "x-counter"
      CE.defaultDefinition
        { input = \props -> fromMaybe 0 (CE.prop "start" props <|> (CE.attr "start" props >>= readMaybe . T.unpack))
        , observed = ["start"]
        , output = \n -> Just ("count", toJSON n)
        , methods = [CE.tellMethod "reset" (\() -> Reset), CE.method "value" (\() -> GetValue)]
        , styles = "button { font: inherit; }"
        }
      counter -- :: H.Component Query Int Int m
  HA.keepForNextRun registration.dispose
```

`define` takes the element's name, which must contain a hyphen, its `Definition`, and the
component. From then on every element of the name mounts a tree of its own with the component
in it, whether the element was in the page's HTML, was inserted by a script or rendered by a
framework:

- **`input`** makes the component's input from the element's `Props`: `CE.attr` reads an
  attribute, as text; `CE.prop` a property set from JavaScript, decoded from JSON.
- **`observed`** names what `input` reads. Each name is watched as an attribute and is a
  property of the element too (unless `HTMLElement` has it already, as `title` or `hidden`).
  When one changes, `input` is asked again and the component hears the new input through
  `H.onReceive`, exactly as under a Halogen parent that rendered it with a new input: the
  element mounts a small parent around the component that does just that.
- **`output`** turns what the component raises into a `CustomEvent`, with a name and a JSON
  `detail`. It bubbles and is `composed`, so it leaves the shadow root and can be listened for
  anywhere above the element.
- **`methods`** are the element's methods, each a query: `CE.method` a request, whose promise
  resolves with the answer as JSON (`null` when the component does not answer), and
  `CE.tellMethod` a tell, whose promise resolves with `null`. No argument is passed as `[]`,
  one as itself and several as an array, so a method of `()` takes none and one of a pair two.
  Arguments that do not decode, or a query that fails, reject the promise.
- **`shadow`** is where the component renders: `OpenShadow` (the default), `ClosedShadow`, or
  `NoShadow`, into the element itself, among the page's elements and styled by its CSS.
- **`styles`** is CSS adopted into the shadow root: it styles the component and nothing else,
  and the page's CSS does not reach in.

## From the page

Plain HTML and JavaScript:

```html
<x-counter id="counter" start="3"></x-counter>
<script>
  const counter = document.getElementById("counter");
  counter.addEventListener("count", (event) => console.log("count", event.detail));
  counter.setAttribute("start", "10");   // the component hears Receive
  counter.start = 100;                   // a property: any JSON value
  counter.reset();                       // a promise, resolved once the component is told
  counter.value().then((value) => console.log("value", value));
</script>
```

React 19 sets a property for anything that is not a string, and listens to custom events with
`on` and the event's name:

```jsx
<x-counter start={5} oncount={(event) => setCount(event.detail)} />
```

Vue does the same with its own syntax (tell it the name is a custom element with
`compilerOptions.isCustomElement`, and use `.prop` to force a property):

```vue
<x-counter :start.prop="5" @count="(event) => count = event.detail" />
```

## An element's life

- An element mounts its component when it is connected to the document, and disposes of it
  (its finalizers run, its subscriptions and forks end) when it is removed. The removal is acted
  on after the code that removed it has run, in a microtask, and not at all if that code put the
  element back: an element moved from one place to another, as frameworks move the items of a
  list, keeps its component and its state.
- A property set on an element before its name was defined (React and Vue set them as soon as
  they create an element) is taken over when the element mounts.
- `customElements.define` is forever: a page cannot take a name back or give it another class.
  `define` can be called again for the same name, though, with another definition and another
  component, and every element of the name already in the page disposes of its component and
  mounts the new one. That is what a program run again in the same page needs: browser GHCi's
  `:main` (`npm run dev`), or the tutorial's next compile.
- `Registration`'s `dispose` disposes of every element's component and leaves the name inert,
  its elements rendering nothing (and their methods resolving `null`), until it is defined
  again. It is what `Halogen.IO.Util.keepForNextRun` wants.
- An element keeps the shadow root it was first given, so a definition that comes later with
  another `shadow` renders there all the same.
- What the page does to an element reaches Haskell through the same synchronous callbacks as
  the browser's events, so Haskell has seen it by the time the page's code goes on; what the
  component does about it may come later. A page that sets an attribute and then calls a method
  has the method answered with the new input.

## Using others' custom elements

An element made elsewhere is an element like any other: `HH.element (HH.ElemName "x-picker")`,
`HP.attr` for its attributes and `HP.prop` for its properties. `CE.onEvent` handles an event with
a `detail`, decoded from JSON:

```haskell
HH.element (HH.ElemName "x-colour-picker")
  [HP.attr (HH.AttrName "value") st.colour, CE.onEvent "colour-changed" (Just . Picked)]
  []
```

An event whose detail does not decode as the type asked for makes no action.

## Natively

There is no page natively: `define` checks the name and returns a registration whose `dispose`
does nothing, and no element ever mounts. The package builds on every backend, so a program using
it type-checks natively.
