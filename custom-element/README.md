# haskell-halogen-custom-element

Expose a [haskell-halogen](https://github.com/Swordlash/haskell-halogen) component as a custom element that any page can use. Plain HTML, React, Vue, Svelte, Astro and HTMX can treat it like a built-in element. The package also provides helpers for using other libraries' custom elements in Halogen.

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

`define` takes an element name, a `Definition` and a component. The name must contain a hyphen. Every matching element gets its own component tree, whether it came from the initial HTML, JavaScript or another framework:

- **`input`** converts the element's `Props` into component input. `CE.attr` reads a text attribute, and `CE.prop` decodes a JavaScript property from JSON.
- **`observed`** lists the names to watch. Each is observed as an attribute and exposed as a property unless `HTMLElement` already defines it, as with `title` or `hidden`. Changes recompute `input` and deliver it through `H.onReceive`, using a small parent component around your component.
- **`output`** converts component outputs into named `CustomEvent`s with JSON `detail`. Events bubble and are `composed`, so listeners outside the shadow root can receive them.
- **`methods`** exposes queries as JavaScript methods. `CE.method` returns a promise of the JSON answer, or `null` if unanswered. `CE.tellMethod` returns a promise resolving to `null`. No arguments are encoded as `[]`, one argument as its value, and several as an array. A `()` method therefore takes no arguments, and a pair takes two. Decoding errors and failed queries reject the promise.
- **`shadow`** chooses where to render: `OpenShadow` by default, `ClosedShadow`, or `NoShadow` to render directly inside the element under the page's CSS.
- **`styles`** supplies CSS for the shadow root. It affects only the component, and page styles do not reach inside.

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

React 19 sets a property for anything that is not a string, and listens to custom events with `on` and the event's name:

```jsx
<x-counter start={5} oncount={(event) => setCount(event.detail)} />
```

Vue does the same with its own syntax (tell it the name is a custom element with `compilerOptions.isCustomElement`, and use `.prop` to force a property):

```vue
<x-counter :start.prop="5" @count="(event) => count = event.detail" />
```

## An element's life

The element mounts its component when connected to the document. Removing it runs finalizers and ends subscriptions and forks. Cleanup happens in a microtask; if the element has already been reinserted, cleanup is skipped. Moving an element therefore preserves its component and state.

Properties assigned before registration are picked up when the element mounts. This supports frameworks that assign properties as soon as they create an element.

The browser cannot unregister a custom element name, but you can call this package's `define` again. Existing elements dispose their old components and mount the new definition. This supports rerunning `:main` in browser GHCi and recompiling tutorial programs.

`Registration.dispose` removes every mounted component and leaves the name inactive until redefined. Elements render nothing and their methods resolve to `null`. Register this cleanup with `Halogen.IO.Util.keepForNextRun`.

An element retains its original shadow root even if a later definition requests another `shadow` mode. Attribute and property changes enter Haskell through synchronous callbacks. The component's response may run later, but a method called after an attribute update sees the new input.

## Using others' custom elements

Use `HH.element (HH.ElemName "x-picker")` for another library's custom element, with `HP.attr` for attributes and `HP.prop` for properties. `CE.onEvent` decodes an event's JSON `detail`:

```haskell
HH.element (HH.ElemName "x-colour-picker")
  [HP.attr (HH.AttrName "value") st.colour, CE.onEvent "colour-changed" (Just . Picked)]
  []
```

If the event detail cannot be decoded as the requested type, no action is produced.

## Natively

On the native backend, `define` validates the name and returns a registration with a no-op `dispose`. No component mounts. The package still builds, allowing native type checking of programs that use it.
