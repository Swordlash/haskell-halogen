title: Your component in a page you do not write
layers: elements
args: -package haskell-halogen-custom-element

A Halogen component can be used in plain HTML or a site built with React, Vue, Astro or HTMX. Expose it as a custom element such as `<x-lesson>`. The host page can then set attributes, listen for events and call methods without depending on Halogen.

Custom elements are part of the browser's Web Components APIs. A registered JavaScript class manages each element with that name, whether it came from HTML, a script or a framework. The name must contain a hyphen. A shadow root can isolate the component's markup and styles from the surrounding page.

## Defining the element

`haskell-halogen-custom-element` creates the JavaScript class. The lesson wrapper calls:

```haskell
CE.define "x-lesson" definition component
```

Each `<x-lesson>` then mounts its own component tree in a shadow root and disposes it when removed. Start with `CE.defaultDefinition` and configure how the page connects to the component. Import the fields with `Halogen.CustomElement (Definition (..))`.

- `input` builds component input from element properties. `CE.attr "label" props` reads the label attribute; `CE.prop` would decode a JavaScript property from JSON. Here `H.Component Query Text Int m` takes a `Text` label.
- `observed` lists names to watch. Changes recompute input and reach `H.onReceive`, implemented here by `receive`. Without this handler, state would retain the original label.
- `methods` exposes queries. `CE.tellMethod "reset" (\() -> Reset)` makes `element.reset()` send the same tell a Halogen parent would send with `H.tell`. The method returns a promise that resolves after delivery.
- `styles` supplies CSS scoped to the shadow root.

## The page

The wrapper inserts the custom element, three buttons and a paragraph. Its JavaScript interacts with the element through this interface:

```javascript
element.setAttribute('label', 'Pears');
element.reset();
element.addEventListener('count', (event) => say(`The page heard a count event: ${event.detail}`));
say(`count() answered ${await element.count()}`);
```

Click the counter, then "Call it Pears". The label changes without resetting the count because receiving input does not create a new component. "reset()" resets it to 0. The count event and `count()` method are not connected yet; you will add them below.

Browsers cannot unregister an element name or replace its class. The package instead registers one class and updates the component behind it on each compile. The wrapper saves `registration.dispose` with `HA.keepForNextRun`, so the next run first disposes existing instances, then installs the new definition.

## Exercise

1. Expose count outputs as browser events. Import `Data.Aeson (toJSON)` and add `output = \count -> Just ("count", toJSON count)` to the definition. `H.raise` already emits the count; this mapping turns it into a named event with JSON `detail`. Compile and click: the paragraph now reports it. The event bubbles beyond the shadow root.
2. Add `CE.method "count" (\() -> GetCount)` to `methods`. Compile and press "count()". Its promise resolves with the query answer, like `H.request` in lesson 4.
