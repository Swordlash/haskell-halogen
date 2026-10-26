title: Your component in a page you do not write
layers: elements
args: -package haskell-halogen-custom-element

Every lesson so far has mounted its component into a page that a Halogen program owns. Many pages are not like that: a page written by hand, a site built with React, Vue or Astro, a server's templates driven by HTMX. Such a page can still use a Halogen component, once the component is a custom element: an element with a name of its own, such as `<x-lesson>`, which the page writes as it writes `<button>`, sets attributes on, listens to and calls methods of, without knowing what is behind it.

Custom elements are the browser's own, one of the standards called Web Components. A page gives a name with a hyphen in it a class of JavaScript, and from then on the browser has that class run every element of the name: one in the page's HTML, one a script inserts, one a framework renders. The class decides what the element shows, usually inside a shadow root, a part of the page of the element's own that the page's CSS does not reach into and whose styles do not leak out.

## Defining the element

`haskell-halogen-custom-element` writes the class. This lesson's program, around your module, calls

```haskell
CE.define "x-lesson" definition component
```

after which every `<x-lesson>` mounts a tree of its own with `component` in it, into its shadow root, and takes it down when the element is removed from the page. `definition` says how the element and the component meet. It starts from `CE.defaultDefinition` and sets what this element needs, with the fields of `Definition` in scope (`import Halogen.CustomElement (Definition (..))`):

- `input` is the component's input, made from what the page set on the element: `CE.attr "label" props` is the `label` attribute, if there is one (`CE.prop` would read a property set from JavaScript, decoded from JSON). The component here is `H.Component Query Text Int m`: its input is the label, `Text`.
- `observed` names what `input` reads. When the page changes one of them, `input` is asked again and the component hears the new label as it would from a parent: through `H.onReceive`, here `receive`. Without `H.onReceive` it would keep the label it started with.
- `methods` are the element's methods, each a query. `CE.tellMethod "reset" (\() -> Reset)` makes `element.reset()` send the tell `Reset`, as lesson 4's parent sent it with `H.tell`. A method returns a promise, which resolves once the component has been told.
- `styles` is CSS for the shadow root: it styles the button, and nothing else on the page.

## The page

The program puts plain HTML into the result: the element, three buttons and a paragraph, and a script of a few lines that is all the page knows of the element:

```javascript
element.setAttribute('label', 'Pears');
element.reset();
element.addEventListener('count', (event) => say(`The page heard a count event: ${event.detail}`));
say(`count() answered ${await element.count()}`);
```

Click the counter, then "Call it Pears": the label changes and the count stays, since a new input is not a new component. "reset()" sets the count back to 0. But the paragraph hears no event when you click, and the element has no `count()` method: that is the exercise.

A page cannot take a custom element's name back, nor give it another class, but each compile defines `x-lesson` with your new component. So the package gives the browser one class for the name, and swaps what is behind it. The program keeps `registration.dispose` with `HA.keepForNextRun`, as the other lessons kept the socket's `dispose`: the next compile runs it first, which takes the component of every `<x-lesson>` down and leaves the name inert, and then defines the name again, over its own page.

## Exercise

1. Make the count an event. `H.raise` already raises it, but the element dispatches only what `output` turns into an event: give the definition `output = \count -> Just ("count", toJSON count)`, the event's name and its `detail` as JSON (`import Data.Aeson (toJSON)`). Compile, click the counter, and the page's paragraph says what it heard. The event bubbles, and leaves the shadow root, so the page could have listened for it anywhere above the element.
2. Give the element a `count()` method, which asks with the request `GetCount`: add `CE.method "count" (\() -> GetCount)` to `methods`. Compile and press "count()": the promise resolves with the count, as `H.request` gave lesson 4's parent its answer.
