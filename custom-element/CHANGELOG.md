# Changelog for `haskell-halogen-custom-element`

## Unreleased

The first version, which makes a Halogen component a custom element (a Web Component), for a
page that is not a Halogen app to use as it uses any element: plain HTML, React, Vue, Svelte,
Astro, HTMX.

`Halogen.CustomElement.define name definition component` defines the name, and every element of
it then mounts a tree of its own with the component in it, in an open shadow root by default
(`ClosedShadow`, or `NoShadow` to render into the element itself), with the definition's `styles`
adopted there. A `Definition` starts from `defaultDefinition`, and says how the element and the
component meet: `input` makes the component's input from the element's `Props` (`attr` reads an
attribute, `prop` a property set from JavaScript, decoded from JSON), whenever one of the
`observed` names changes, and the component hears it through `H.onReceive`, as it would from a
Halogen parent; `output` turns what the component raises into a `CustomEvent` with a JSON
`detail`, which bubbles and leaves the shadow root; and `methods` are element methods, each a
query, `method` for a request whose promise resolves with the answer as JSON (`null` when there
is none) and `tellMethod` for a tell.

An element mounts its component when it is connected to the document and disposes of it,
finalizers and all, when it is removed, but not when it is only moved. A property a page set
before the name was defined is taken over. Defining a name again swaps the component of every
element of it, which is what a program run again in the same page needs (browser GHCi, the
tutorial), and the `Registration`'s `dispose`, for `Halogen.IO.Util.keepForNextRun`, disposes
of every element's component and leaves the name inert until it is defined again. A name the
browser would refuse is thrown as a `JSError`.

For custom elements made elsewhere, `onEvent` handles an event whose `detail` decodes from JSON.

Natively `define` only checks the name, and nothing mounts.
