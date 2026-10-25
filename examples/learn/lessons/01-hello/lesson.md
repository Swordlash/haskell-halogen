title: Hello, Halogen

A Halogen application is made of components. A component is a value of type `H.Component`, built by `H.component` from three things: `initialState`, the state it starts with; `render`, which turns that state into HTML; and a list of handlers, which say what it does when something happens. This first component has nothing to do, so its state is `()` and its list of handlers is empty, `[]`: it does nothing at all. (`initialState` is given the component's input, here `()`, which it ignores with `_`, and `pure` hands back the state.)

## Rendering

`render` is an ordinary function from the state to HTML. It does not touch the page: Halogen calls it, compares what it returns with what is on the page, and changes only what differs. HTML is built with the functions of `Halogen.HTML`, imported as `HH`:

- `HH.div_` makes a `<div>` from a list of children;
- `HH.p_` makes a paragraph;
- `HH.text` makes text. Writing its argument as a string literal needs `OverloadedStrings`, at the top of the file.

The underscore at the end of `HH.div_` says the element has no properties; the name without it takes a list of properties first, as the next lesson shows.

Press **Compile**. GHC compiles your module, `Lesson`, in this page, with a small program around it that mounts its `component` in the column on the right.

## Exercise

Add a second paragraph under the first one, with your name in it: another `HH.p_` in the list that `HH.div_` is given. The items of a Haskell list are separated by commas.

Compile again to see it. **Reset** brings back the code you started with, and **Solution** shows one answer.
