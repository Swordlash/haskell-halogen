title: Hello, Halogen

A Halogen application is made of components. A component is a value of type `H.Component`, built by `H.mkComponent` from three things: `H.initialState`, the state it starts with; `H.render`, which turns that state into HTML; and `H.eval`, which says what it does when something happens. This first component has nothing to do, so its state is `()` and its `H.eval` is `H.mkEval H.defaultEval`, which does nothing at all.

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
