title: Hello, Halogen

A Halogen application consists of components. Use `H.component` to create an `H.Component` from three parts: an initial state, a render function and a list of handlers.

`initialState` receives the component's input and returns its starting state. Here both are `()`: `_` ignores the input, and `pure ()` returns the state. The handler list is empty because this component only displays text.

## Rendering

`render` takes the state and returns a description of the HTML. Halogen compares that description with the current page and updates what changed. Use the functions from `Halogen.HTML`, imported as `HH`, to build it:

- `HH.div_` creates a `<div>` containing a list of children.
- `HH.p_` creates a paragraph.
- `HH.text` creates text. String literals need the `OverloadedStrings` extension declared at the top of the file.

An underscore, as in `HH.div_`, selects the version without properties. `HH.div` takes a property list first; the next lesson uses that form.

Press **Compile**. GHC compiles your `Lesson` module in the browser. A small wrapper program mounts its `component` in the result column.

## Exercise

Add another `HH.p_` to the children of `HH.div_`. Put your name in the new paragraph, and separate the list items with a comma.

Compile again to see the change. **Reset** restores the starting code, and **Solution** shows one possible answer.
