title: State and actions

A component's state is the value it keeps from one render to the next. Here it is an `Int`, a count, which `initialState` starts at `0`. Whenever the state changes, Halogen calls `render` with the new one and updates the page to match.

The state changes only in answer to an action: a value of a type of your own, here `Action`, with a constructor for each thing that can happen, `Increment` and `Decrement`. Two pieces connect a click to a new state:

1. In `render`, `HE.onClick`, from `Halogen.HTML.Events`, turns a click on a button into an action. It is given a function from the click to the action; the click itself is not needed here, so `\_ -> Increment` ignores it. It goes in the list of properties, the first of the two lists `HH.button` takes.
2. `handleAction` says what each action does, and `H.onAction handleAction`, in the component's list of handlers, hands it to Halogen. `modify`, from `Control.Monad.State`, changes the state by applying a function to it: `modify (+ 1)` adds one.

Halogen then renders again by itself: `render` is never called by your code.

## Exercise

The buttons in your code do nothing yet: compile it as it is and clicking them leaves the count at `0`, since nothing turns a click into an action. Give each one an `HE.onClick` with its action. Then write `handleAction`, which answers `Increment` with `modify (+ 1)` and `Decrement` with `modify (subtract 1)`, and hand it to Halogen: put `H.onAction handleAction` in the list of handlers, which is `[]` so far.

Compile, and click: the count follows. Compiling again starts it over from `0`, since each compile mounts a new component with a new state.
