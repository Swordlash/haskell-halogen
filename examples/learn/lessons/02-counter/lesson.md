title: State and actions

State is the value a component keeps between renders. This counter stores an `Int`, initially `0`. Updating it makes Halogen call `render` again and update the page.

In this example, button clicks produce values of the `Action` type: `Increment` or `Decrement`. Two parts connect those clicks to state changes:

1. `HE.onClick`, from `Halogen.HTML.Events`, converts a click into an action. It takes a function such as `\_ -> Increment`, which ignores the event and returns `Increment`. Put the handler in the first list passed to `HH.button`, its properties.
2. `handleAction` defines what each action does. Register it with `H.onAction handleAction` in the component's handler list. `modify`, from `Control.Monad.State`, updates state by applying a function: `modify (+ 1)` increments it.

Halogen renders after a state change. You do not call `render` yourself.

## Exercise

Compile the starting code. Both buttons leave the count at `0` because neither has a click handler.

Add an `HE.onClick` handler to each button. Implement `handleAction` with `modify (+ 1)` for `Increment` and `modify (subtract 1)` for `Decrement`. Replace the empty handler list with `[H.onAction handleAction]`.

Compile and try both buttons. Compiling again creates a new component, so its count starts at `0`.
