title: Hooks
layers: hooks
args: -package haskell-halogen-hooks

`haskell-halogen-hooks` writes a component as one function from its input to its HTML. What it needs on the way, state or an effect, it asks for where it needs it, rather than in separate fields of a spec. This is lesson 2's counter, written that way.

## A component made of hooks

`Hooks.component` turns the function into an `H.Component`; `@Empty` (from `Data.Row`) says it has no child components. The body is a `Hooks.do` block, which needs `QualifiedDo`, and ends with `Hooks.pure` and the HTML:

- `Hooks.useState initial` gives this render's value and a handle for changing it, `countId`. A click handler is a `Hooks.HookM` program: `Hooks.modify_ countId (+ 1)` changes the count, and the component renders again.
- `Hooks.useLifecycleEffect` runs its program once, after the first render. This one starts a `ticker` with `Hooks.fork`: a loop that waits a second (`threadDelay`, in `liftIO`) and adds one to `seconds`. What it returns, `Just (Hooks.kill ticker)`, runs when the component goes away (when you compile again, say) and stops the loop.

## A hook of your own

A hook is an ordinary function. `useCounter` holds the count and hands back a `Counter`: the value, and the handlers `increment` and `decrement`, so its caller cannot change the count any other way. The component uses it like the library's hooks: `counter <- useCounter 0`, then `counter.count` and `counter.increment`.

Its type names the hooks it uses. A `Hooks.Hook` is indexed by a type-level list of the hooks it runs, in order (`UseCounter hooks` is `Hooks.UseState Int` in front of whatever the caller runs after it), so every path through a component must run the same list. A hook inside an `if` would make its two branches different lists, a type error, which is how the rules of hooks (the same hooks, in the same order, on every render) are checked when the code compiles rather than when it runs.

## Exercise

Give the counter a step size: a second `Hooks.useState 1` in `useCounter`, whose value `step` replaces the `1` in `increment` and `decrement`. Add `step` and `setStep = Hooks.put stepId` to `Counter`, and two buttons that set the step to 1 and to 10.

Compile it before you change `UseCounter`: GHC refuses, because `useCounter` now uses two hooks and its type says one. Add a second `Hooks.UseState Int` to the list, and it compiles.
