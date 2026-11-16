title: Hooks
layers: hooks
args: -package haskell-halogen-hooks

`haskell-halogen-hooks` lets you write a component as one function from input to HTML. The function requests state and effects through hooks. This lesson rewrites the counter from lesson 2 that way.

## A component made of hooks

`Hooks.component` creates an `H.Component`. `@Empty`, from `Data.Row`, declares that it has no child slots. The body uses `Hooks.do`, which requires `QualifiedDo`, and returns HTML with `Hooks.pure`.

- `Hooks.useState initial` returns the current value and a state handle, here `countId`. Event handlers run in `Hooks.HookM`. `Hooks.modify_ countId (+ 1)` updates the count and triggers a render.
- `Hooks.useLifecycleEffect` runs after the first render. This effect starts a `ticker` with `Hooks.fork`, waits one second with `Hooks.delay 1`, and increments `seconds` repeatedly. Its returned `Just (Hooks.kill ticker)` is the cleanup action, run when the component is removed.

`Hooks.delay` takes seconds and does not count as pending work. Tests waiting for a quiet program can therefore finish between clock ticks. Use it for repeating timers and temporary notices. Use `Hooks.delayCounted` for a delay before work that tests should await, such as a debounced request. Counted delays behave like `Hooks.async` waits for activity tracking. Both delay functions support the test clock from `Halogen.Activity.Control`.

## A hook of your own

`useCounter` is an ordinary function that owns the count and returns a `Counter` record. The record exposes the value and its `increment` and `decrement` handlers. The caller uses `counter <- useCounter 0`, then reads `counter.count` or attaches `counter.increment` to a button.

A hook's type lists the hooks it uses in order. `UseCounter hooks` prepends `Hooks.UseState Int` to the caller's remaining hooks. Every branch must use the same list, so conditionally adding a hook produces a type error. GHC enforces consistent hook order at compile time.

## Exercise

Add `Hooks.useState 1` inside `useCounter` for a step size. Use its value, `step`, in place of `1` in the increment and decrement handlers. Add `step` and `setStep = Hooks.put stepId` to `Counter`, then add buttons that set the step to 1 and 10.

Compile before updating `UseCounter`. GHC reports that the implementation now uses two hooks while its type declares one. Add another `Hooks.UseState Int` to the type-level list and compile again.
