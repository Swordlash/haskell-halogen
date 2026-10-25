title: Child components

A component can render other components, each in a slot. Here a counter like the last lesson's is a component of its own, `counter`, and the parent `component` renders it with `HH.slot`:

```haskell
HH.slot "counter" () counter () (\Clicked -> HeardClick)
```

The arguments are the slot's label, `"counter"`; its index, `()`, as there is only one (an `Int` could tell several apart); the child component; its input, `()` again; and a function from the child's output to an action of the parent.

The parent's type lists its slots, in the row `Slots`: `"counter" .== H.Slot Query Output ()` says that the label `"counter"` holds components that answer `Query`, raise `Output` and are told apart by a `()`. `.==` is a type operator from `Data.Row`, imported with `type (.==)` (which needs `ExplicitNamespaces`), and a string as a type needs `DataKinds`.

The child's state is its own: the parent never sees the count, and the two talk in two ways.

## Up: output

When it is clicked, the counter calls `H.raise Clicked`. Halogen gives `Clicked` to the function the parent passed to `HH.slot`, and the parent handles the `HeardClick` it makes like any other action.

## Down: queries

The parent sends the child a value of the child's `Query` type, which the child answers in its `handleQuery`, handed to Halogen with `H.onQuery handleQuery` in its list of handlers, beside `H.onAction handleAction`. A constructor whose last field is the `a`, as in `Reset a`, is a tell: the parent sends it with `H.tell "counter" () Reset`, label and index first, and gets nothing back. The child's `handleQuery` returns `Just next`, the `a` it was given, to say it has handled it.

A constructor whose last field is a function to `a`, as in `GetCount (Int -> a)`, is a request: the child calls the function with its answer, and the parent asks with `H.request "counter" () GetCount`, which gives a `Maybe Int`, `Nothing` if that slot is empty (so the answer shows as `Just 2`).

## Exercise

Add `GetCount (Int -> a)` to `Query` and answer it in the counter's `handleQuery` with `Just . reply <$> get` (`get` joins the import from `Control.Monad.State`). Then give the parent an `AskCounter` action, with a button, that asks with `H.request` and `put`s the answer into its message.

Click the counter, then reset it and ask: the counter raises no output when it is reset, only when it is clicked, but asking always gets the count as it is.
