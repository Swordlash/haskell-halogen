title: Child components

A component can mount child components in slots. Here `counter` owns its own state, and the parent mounts it with `HH.slot`:

```haskell
HH.slot "counter" () counter () (\Clicked -> HeardClick)
```

The arguments are the label `"counter"`, the index `()`, the child component, its input `()`, and a function converting child outputs into parent actions. There is one counter, so its index is `()`. Use a type such as `Int` to distinguish several counters under one label.

The parent's `Slots` row declares this interface: `"counter" .== H.Slot Query Output ()`. It specifies the child's query type, output type and index type. Import `.==` from `Data.Row` with `type (.==)`, enable `ExplicitNamespaces`, and use `DataKinds` for the type-level label.

The parent does not access the child's state directly. They communicate through outputs and queries.

## Up: output

When clicked, the counter calls `H.raise Clicked`. The function passed to `HH.slot` converts `Clicked` into `HeardClick`, which the parent handles like any other action.

## Down: queries

The parent sends values of the child's `Query` type. The child handles them in `handleQuery`, registered with `H.onQuery handleQuery` alongside its action handler.

`Reset a` is a tell: its final field is the continuation value `a`. Send it with `H.tell "counter" () Reset`. The child returns `Just next` to acknowledge it, and the parent receives no result.

`GetCount (Int -> a)` is a request: the child applies the continuation function to its answer. `H.request "counter" () GetCount` returns `Maybe Int`, with `Nothing` if the slot is empty. A successful answer might display as `Just 2`.

## Exercise

Add `GetCount (Int -> a)` to `Query`. Handle it with `Just . reply <$> get`, adding `get` to the `Control.Monad.State` import.

Give the parent an `AskCounter` action and a button that triggers it. Use `H.request` to read the count and `put` to display the answer in the parent's message.

Click the counter, reset it and ask for its value. Resetting produces no `Clicked` output, but a query still returns the current count.
