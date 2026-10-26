# Changelog for `haskell-halogen-store`

## Unreleased

The first version: a port of purescript-halogen-store, global state for a
Halogen application.

An application keeps one store, a value that only actions change, through a
pure reducer. A component asks for it with a constraint,
`MonadStore action store m`, and reads it with `getStore` and changes it with
`updateStore`. The tree runs in `StoreT action store m`, which `runStoreT`
makes the store for and hoists away at the root, before `runUI`;
`runAndEmitStoreT` also gives an emitter of every value the store takes, for
the program around the tree. `StoreT` takes the DOM classes, `MonadUnliftIO`,
`MonadUUID` and the exception classes from the monad under it, so components
written against those work in it unchanged.

`connect` gives a component the part of the store a `Selector` picks out,
with its input, as a `Connected`, and renders it again when that part
changes and not otherwise; its queries and its output go through the
connection. `selectEq` compares the part with `==`, `select` with a function
of your own, and `selectAll`, which in PureScript compares by reference,
counts every update as a change here, since Haskell has no honest reference
equality: use `selectEq identity` for a store with `Eq`. `subscribe` turns the
part's changes into a component's actions, and `emitSelected` gives the
emitter itself.

Two things differ from the original beyond that. A connected component is
there from the first render, because the connection reads the store in its
initial state (made in the component's monad here) rather than in its
initializer, so a page prerendered with `Halogen.Static` is adopted by
`hydrateUI` as it is. And because two threads can update the store at once,
or a subscriber can update it inside another update's notification, every
update carries its number, and a subscriber is never handed an update older
than one it already has, so that what it heard last is what the store holds.

`HalogenM` uses the store through `liftEffect`, not `lift` (which is where a
program waits, on a worker thread): an update made in an event's action is
in the store, and in the tree's components that follow it, before the action
goes on. There is no `useSelector` for hooks; the README shows the few lines
it takes.
