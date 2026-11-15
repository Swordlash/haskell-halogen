title: One store, many components
layers: store
args: -package haskell-halogen-store

So far, components have owned their state and shared it through parent input and child output. Passing data through many levels becomes cumbersome when several parts of a page need the same user, basket or settings.

A store keeps shared state in one place. This lesson uses `haskell-halogen-store`, a port of purescript-halogen-store, imported as `Halogen.Store qualified as Store`. The shared `Store` contains a count and a name.

## The reducer

Store changes are described by actions, here `Increment` and `Rename`. `reduce` takes the current store and an action and returns the next store. It is pure, so you can read and test the update rules independently of the UI.

## A tree in the store

The wrapper calls `Store.runStoreT initialStore reduce component`. This creates a store shared by the component tree and returns a component that `runUI` can mount in the browser monad.

Components that access the store declare `Store.MonadStore Action Store m`. They can use any monad providing that store.

## Changing the store

`toolbar` has no local state. Its handler calls `Store.updateStore`, which applies the reducer, saves the new value and notifies subscribers. The update happens synchronously within the action.

## Following a part of it

`countView` uses `Store.connect (Store.selectEq (.count)) (display "Count" (T.pack . show))`. The selector extracts `count` and compares values with `==`.

`connect` reads the store at initialization and passes a `Store.Connected` record to `display`. `connected.context` is the selected count, and `connected.input` is the parent's input, here `()`.

When the count changes, `display` receives a new input through `H.onReceive` and renders again. Changes to the name do not update `countView`. The displayed render counter lets you observe this.

## Exercise

`nameView` currently displays only the initial name. Typing updates the store, but the view is not subscribed to it.

Connect it with `Store.selectEq (.name)` and `display "Name" id`, using the same component type as `countView`. Compile, edit the name and increment the count. Each view's render counter should increase only when its selected value changes.
