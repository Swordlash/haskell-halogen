title: One store, many components
layers: store
args: -package haskell-halogen-store

So far each component has kept its own state, and two components have shared something only through their parent: input down, output up. When many components across the page need the same thing (who is signed in, what is in the basket, the settings), handing it through every component between them grows heavy. A store keeps it in one place: one value for the whole application, here `Store`, with a count and a name. `haskell-halogen-store`, imported as `Halogen.Store qualified as Store`, is a port of purescript-halogen-store.

## The reducer

The store changes only through actions, values that say what happened: here `Action`, with `Increment` and `Rename`. `reduce` is a pure function from the store and an action to the next store. It is the one place that says how the store changes, so it can be read, and tested, on its own.

## A tree in the store

The program around your module makes the store, with `Store.runStoreT initialStore reduce component`: a store holding `initialStore`, shared by every component of the tree, and the tree handed back in the browser's own monad, which `runUI` mounts as in every lesson. A component that uses the store says so in its type, `Store.MonadStore Action Store m`: its monad is any one with this store in it.

## Changing the store

`toolbar` keeps nothing of its own. Its `handleAction` calls `Store.updateStore`, which runs `reduce` on the store and the action, keeps the result and tells every component that follows the store. It does so at once, inside the click's action: no component waits for it.

## Following a part of it

`countView` is connected: `Store.connect (Store.selectEq (.count)) (display "Count" (T.pack . show))`. `Store.selectEq (.count)` is a selector, which picks the count out of the store and compares it with `==`. `Store.connect` reads the store as the component starts and gives `display` a `Store.Connected` as its input: `connected.context` is the part the selector picked and `connected.input` the input the parent passed, here `()`.

When the count changes, `display` is given a new `Store.Connected`, in its `H.onReceive` as any new input, and renders again. When the store changes anywhere else, the selector finds the count as it was and `countView` is left alone: a connected component renders again only for its own part of the store. `display` counts the times it was rendered again, to show it.

## Exercise

`nameView` is not connected yet: it shows the name the store starts with, and typing a new name changes the store but not the page. Connect it as `countView` is, to the name, with `Store.selectEq (.name)` and `display "Name" id`, and give it the type `countView` has. Then type a name and add one a few times: each view's count of renders goes up only when its own part changes.
