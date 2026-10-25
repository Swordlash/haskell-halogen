# haskell-halogen-store

Global state for a [haskell-halogen](https://github.com/Swordlash/haskell-halogen) application:
a port of thomashoneyman's
[purescript-halogen-store](https://github.com/thomashoneyman/purescript-halogen-store). The
application has one store, a value changed only by actions through a pure reducer; a component
reads it, updates it, and follows the part of it it needs, rendering again only when that part
changes. It depends on `haskell-halogen-core` alone, has no JavaScript, and works the same on every
backend.

## The store

```haskell
import Halogen.Store qualified as Store

data Store = Store {count :: Int, name :: Text}

data Action = Increment | Rename Text

reduce :: Store -> Action -> Store
reduce store Increment = store {count = store.count + 1}
reduce store (Rename new) = store {name = new}
```

`reduce` is the one place that says how the store changes. A component that uses the store says so
with a constraint, and stays polymorphic in its monad:

```haskell
toolbar :: forall q o m. (Store.MonadStore Action Store m) => H.Component q () o m
toolbar = H.component (\_ -> pure ()) render [H.onAction handleAction]
  where
    render _ = HH.button [HE.onClick (\_ -> Add)] [HH.text "Add one"]
    handleAction Add = Store.updateStore Increment
```

`Store.getStore` reads the store as it is now, and `Store.updateStore` runs the reducer on it and
an action, keeps the result and tells everyone who follows the store.

## At the root

The tree runs in `StoreT action store m`, which `runStoreT` makes the store for and takes away,
once, before `runUI`:

```haskell
main :: IO ()
main = runBrowserDOM $ do
  body <- HA.awaitBody
  root <- Store.runStoreT (Store {count = 0, name = "Ada"}) reduce App.component
  void (runUI root () body)
```

`runAndEmitStoreT` also gives an emitter of every value the store takes, for the program around the
tree (to save the store as it changes, say). `StoreT` is a `ReaderT` underneath and derives the DOM
classes (`MonadDOM`, `MonadAttributes`, `MonadBrowserDOM`, `MonadIndexedDB`), `MonadUnliftIO`,
`MonadUUID` and the exception classes from the monad under it, so a component written against
those works inside it too. An application monad of its own goes under `StoreT`
(`StoreT Action Store AppM`), or derives its own `MonadStore` instance from a `StoreT` inside it.

## Following a part of it

A selector picks a part of the store and says when it changed: `selectEq (.count)` compares with
`==`, `select eq f` with a function of your own, and `selectAll` is the whole store, changed on
every update. `connect` gives a component that part with its input, as `Connected`, and renders it
again whenever the part changes, and not when the store changes elsewhere:

```haskell
counter :: forall q o m. (MonadIO m) => H.Component q (Store.Connected Int Text) o m
counter = H.component pure render [H.onReceive put]
  where
    render connected = HH.p_ [HH.text (connected.input <> ": " <> T.pack (show connected.context))]

connectedCounter :: forall q o m. (Store.MonadStore Action Store m) => H.Component q Text o m
connectedCounter = Store.connect (Store.selectEq (.count)) counter
```

The connected component hears of a change as of any new input, in its `H.onReceive`. It answers
the queries sent to the connection, and the connection raises its output. Unlike the original, the
connection reads the store in its initial state (made in the component's monad here, pure in
PureScript), so the component is there from the first render, and a page `Halogen.Static`
prerendered with it is adopted by `hydrateUI` as it is.

`Store.subscribe selector toAction`, in a component's `H.onInitialize`, turns the part's changes
into actions instead; the emitter carries changes only, so the component reads the present value
with `getStore` beside it. `emitSelected` gives the emitter itself.

## Lifted with `liftEffect`

`HalogenM` has a `MonadStore` instance when its monad has one, and it uses the store through
`H.liftEffect`, not `lift`. In this port `lift` and `liftIO` are where a component's program may
wait: they run on a worker thread while the rest of the tree goes on. The store's operations never
wait, so they run at once: an update made in an event's action is in the store, and its
subscribers have been told, before the action goes on, and a subscriber in the same tree handles
it there and then. `ReaderT`, `ExceptT`, `MaybeT`, `IdentityT`, `StateT`, `WriterT`, `RWST` and
`ContT` lift it as usual.

## Differences from purescript-halogen-store

- `selectAll` cannot compare with `unsafeRefEq`, which Haskell has no honest version of, so it
  counts every update as a change. With an `Eq` store, `selectEq identity` notices an update that
  changed nothing.
- The store's updates carry the number of the update. Here two threads can update the store at
  once, and a subscriber can update it again inside another update's notification; each
  subscription drops an update older than one it was already given, so that what a subscriber
  heard last is what the store holds.
- `HalogenStore` is abstract: `newHalogenStore` makes one and `storeEmitter` gives its emitter. With
  `StoreT`'s constructor, a program can be run against a store made by hand (`runReaderT`), as the
  tests do.
- There is no `useSelector`. It belongs with the hooks, and a package for it would sit between
  `haskell-halogen-hooks` and this one; in a hooks component it is a few lines with
  `useLifecycleEffect` and `emitSelected`, after an instance for `HookM` (which, like `HalogenM`'s,
  lifts with the hooks' `liftEffect`; an orphan in your code, with `UseState` and `UseEffect`
  from `Halogen.Hooks.Types`):

  ```haskell
  instance (Store.MonadStore a s m) => Store.MonadStore a s (Hooks.HookM scope slots output m) where
    getStore = Hooks.liftEffect Store.getStore
    updateStore = Hooks.liftEffect . Store.updateStore
    emitSelected = Hooks.liftEffect . Store.emitSelected

  -- The selected part, Nothing until the component's first effect has read it.
  useSelector
    :: forall a s b scope q slots output m i
     . (Store.MonadStore a s m)
    => Store.Selector s b
    -> Hooks.Hook scope q slots output m (UseState (Maybe b) : UseEffect () : i) i (Maybe b)
  useSelector selector = Hooks.do
    (value, valueId) <- Hooks.useState Nothing
    Hooks.useLifecycleEffect $ do
      emitter <- Store.emitSelected selector
      _ <- Hooks.subscribe (Hooks.put valueId . Just <$> emitter)
      store <- Store.getStore
      Hooks.put valueId (Just (selector.select store))
      pure Nothing
    Hooks.pure value
  ```

## Testing

`Halogen-store-test` runs under Node on all three backends like core's suite
(`cabal test haskell-halogen-store`, `npm run test-ghcjs`, `npm run test-wasm`): components
rendered by a render spec that records their text, and natively, through the real VDom driver into
the in-memory DOM, a tree in `StoreT` and a connected component adopting a prerendered page.
