# haskell-halogen-store

Global state for a [haskell-halogen](https://github.com/Swordlash/haskell-halogen) application, ported from thomashoneyman's [purescript-halogen-store](https://github.com/thomashoneyman/purescript-halogen-store). The application has one store. Components update it by dispatching actions to a pure reducer and subscribe to the parts they need. A connected component renders again only when its selected value changes. The package depends only on `haskell-halogen-core`, contains no JavaScript and works on every backend.

## The store

```haskell
import Halogen.Store qualified as Store

data Store = Store {count :: Int, name :: Text}

data Action = Increment | Rename Text

reduce :: Store -> Action -> Store
reduce store Increment = store {count = store.count + 1}
reduce store (Rename new) = store {name = new}
```

`reduce` defines how each action changes the store. Add a `MonadStore` constraint to any component that needs access to it:

```haskell
toolbar :: forall q o m. (Store.MonadStore Action Store m) => H.Component q () o m
toolbar = H.component (\_ -> pure ()) render [H.onAction handleAction]
  where
    render _ = HH.button [HE.onClick (\_ -> Add)] [HH.text "Add one"]
    handleAction Add = Store.updateStore Increment
```

`Store.getStore` reads the current value. `Store.updateStore` passes an action and the current value to the reducer, saves the result and notifies subscribers.

## At the root

Components use `StoreT action store m` to access the store. Call `runStoreT` once before `runUI` to create the store and convert the component tree to the underlying monad:

```haskell
main :: IO ()
main = runBrowserDOM $ do
  body <- HA.awaitBody
  root <- Store.runStoreT (Store {count = 0, name = "Ada"}) reduce App.component
  void (runUI root () body)
```

`runAndEmitStoreT` also returns an emitter that reports every store value. Use it outside the component tree to save changes, for example.

`StoreT` wraps `ReaderT` and derives `MonadDOM`, `MonadAttributes`, `MonadBrowserDOM`, `MonadIndexedDB`, `MonadUnliftIO`, `MonadUUID` and the exception classes from its underlying monad. Components that use those classes work inside `StoreT` too. For a custom application monad, use `StoreT Action Store AppM`, or wrap `StoreT` in your monad and derive `MonadStore`.

## Following a part of it

A selector chooses a value from the store and defines how to detect changes. `selectEq (.count)` uses `==`, `select eq f` uses your comparison function, and `selectAll` reports the entire store on every update. `connect` passes the selected value and the original input to a component in a `Connected` record. Updates elsewhere in the store do not make that component render again:

```haskell
counter :: forall q o m. (MonadIO m) => H.Component q (Store.Connected Int Text) o m
counter = H.component pure render [H.onReceive put]
  where
    render connected = HH.p_ [HH.text (connected.input <> ": " <> T.pack (show connected.context))]

connectedCounter :: forall q o m. (Store.MonadStore Action Store m) => H.Component q Text o m
connectedCounter = Store.connect (Store.selectEq (.count)) counter
```

The component receives selected values through `H.onReceive`, just like any other input. The connection forwards queries to the component and passes its outputs to the parent.

Unlike the PureScript version, this port can read the store while creating the component's initial state. The child is therefore present on the first render. This also lets `hydrateUI` adopt HTML prerendered with `Halogen.Static` without changing it.

`Store.subscribe selector toAction` converts selected value changes into component actions. Call it in `H.onInitialize`, and use `getStore` to read the initial value: the subscription reports changes only. Use `emitSelected` if you need the emitter directly.

## Lifted with `liftEffect`

`HalogenM` has a `MonadStore` instance whenever its underlying monad does. Store operations use `H.liftEffect` because they do not wait. An update and its notifications complete before the action continues; a subscriber in the same tree handles the notification immediately. By comparison, `lift` and `liftIO` run on worker threads so they can wait while the rest of the tree continues.

`ReaderT`, `ExceptT`, `MaybeT`, `IdentityT`, `StateT`, `WriterT`, `RWST` and `ContT` provide the usual lifted instances.

## Differences from purescript-halogen-store

- `selectAll` treats every update as a change because Haskell has no equivalent of `unsafeRefEq` suitable here. Use `selectEq identity` with an `Eq` store to suppress unchanged values.
- Updates carry sequence numbers. Concurrent updates and updates triggered inside notifications can arrive out of order, so subscriptions ignore any update older than one already delivered.
- `HalogenStore` is abstract. Create one with `newHalogenStore` and obtain its emitter with `storeEmitter`. The `StoreT` constructor and `runReaderT` let you run against a manually created store, as the tests do.
- There is no built-in `useSelector`. You can implement one with `useLifecycleEffect` and `emitSelected`. The example below adds an orphan `MonadStore` instance for `HookM`, lifting operations with `Hooks.liftEffect`. Import `UseState` and `UseEffect` from `Halogen.Hooks.Types`:

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

`Halogen-store-test` runs natively and under Node on both browser backends. Use `cabal test haskell-halogen-store`, `npm run test-ghcjs` or `npm run test-wasm`. The suite checks components with a render spec that records their text. Native tests also use the real VDom driver and in-memory DOM to test a tree in `StoreT` and hydration of a connected component.
