title: Lifecycle, forks and subscriptions

Lifecycle handlers run when a component mounts and when it is removed. Add `H.onInitialize startClock` and `H.onFinalize report` beside `H.onAction handleAction`.

`startClock` and `report` are component programs with the same capabilities as an action handler. They are passed directly to the lifecycle hooks; they are not constructors of the action type and cannot be triggered from `render`.

## Subscriptions

Use `H.subscribe` for events that come from outside the component's elements, such as a timer. `Window.interval 1000`, from `Halogen.Query.Event.Window`, emits once a second. `Tick <$ Window.interval 1000` converts each emission into `Tick`.

`startClock` subscribes to this emitter, so the seconds update automatically. `H.subscribe` returns a `H.SubscriptionId`. Pass it to `H.unsubscribe` if you need to stop listening before the component is removed.

## Forks

`H.fork` starts a component program that can continue after the current action finishes. `Remind` forks a program that waits two seconds with `H.async (liftIO (threadDelay 2000000))`, then updates state. `threadDelay` takes microseconds.

Put an action that waits in `H.async`. The wait runs on a separate Haskell thread, so the component handles clock ticks meanwhile. Put an action that does not wait in `H.sync`, as `report` does with `putStrLn`. It runs at once, and nothing else runs until it ends. `H.fork` returns an ID for early cancellation. This example discards it with `void`, as it does the subscription ID.

## What finalizing is for

Halogen automatically ends subscriptions and cancels forks when a component is removed. Here that happens on recompilation or when leaving the lesson, so a pending reminder will not arrive afterward.

Clean up resources you created outside those APIs in `H.onFinalize`: for example, manually started timers or threads, JavaScript widgets and open resources. This component has none, so `report` only calls `putStrLn`. Compile again to see the previous component's final message below the editor.

## Exercise

Add a stopwatch with Start and Stop buttons. Store elapsed tenths in `tenths` and its subscription in `running :: Maybe H.SubscriptionId`.

`Start` subscribes to `Window.interval 100`, converting ticks into `TenthTick`, and saves the subscription ID. `TenthTick` increments `tenths`. `Stop` calls `mapM_ H.unsubscribe state.running` and resets `running` to `Nothing`.

Show Start only when `running` is `Nothing`, and Stop otherwise. This prevents duplicate subscriptions.
