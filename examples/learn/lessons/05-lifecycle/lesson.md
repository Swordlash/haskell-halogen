title: Lifecycle, forks and subscriptions

A component can act when it arrives and when it goes: `H.initialize` names the action Halogen runs once the component is on the page, and `H.finalize` the one it runs as the component is taken away. Here they are `Initialize` and `Finalize`, set in `H.defaultEval` beside `H.handleAction`.

## Subscriptions

What does not happen on one of the component's own elements, such as a timer, comes from an emitter, which the component subscribes to with `H.subscribe`. `Window.interval 1000`, from `Halogen.Query.Event.Window`, ticks every second, and `Tick <$ Window.interval 1000` makes each tick the action `Tick`. `Initialize` subscribes, and the seconds count up by themselves. `H.subscribe` returns a `H.SubscriptionId`, which `H.unsubscribe` takes to end the subscription before the component goes.

## Forks

`H.fork` starts a program of the component's own that goes on after the action that started it has ended. `Remind` forks one that waits two seconds, with `liftIO (threadDelay 2000000)` (the delay is in microseconds), and then changes the state; the clock ticks on meanwhile. `liftIO` is where a program may wait: it runs on a thread of its own while the page goes on. `H.fork` returns an id for stopping the fork early, which `void` drops, as it drops the subscription's.

## What Finalize is for

When the component goes away, which here is when you compile again or leave the lesson, Halogen ends its subscriptions and stops its forks by itself: a reminder still waiting never arrives. What it did not start, it cannot end: a thread or a timer you started yourself in `liftIO`, a JavaScript widget set up on one of the component's elements, a resource opened by hand. Those are what `Finalize` has to clean up. Here there is nothing of the kind, and `Finalize` only reports, with `putStrLn`: compile again, and what the old component printed as it went shows under the editor.

## Exercise

Add a stopwatch, with a Start button and a Stop button. `Start` subscribes to `Window.interval 100` with a `TenthTick` action that counts tenths of a second in a field `tenths`, and keeps the `H.SubscriptionId` in a field `running :: Maybe H.SubscriptionId`. `Stop` ends it with `mapM_ H.unsubscribe state.running`, which unsubscribes when there is a subscription, and sets `running` back to `Nothing`. Show Start when `running` is `Nothing` and Stop otherwise, so that the stopwatch is never subscribed twice.
