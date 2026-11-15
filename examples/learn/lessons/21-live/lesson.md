title: The DOM over a wire
layers: live
args: -package haskell-halogen-remote

A component does not have to run in the browser that displays it. Phoenix LiveView uses a server-side program that sends DOM updates to a small browser client and receives events over a WebSocket. The browser needs the client script rather than a copy of the application, and the server can share application state across tabs.

`haskell-halogen-remote` provides this model for Halogen. Compile the lesson to see protocol messages below the list, newest first. `Halogen.Remote.Loopback` runs both ends in this page so you can inspect them. The messages are the same ones used over a network connection.

## The same component

The component remains polymorphic in its monad. The wrapper selects `RemoteDOM`, another reconciler backend like the in-memory DOM, Pixi and Three.js.

`RemoteDOM` maintains a mirror of the page. Reconciler reads use that mirror; changes update it and are recorded for the client. The first render starts with `["clear"]`, then operations such as:

```
["create", 2, null, "div"]
["create", 3, null, "input"]
["prop", 3, "placeholder", "Something to buy"]
["insert", 2, 0, 3]
["listen", 4, "click", 1]
```

Both ends identify nodes by number: 2 is the div and 3 the input. `insert` places a child at an index in its parent. `listen` asks the client to report clicks on node 4 to listener 1. Source-mode marks appear as extra `attr` operations for `data-learn-src`.

Clicking Add sends `{"listener": 1, "node": 4, "type": "click", "event": {...}}`. The program handles the event and sends only the resulting DOM changes. If nothing changes, there is no update batch.

## The exercise

Typing and pressing Add currently adds nothing. Without an input handler, the text stays in the browser and `st.draft` remains empty.

Add `HE.onValueInput Typed`, compile and type again. Input events now carry `value` in their target snapshots, which the normal handler reads. Add creates and inserts an item, then sends a property update to clear the field.

Add `HH.button [HE.onClick (\_ -> Clear)] [HH.text "Clear"]` and a `Clear` action that sets `items` to `[]`. Inspect the resulting batch: it removes each item. Nodes still detached at the end of a batch are forgotten with their descendants and listeners, so separate removals for their text are unnecessary.

## What cannot cross a wire

Refs point to mirror nodes, so they cannot provide browser layout measurements. Loopback also cannot perform ref-based focus calls; native remote sessions can forward supported operations such as focus to the client.

Handlers receive events after browser dispatch. Calling `preventDefault` then is too late. Declare the required behaviour on the handler instead: `HE.preventingDefault (HE.matching (HE.keys ["/"]) (HE.onKeyDown (const Search)))` prevents slash input in the client and forwards only matching keys.

Without declarations, the client prevents listened-for form submissions and plain same-tab link clicks. Other defaults remain browser behaviour. To preserve typing and selection, it assigns an input's value only when it differs from the current browser value.

## A server

`Halogen.Remote.Server.serve` runs the same pipeline natively over WebSockets and serves the client script. Each connection gets a session and component instance. The repository's `examples/live` keeps a shared board outside those instances so posting in one tab updates every tab. Run it with `cabal run halogen-example-live-server`.
