title: The DOM over a wire
layers: live
args: -package haskell-halogen-remote

In every lesson so far the component's program and the page it draws were in one place: the program ran in the page and changed its DOM itself. Phoenix LiveView, in Elixir, splits them. The program runs on a server; what it does to its DOM goes over a WebSocket to a small script in the page, which does it; and what the reader does in the page comes back over the same socket to the program's handlers. The page holds no state and runs none of the program, so it has nothing to download but the script, and a server that holds the state can show the same state to every page at once.

`haskell-halogen-remote` does that for Halogen. Compile the code, and under the list you see the wire: every message that crosses it, newest last. Here both ends are in this page, through `Halogen.Remote.Loopback`, so that the wire can be shown; what crosses is what would cross the network.

## The same component

Nothing in the code knows about the wire. `component` is an ordinary component, polymorphic in its monad `m`, and the program around it mounts it in a monad of its own, `RemoteDOM`. That is one more backend of the reconciler you have been using all along, as the in-memory DOM that renders a page ahead of time is (lesson 12), and the canvas and the three-dimensional scene are (lessons 11 and 19). Its DOM is a mirror of what the page shows: each operation the reconciler makes changes the mirror, which answers what the reconciler reads back, and is recorded to be sent.

The first message holds the first render, after a `["clear"]` that empties the page's element. Each operation is an array with its name first; among the first render's are

```
["create", 2, null, "div"]
["create", 3, null, "input"]
["prop", 3, "placeholder", "Something to buy"]
["insert", 2, 0, 3]
["listen", 4, "click", 1]
```

A node is a number both ends agree on: node 2 is the `div`, 3 the input. `insert` puts a child at an index among a parent's children, and `listen` asks the page to tell the program when the button, node 4, is clicked, for its listener 1. Click Add: a message goes the other way, `{"listener": 1, "node": 4, "type": "click", "event": {...}}`, the program runs the button's handler, and a render sends back only what changed. With nothing typed, nothing changes, and no message comes back at all.

## The exercise

Type into the input and press Add: nothing is added. The input has no handler, so what is typed stays in the page, and `handleAction` finds `st.draft` empty. Give the input `HE.onValueInput Typed`, compile, and type again. Each key now sends an `input` event whose `target` and `currentTarget` carry the input's `value`, which `HE.onValueInput` reads as it would in a page; Add then sends a `create` and an `insert` for the new item, and a `prop` that empties the input.

Then add a second button, `HH.button [HE.onClick (\_ -> Clear)] [HH.text "Clear"]`, with an action `Clear` that sets `items` to `[]`. Click it and look at the wire: one `remove` for each item. A node that is removed is forgotten at both ends, with everything under it and its listeners.

## What cannot cross a wire

The program is on one side and the page on the other, and the wire takes time. A ref, `H.getRef`, is a node of the mirror, not an element of the page: focusing it or measuring it does nothing useful. A handler hears of an event once the page has finished with it, too late to prevent its default, so the script prevents it itself for a form's `submit` and a click on a link, the two defaults a component that listens never wants, and for nothing else. And focus is the page's: the script sets an input's `value` only when it differs from what the input holds, so what the reader is typing is never undone.

## A server

Natively, `Halogen.Remote.Server.serve` does the same over a WebSocket, a session and an instance of the component per page that connects, and serves the script beside it. The repository's `examples/live` serves a board this way: every tab that opens it shows one board, and a line posted in one shows up in all of them (`cabal run halogen-example-live-server`).
