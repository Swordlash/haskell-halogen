# haskell-halogen-devtools

A panel over the page that shows what a [haskell-halogen](https://github.com/Swordlash/haskell-halogen)
program's components do, as they do it.

```haskell
import Halogen.DevTools qualified as DevTools

main :: IO ()
main = runBrowserDOM $ do
  HA.endPreviousRun
  DevTools.showing @Counter.State
  tools <- DevTools.open
  body <- HA.awaitBody
  socket <- runUI Counter.component () body
  HA.keepForNextRun (socket.dispose >> tools.dispose)
```

`DevTools.open` puts the panel at the bottom right of the window, in an element of its own at
the end of the body; `DevTools.openIn element` puts it inside an element of the page instead, in
its flow; `DevTools.panel` is the panel as a component, for a program (or a test) that mounts it
itself. The socket's `dispose` closes it. Each has a variant that takes `Settings` (`openWith`,
`openInWith`, `panelWith`): how many events the log keeps (200 by default; this is also how many
states back a component can be put, since a state travels with its event) and how many the
listener keeps for the panel between two of its refreshes (5000). Nothing the panel holds grows
past these.

## What it shows

- **The tree of components**, one row each, indented under its parent, with its label and id
  (`Example.Counter.State#3`: the name of its state's type, unless `H.named "Counter"` gave it
  another) and how many times it has rendered, handled an action and changed its state, and how
  long its last render took. Clicking a row narrows the log to that component; clicking it
  again shows them all.
- **The log** of the last 200 events, newest first: components mounted, initialized and
  finalized, renders and how long they took, actions, state changes, queries, outputs,
  subscriptions and forks. The buttons above it narrow it to one kind.
- **An outline** on the page of the element a component rendered, while the mouse is over its
  row.
- **States**, for a type the program has named with `DevTools.showing @State` (which needs
  `Show State`): each state change in the log shows the state, with a "restore" button that puts
  it back into the component, which renders it as a `put` would. A component whose state is of
  any other type has its changes counted and listed, under the type's name. The panel cannot show
  what it has no `Show` for, and Halogen asks no state for one, so this is opt-in, per type.
- "Clear" forgets the log and the counts, and "Hide" folds the panel down to its title.

## How it works

It is a listener of `Halogen.Trace`, in `haskell-halogen-core`, to which every component tree
of the program reports once someone listens; with nothing listening a tree builds no events at
all, so a program without the panel pays nothing for it. The listener only keeps the events, and
the panel takes them a tenth of a second after the first of a batch, so a component that renders
on every frame does not make the panel render on every frame too.

Open the panel before mounting what it is to look at. It lists and counts every component it
hears from, but the trace keeps the means to outline a component, or to put a state back into
it, only for components made while someone listens.

The panel is a Halogen component itself, in a tree of its own, and leaves that tree's events out
of what it shows (it knows them by the type of its state). Its CSS is in the panel, every rule
under the class `halogen-devtools`, light and dark after the page's `prefers-color-scheme`; it
loads nothing from anywhere.

Natively the panel mounts in the in-memory DOM like any component, and its outline has no size.

## Tests

`Halogen-devtools-test` runs the panel beside a counter in headless Chromium on both browser
backends (see `hspec-halogen`): the counter listed and counted after clicks, its events in the
log and the log narrowed to a kind, a state put back, the outline of a hovered row, and the
panel's own tree left out. Natively it only reports that it skipped.

Lesson 16 of the tutorial (`examples/learn`) opens the panel under the reader's component.
