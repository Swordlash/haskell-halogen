title: Watch your component live
layers: devtools
args: -package haskell-halogen-devtools

A component's code says what it does; it does not say how often. Under this lesson's component is a panel that shows what the component does as it does it: `Halogen.DevTools`, from the package `haskell-halogen-devtools`. Click "+1" a few times and watch the panel.

The top of the panel is the tree of components, one row each, with a label and an id: here one row, `Lesson.State#` and a number, since a component is labelled with the name of its state's type unless `H.named` gives it another (`H.named "Clicks" component`). Beside the label are three counts: how many times the component has rendered, how many actions it has handled, how many times its state has changed. Under the tree is the log, the newest event first: `action`, `state changed`, `rendered in 0.2 ms`. The buttons above it narrow it to one kind of event, and clicking the component's row narrows it to that component. Hovering the row outlines, on the page, the element the component rendered.

## Where it comes from

Every Halogen tree reports what it does to `Halogen.Trace`, from `haskell-halogen-core`, but only once something listens: with nothing listening a tree builds no event at all, so a program without the panel pays nothing for it. The panel is a listener, and a Halogen component itself, in a tree of its own that it leaves out of what it shows. This lesson's program opens it under your component, before mounting the component, with

```haskell
DevTools.showing @State
panel <- DevTools.openIn (coerce tools :: HTMLElement)
socket <- runUI component () (coerce app :: HTMLElement)
```

and a program of your own would call `DevTools.open`, which puts the panel over the page, at the bottom right of the window. The panel is opened first because it can only reach the components made while it listens: those are the ones it can outline and put states back into.

`DevTools.showing @State` is why each `state changed` in the log shows the state it changed to, `State {count = 2, clock = "14:05"}`, with a "restore" button beside it. The panel cannot show a state without a `Show` instance, and Halogen asks no state for one, so showing is opt-in, for each type the program names; a state of any other type is listed under its type's name. "restore" puts that state back into the component, and the component renders it, as a `put` in its own code would: click "+1" five times, press "restore" on the third change, and the count is 3 again.

## Renders that change nothing

Leave the page alone for a few seconds and watch the counts: the renders and the state changes go up by one every second, though nothing on the page changes but the minute, once a minute. The timer's `Tick` reads the clock and writes the minute into the state with `modify (\st -> st {clock = minute})`. Most of the time the minute is the one already there, but the record is a new one, and Halogen renders after every write of a new state. It does not compare the new state with the old one field by field: it cannot, since a state need not have `Eq`, and comparing every state after every write would cost more than most renders. It only skips the render when the state is the very same value, as `modify id` leaves it.

## Exercise

Make the clock render only when the minute changes. `State` derives `Eq`, so `Tick` can read the state with `get`, compare, and `put` only a state that is different:

```haskell
st <- get
when (st.clock /= minute) $ put st {clock = minute}
```

with `get` and `put` from `Control.Monad.State`, and `when` from `Control.Monad`. Compile, and the counts go up only when you click, and once a minute. When what changes every tick is something the page does not show (a time kept only to compare with the next one, say), it need not be in the state at all, where every change of it renders.
