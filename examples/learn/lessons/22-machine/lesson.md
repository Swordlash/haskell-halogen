title: A view's logic as a pure step

This lesson builds search as you type. The reader types a word, and the view lists the words that start with it. A search starts after a short pause in typing, and an older answer must never replace a newer one. The view's rules are written as a pure function, and `Halogen.Machine` runs it.

This way of writing a view is optional. A view with one ask is simpler as a plain handler, as in lesson 7.

## Try it first

Compile `Start.hs` and type "ab" quickly. The list ends on the words for "a", while the field says "ab".

The server in this lesson is the function `search`. It waits, then answers. A short query takes it longer: 0.9 seconds for one letter, 0.3 seconds for more. So the answer for "a" always comes after the answer for "ab".

## The step

The top of the module is plain Haskell. `Model` is what the view knows. `Event` is what can happen to it. `Effect` is what it wants done. `step` takes an event and the model, and returns the new model and a list of effects:

```haskell
step :: Event -> Model -> (Model, [Effect])
```

`step` imports nothing of Halogen. A test can call it with any order of events, with no component and no browser.

`Halogen.Machine` makes one promise: the effects of one step do not mix with later events. It carries out a step's effects in the list's order, before it steps the next event.

## Two kinds of effect

`carry` says how each effect is carried out. An effect is one of two kinds.

- `M.Now` runs in place, before the next effect. `FocusField` uses it to put the cursor back in the field. Its code cannot wait, and the compiler checks that.
- `M.Ask` starts work that takes time, such as `M.run (liftIO (search text))`. The work cannot change the view. Its answer comes back later as an event, here `Found`, and `step` handles it like any other event.

A pause is an ask too. `M.AskUnder Pausing (M.delayCounted 0.3)` waits 0.3 seconds under the key `Pausing`, then brings `Paused`. `M.Cancel Pausing` stops every ask under that key, and its answer never comes. `M.delayCounted` is a pause the tutorial's tests wait for, since a search follows it.

## The component

The component keeps the machine in its state, made with `M.new`. `M.Spec` names the step, `carry`, and where the machine is in the state. `H.onAction (M.happen spec)` brings each event to the machine. `render` reads the model with `M.current`.

## Tickets

`Halogen.Latest` keeps the latest answer on the screen. Each search gets a ticket from `newAsk`, and its answer brings the ticket back. `takes` says whether to take an answer: it says no when an answer to a later search was taken already. `supersede` makes every search still out too old. `cleared` uses it, for Clear and for an empty field.

`Start.hs` takes a ticket for each search, but never looks at it.

## Exercise

Change only `step`:

1. In `Typed`, put `StopPause` before `Pause text`. Each key then cancels the pause still out, and typing "ab" quickly starts one search.
2. In `Found`, take the answer only when `takes ticket model.searches` gives `Just` the new `Asks`. Keep those in the model. On `Nothing`, change nothing.

Compile and type "ab" quickly. Then type "a", wait half a second, and type "b". The list ends on the words for "ab" both times. Without step 2, the second try still ends on "a".

## When not to use it

Use a plain handler for a view with one ask and simple rules. Use a pure step when a view has several asks, pauses or pushes, and its rules are hard to keep right in handlers.
