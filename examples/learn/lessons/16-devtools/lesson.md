title: Watch your component live
layers: devtools
args: -package haskell-halogen-devtools

The panel below this component shows its renders, actions and state changes as they happen. It comes from `Halogen.DevTools` in `haskell-halogen-devtools`. Click "+1" a few times and watch it update.

The component tree has one row here, labelled `Lesson.State#` followed by its ID. Labels default to the state type's name; `H.named "Clicks" component` supplies a custom one. Each row shows render, action and state-change counts. Hovering outlines the component on the page.

The log lists recent events, newest first. Use the event buttons to filter by kind or click a component row to filter by component.

## Where it comes from

Component trees produce `Halogen.Trace` events while a listener is present. The devtools panel is one such listener and is itself a Halogen component, but excludes its own tree from the display. The lesson wrapper opens it before mounting your component:

```haskell
DevTools.showing @State
panel <- DevTools.openIn (coerce tools :: HTMLElement)
socket <- runUI component () (coerce app :: HTMLElement)
```

In your application, use `DevTools.open` to place the panel at the bottom right of the window. Open it before the components you want to inspect: outlining and state restoration require them to have been created while a listener was active.

`DevTools.showing @State` enables state values in the log and requires `Show State`. Other state types still have their changes counted and listed. Register each type whose values you want to inspect.

Restore writes a recorded state back to the component and triggers a render, like `put`. Click "+1" five times, then restore the third change to return to a count of 3.

## Renders that change nothing

Wait a few seconds. The render and state-change counts increase every second, although the displayed clock changes only once a minute.

`Tick` always writes a new record with `modify (\st -> st {clock = minute})`. Halogen does not compare fields using `Eq`: component state need not provide that instance. It can skip an unchanged value such as `modify id`, but a fresh record still triggers a render even if its fields match.

## Exercise

Update state only when the minute changes:

```haskell
st <- get
when (st.clock /= minute) $ put st {clock = minute}
```

Import `get` and `put` from `Control.Monad.State` and `when` from `Control.Monad`. Compile and watch the counters. They should now increase on clicks and minute changes.

Values used only for bookkeeping may not need to be in rendered state at all. Updating them there would otherwise trigger unnecessary renders.
