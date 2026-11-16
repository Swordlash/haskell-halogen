# haskell-halogen-devtools

A developer panel for [haskell-halogen](https://github.com/Swordlash/haskell-halogen) that shows the component tree, events and state changes as your program runs.

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

`DevTools.open` adds a panel at the bottom right of the window. `DevTools.openIn element` places it inside a specific page element, in the normal document flow. Use `DevTools.panel` if you want to mount the panel as a component yourself. Call the returned socket's `dispose` to close it.

Each option has a variant that accepts `Settings`: `openWith`, `openInWith` and `panelWith`. By default, the log retains 200 events, including the states available for restoration. The listener buffers up to 5000 events between panel refreshes. These limits keep the panel's memory use bounded.

## What it shows

The component tree shows one row per component, nested under its parent. Rows include the label and ID, render/action/state-change counts and the last render duration. Labels default to the state type, such as `Example.Counter.State#3`, or use the name from `H.named`. Click a row to filter the log; click it again to clear the filter. Hovering outlines the component's element on the page.

The log shows the most recent 200 events, newest first: mounting, initialization, finalization, renders, actions, state changes, queries, outputs, subscriptions, forks and slow syncs. Filter buttons select an event kind.

Register a type with `DevTools.showing @State` to display its state values. This requires `Show State` and enables a Restore button on each recorded state. Restoring behaves like `put` and triggers a render. Other state types still have their changes counted and listed by type name. Halogen does not require `Show` for component state, so displaying values is opt-in.

Clear resets the log and counters. Hide collapses the panel to its title.

## How it works

The panel subscribes to `Halogen.Trace` in `haskell-halogen-core`. Component trees create trace events only while a listener is present. The listener buffers events, and the panel refreshes a tenth of a second after the first event in a batch. This prevents a component that renders every frame from forcing the panel to do the same.

Open the panel before mounting the components you want to inspect. It can list and count events from existing components, but outlining and state restoration are available only for components created while a trace listener was active.

The panel is a Halogen component with its own tree. It recognises that tree by its state type and excludes its events from the display. All CSS is included in the panel and scoped under `halogen-devtools`. It follows `prefers-color-scheme` for light and dark themes and loads no external assets.

On the native backend, the panel mounts in the in-memory DOM. Its component outline has no size there.

## Tests

`Halogen-devtools-test` runs the panel beside a counter in headless Chromium on both browser backends, using `hspec-halogen`. It checks component counts after clicks, event logging and filtering, state restoration, hover outlines and exclusion of the panel itself. Native runs report a skip.

Lesson 16 of the tutorial (`examples/learn`) opens the panel under the reader's component.
