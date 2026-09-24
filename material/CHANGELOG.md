# Revision history for haskell-halogen-material

## Unreleased

* `tabsComponent` now takes every new `TabsSpec` its parent renders, not only
  the first one. The tab bar mounts only the selected tab, so the controls in
  the other tabs lose their state on a switch. Before this, a parent had no way
  to keep that state, because the tab contents were fixed when the tab bar
  mounted. Now a parent can map the controls' outputs to its own actions (they
  arrive as `ChildOutput`), store the values in its own state and pass them back
  in the controls' specs, so a control comes back as it was left. The example
  app now works this way for its text fields, radio buttons and checkboxes.
  `selectedTab` still sets only the initial tab. If a new spec has fewer tabs
  than the selected index, the selection moves to the last tab and
  `SelectedTab` is raised.

## 0.2.0.0 - 2026-09-14

* Move into the `haskell-halogen` monorepo. The library builds against the
  in-tree `haskell-halogen-core` rather than a pinned git revision, and the
  example app moved to `examples/material`.
* Build against GHC 9.12 and Hackage `protolude`; the 9.10.1 and
  `tomjaguarpaw/protolude` pins are gone.

## 0.1.0.0 -- 2025-01-12

* First version. Released on an unsuspecting world.
