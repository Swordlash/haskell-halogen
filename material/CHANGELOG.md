# Revision history for haskell-halogen-material

## Unreleased

* Every component labels its element ref with `newRefLabel` rather than a fixed
  name, so markup a parent hands to `tabsComponent` or `list` can use any ref
  label without taking over theirs. `tabsComponent`, `list` and `button` now
  need `MonadUUID m`, which `BrowserDOM` has.
* `tabsComponent` keeps every tab mounted and hides the ones not selected,
  where before it rendered only the selected tab. Switching tabs used to unmount
  the components in the tab being left, so a radio button picked or text typed
  there was gone on the way back; now they keep their own state. Every tab's
  components are created, and MDC-initialised, when the tab bar first renders.
* `tabsComponent` takes every new `TabsSpec` its parent renders, not only the
  first, so a parent that reacts to a control's output (arriving as
  `ChildOutput`) sees its re-render reach the tab contents. `selectedTab` still
  sets only the initial tab. If a new spec has fewer tabs than the selected
  index, the selection moves to the last tab and `SelectedTab` is raised.
* `TextFieldStyle` gains `Outlined`, drawn with MDC's notched outline, which
  MDC opens around the floating label.
* A text field's floating label carries the id its input's `aria-labelledby`
  names, so screen readers announce the label.
* A text field leaves floating its label to MDC. It used to float the label
  exactly while the text was non-empty, so clearing a focused field dropped the
  label under the caret; now it stays up until the field loses focus empty.

## 0.2.0.0 - 2026-09-14

* Move into the `haskell-halogen` monorepo. The library builds against the
  in-tree `haskell-halogen-core` rather than a pinned git revision, and the
  example app moved to `examples/material`.
* Build against GHC 9.12 and Hackage `protolude`; the 9.10.1 and
  `tomjaguarpaw/protolude` pins are gone.

## 0.1.0.0 -- 2025-01-12

* First version. Released on an unsuspecting world.
