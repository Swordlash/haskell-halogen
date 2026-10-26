# Changelog for `haskell-halogen-devtools`

## Unreleased

The first version: a panel over the page that shows what a program's components do, read from
`haskell-halogen-core`'s trace (`Halogen.Trace`). `Halogen.DevTools.open` docks it at the bottom
right of the window, `openIn` puts it in an element of the page, and `panel` is the component
itself. It shows the tree of components with their labels, ids and counts of renders, actions
and state changes; a log of the last 200 events, narrowed to a component or a kind of event; the
element a component rendered, outlined while its row is hovered; and, for a state type named with
`showing @State`, each state a component went through, with a button that puts it back. It
leaves its own tree out, updates at most ten times a second, and needs no JavaScript or CSS from
anywhere else.
