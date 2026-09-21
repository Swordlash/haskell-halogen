# Changelog for `haskell-halogen-hooks`

## 0.1.0.0 -- unreleased

* First version. A port of `purescript-halogen-hooks`: `useState`,
  `useLifecycleEffect`, `useTickEffect`, `useMemo`, `useRef` and `useQuery`,
  with the hook list tracked in the type of the hook program.
* `Halogen.Hooks.Extra.Hooks`: `useDebouncer`, `useThrottle`, `useGet`,
  `useEvent` and the `useStateFn` family, ported from
  `purescript-halogen-hooks-extra` rather than kept in a package of their own.
* `Halogen.Hooks.Extra.Actions.Events`: `preventDefault` and friends, for
  handlers that have to stop the browser doing it too.
