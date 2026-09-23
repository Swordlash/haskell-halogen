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
* State handles and handler programs are branded with the component they
  belong to, so one cannot be used by another component or outlive its owner.
  The scope has a nominal role so `coerce` cannot bypass that boundary.
* Render and effect passes defer reentrant updates until every effect has
  returned its cleanup. A failed pass releases its guard so later updates can
  proceed.
* State writes from a fork request renders immediately, including in timer
  loops and trailing throttle actions whose fork is still running.
* `component` rejects a program that installs more than one query handler.
* `useTickEffectBy` and `useMemoBy` take the comparison on dependencies, for
  types with no `Eq` instance or an expensive one.
* `usePrevious`, and `useLocalStorage` / `useSessionStorage` / `useStorageWith`
  for state that outlives the page, after the example hooks in the PureScript
  repository. They take a key and a default, and how the value is written is
  `Web.Storage.Serialize` — JSON, for a type that has aeson instances.
