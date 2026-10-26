# Changelog for `haskell-halogen-lazy`

## Unreleased

The first version, for a component the page has not got yet: the tree shows a placeholder where
it goes, a loader brings it, and it mounts in its place, as React's `lazy` and `Suspense` do.

`Halogen.Lazy.lazy loading loader` is a component of the same type as the one the loader gives.
It shows `loading.placeholder` when it is mounted, runs the loader in `lift` (so the rest of the
tree goes on while it waits), and then shows the loaded component in its place, with the input
the wrapper was given and every one it receives after; it raises what the component raises, and
sends the component the queries it is sent. A query that arrives before the component is there is
answered `Nothing`, as one sent to an empty slot. A loader that throws makes it show
`loading.failed` with the exception. `defaultLoading` is an empty `<div>` and the error in a
`<pre>`. `memo` shares one load between every mount: the first loads, the others get what it
loaded, and a failed load is tried again by the next mount.

`Halogen.Lazy.Dyld.load` is a loader for code the page loads on demand: on WebAssembly, a shared
library that GHC's dynamic linker links into a program it started, whose exported entry hands a
value over as a `Dynamic` (`offer` makes one) and which `load` checks the type of. It needs the
page to hand dyld over (`globalThis.halogenDyld`, see `available`); natively, on the JavaScript
backend and in a program linked into one `.wasm`, it throws `NotAvailable`. A library is
fetched and linked once per path; a fetch that failed is tried again, but a link that failed is
not, for as long as the page lives, since dyld will not link a library of the same name twice.
