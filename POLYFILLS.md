# Polyfills, and other fixes that belong upstream

Workarounds in this repository for problems in other projects. Each entry
says what is missing or wrong, where we work around it, and which project the
fix should go to. When a fix lands upstream, the workaround it names can go.

## JavaScript backend polyfills

**File:** `core/jsbits/polyfills.js`, one of `haskell-halogen-core`'s
`js-sources` on the JavaScript backend.

GHC's JavaScript backend compiles a `foreign import ccall "f"` into a call to
a JavaScript function `h$f`. For the functions below, neither the runtime nor
the package that imports them provides one, so a program that reaches them
fails with `ReferenceError: h$f is not defined`. hspec reaches all of them,
which is how we found them.

Because the file is one of core's `js-sources`, every program that links
`haskell-halogen-core` gets these functions:
- core's and hooks' test suites, and `hspec-halogen`'s browser suites;
- any app built with haskell-halogen that happens to need them.

Each is written for a page as well as for Node. A page has no `process`, no
file system and no users, so each function reports that the way the C
function would.

| Function | Pulled in by | What ours does | Where the fix belongs |
|---|---|---|---|
| `h$splitmix_init` | `splitmix`'s C seed function (`cbits/`), reached through `random` and QuickCheck | seeds from `crypto.getRandomValues` | **splitmix**: `js-sources` for the JavaScript backend with this function. The wasm backend compiles the C, so only JS lacks it. |
| `h$readlink` | `unix`, through `directory` | Node's `fs.readlinkSync`; `ENOSYS` in a page | **unix**, which already has JavaScript-backend support for other functions: implement these in its jsbits. These are what stopped hspec running on the JS backend. |
| `h$geteuid` | `unix`, through `directory` | `process.geteuid()` under Node, else `0` | as above |
| `h$getpwuid_r` | `unix`, through `directory` (a fallback when looking up the home directory) | reports "no such user" | as above |
| `h$sysconf` | `unix` (sizing `getpwuid_r`'s buffer) | a fixed 16384 | as above |
| `h$realloc` | the C `realloc`, from a library's C-backed code | copies into a new byte array | **GHC's JavaScript runtime** (`rts/js`), which has `malloc` and friends but not `realloc`. The linked output names the caller; find it before filing. |

hspec needs `directory` to find the home directory, so that it can read
`~/.hspec`. `directory` normally takes the home directory from the
environment, so `h$getpwuid_r` only has to say "no entry".

## Other workarounds waiting on upstream

These aren't polyfills, but they're workarounds of the same kind.

### `@bjorn3/browser_wasi_shim` 0.3.0: debug logging is on unless you say otherwise

`new WASI(args, env, fds)` without an options object logs every WASI call to
the console (`wasi: 0 0`, …): the constructor calls
`debug.enable(options.debug)`, and `enable` treats `undefined` as `true`.

**Workaround:** `hspec-halogen/js/runner.mjs` passes `{ debug: false }`. The
examples' `web/index.js` files don't, so the deployed gallery logs those lines.

**Upstream:** default to off when `options.debug` is absent. First check
whether 0.4.x still does this; if not, upgrading fixes it. Its `random_get`
also uses `Math.random`, where `crypto.getRandomValues` would be the better
source.

### cabal-install: reuses the host `ghc-pkg` after the compiler changes

With one build directory used for two compilers, cabal can pair the new `ghc`
with the old one's `ghc-pkg`, cached in `<builddir>/cache/compiler`. It then
fails with "Version mismatch between ghc and ghc-pkg", even when
`--with-hc-pkg` is given.

**Workaround:** every `toolchain/` script that builds for wasm or JS deletes
`<builddir>/cache/compiler` first. `toolchain/ghcjs-env.sh` also passes the
compilers on the command line, because `with-hc-pkg` in the project file is
read too late.

**Upstream (cabal issue):** invalidate the cache when the compiler or
`--with-hc-pkg` changes, and apply `with-hc-pkg` from the project file before
the check.

### cabal-install 3.18 rejects the pinned wasm GHC

cabal 3.18 refuses `wasm32-wasi-ghc-9.14.1.20260731`. `ghc --info` reports a
`ghc-internal` unit id that differs from the one in the compiler's package
database, and the solver fails with "requires installed instance with unit id
ghc-internal-…".

**Workaround:** cabal is pinned to 3.16.1.0 (AGENTS.md, CI's
`CABAL_VERSION`).

**Upstream:** a cabal or ghc-wasm-meta issue, depending on which side is
wrong: whether the mismatch comes from how the wasm GHC is built (its
`--info`) or from how cabal 3.18 compares the two.

### Behaviour the GHC user's guide could document

- **wasm, async JSFFI imports.** A `safe` import's result is a thunk, and the
  thread only waits for the promise when it is forced. An `IO ()` import whose
  result nobody inspects lets the program carry on while the JavaScript is
  still running. The guide should say so, with the `evaluate` idiom
  (`hspec-halogen`'s `awaitJS`).
- **JavaScript backend, `interruptible` imports.** They need
  `InterruptibleFFI`, and the JavaScript gets its continuation as a trailing
  `$c` argument. The guide's JavaScript FFI section could show an example like
  those in `hspec-halogen/src/Test/Hspec/Halogen/Internal/JS.hs`.
