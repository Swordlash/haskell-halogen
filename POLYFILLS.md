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
fails with `ReferenceError: h$f is not defined`. hspec reaches most of them,
which is how we found them; hashable's turned up when a row-types record
hashed its labels.

Because the file is one of core's `js-sources`, every program that links
`haskell-halogen-core` gets these functions:
- core's and hooks' test suites, and `hspec-halogen`'s browser suites;
- any app built with haskell-halogen that happens to need them.

Each is written for a page as well as for Node. A page has no `process`, no
file system and no users, so each function reports that the way the C
function would.

**Defined only if missing.** Each is written as
`var h$f = typeof h$f === "function" ? h$f : function (…) {…};`. When
`unix` or `splitmix` ships its own `function h$f`, theirs is kept, whichever
file is linked first; this has been checked against the JavaScript backend in
both link orders. So an upstream fix needs no change here, and the polyfill
can be deleted at leisure.

**Leaving them out.** Core's `js-polyfills` flag (on by default) links the
file. Build with `--constraint="haskell-halogen-core -js-polyfills"` to
provide these some other way, or to keep them out of a bundle that doesn't
need them.

| Function | Pulled in by | What ours does | Where the fix belongs |
|---|---|---|---|
| `h$splitmix_init` | `splitmix`'s C seed function (`cbits/`), reached through `random` and QuickCheck | seeds from `crypto.getRandomValues` | **splitmix**: `js-sources` for the JavaScript backend with this function. The wasm backend compiles the C, so only JS lacks it. |
| `h$readlink` | `unix`, through `directory` | Node's `fs.readlinkSync`; `ENOSYS` in a page | **unix**, which already has JavaScript-backend support for other functions: implement these in its jsbits. These are what stopped hspec running on the JS backend. |
| `h$geteuid` | `unix`, through `directory` | `process.geteuid()` under Node, else `0` | as above |
| `h$getpwuid_r` | `unix`, through `directory` (a fallback when looking up the home directory) | reports "no such user" | as above |
| `h$sysconf` | `unix` (sizing `getpwuid_r`'s buffer) | a fixed 16384 | as above |
| `h$realloc` | the C `realloc`, from a library's C-backed code | copies into a new byte array | **GHC's JavaScript runtime** (`rts/js`), which has `malloc` and friends but not `realloc`. The linked output names the caller; find it before filing. |
| `h$eq_thread` | `ghc-internal`'s `Eq ThreadId` (`==` and `/=` on thread ids), as core's `Halogen.Query.Event.Window.interval` asks whether it is unsubscribed from its own thread | compares the two threads' ids | **GHC's JavaScript runtime** (`rts/js`), which has `h$cmp_thread` for `compare` but not this counterpart of the RTS's C `eq_thread`. |

| `h$XXH3_64bits_withSeed`, `h$hs_XXH3_64bits_withSeed_offset`, `h$hs_XXH3_64bits_withSeed_u64`, `h$hs_XXH3_64bits_withSeed_u32` | `hashable` (`Data.Hashable.FFI`), which hashes every `ByteString`, `Text`, `ShortByteString` and `ByteArray` with XXH3: a `HashMap Text`, a row-types `Rec`, anything keyed by text | XXH3-64 with a seed, ported from the xxHash 0.8.3 that hashable bundles; same values as the C | **hashable**: `js-sources` for the JavaScript backend with these functions (or the port below). The wasm backend compiles the C, so only JS lacks them. |
| `h$hs_XXH3_sizeof_state_s`, `h$XXH3_INITSTATE`, `h$XXH3_64bits_reset_withSeed`, `h$XXH3_64bits_update`, `h$hs_XXH3_64bits_update_offset`, `h$hs_XXH3_64bits_update_u64`, `h$hs_XXH3_64bits_update_u32`, `h$XXH3_64bits_digest` | `hashable`'s incremental XXH3, for lazy `ByteString` and lazy `Text` | keeps the seed and the bytes on the state's array object; the digest is the one-shot hash of them | as above |

The XXH3 functions are a faithful port of xxHash (BSD 2-Clause, Yann Collet; see
`core/NOTICE`), not a stand-in: hashable makes no
promise that a hash is the same across platforms, but a function named XXH3
should compute XXH3, and a port that agrees with the C can be checked against
it. It works on `BigInt`, reduced to 64 bits after each operation, which is
fast enough for the short keys hashable is given, and covers every one of
XXH3's length classes (0, 1–3, 4–8, 9–16, 17–128, 129–240, and the stripe and
block loop with the seeded secret past that). The incremental functions lean
on XXH3's guarantee that a stream hashes to what the one-shot function makes
of the same bytes. `core/test/Test/Hashable.hs` pins it to values hashable's C
computes natively, in every length class, through `hash` and
`hashWithSalt` (whose `Int` is 32 bits on this backend) and, on the JavaScript
backend, through the functions themselves for all 64 bits.

The JavaScript backend passes a `Word64` as two unsigned 32-bit halves, high
first, and returns one with the low half in `h$ret1`; an `Addr#`, a
`ByteArray#` and a `MutableByteArray#` each arrive as an array and an offset.

hspec needs `directory` to find the home directory, so that it can read
`~/.hspec`. `directory` normally takes the home directory from the
environment, so `h$getpwuid_r` only has to say "no entry".

## Other workarounds waiting on upstream

These aren't polyfills, but they're workarounds of the same kind.

### `@bjorn3/browser_wasi_shim` 0.3.0: debug logging is on unless you say otherwise

`new WASI(args, env, fds)` without an options object logs every WASI call to
the console (`wasi: 0 0`, …): the constructor calls
`debug.enable(options.debug)`, and `enable` treats `undefined` as `true`.

**Workaround:** `hspec-halogen/js/runner.mjs` and the tutorial's
`examples/learn/web/index.js` pass `{ debug: false }`. The other examples'
`web/index.js` files don't, so their pages log those lines.

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
  those `browserJS` generates for a safe binding (`jsbits/src/Halogen/JSBits.hs`).
