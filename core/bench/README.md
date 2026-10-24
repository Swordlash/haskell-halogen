# Native HTML serialisation benchmark

Run from the repository root with the host GHC:

```sh
cabal bench bench:render-to-text --builddir=dist-newstyle/native -O2 \
  --benchmark-options='--regress allocated:iters +RTS -T -RTS'
```

Criterion reports time per render and, under `allocated`, the `iters`
coefficient gives bytes allocated per render. This is total allocation, not
peak live memory. `-O2` applies to the library as well as the benchmark; use
the same optimisation level when comparing revisions.

For a shorter run with machine-readable results:

```sh
cabal bench bench:render-to-text --builddir=dist-newstyle/native -O2 \
  --benchmark-options='-L 0.2 --resamples 300 --regress allocated:iters --csv /tmp/render.csv --json /tmp/render.json +RTS -T -RTS'
```

The benchmark constructs each mutable DOM once, before measuring. Each
iteration calls the public `renderToText` and forces the entire strict
`Text` with `nfIO`, including the final conversion from the builder. It
measures serialisation only, excluding DOM construction, component
initialisation and file I/O. The benchmark is native-only because the
in-memory DOM and static renderer are native-only.

The cases cover a small card; flat lists of 100, 1000 and 10000 rows;
10, 100 and 1000 nested elements around 64 KiB of text; binary trees;
long plain and escape-heavy Unicode text; attribute-heavy markup; and
raw script text. The deep cases deliberately expose copying an already
serialised subtree at every ancestor.

## Comparison recorded on 2026-10-23

GHC 9.14.1, text 2.1.3, Criterion 1.6.5.0, `-O2`, Linux x86_64,
Intel Core i7-1270P, one RTS capability. Each variant used the same
benchmark fixtures and the shorter command above, with no concurrent
builds. All variants return and fully evaluate a strict `Text`.

The variants were:

- `strict`: the original renderer at `00dfe13`.
- `lazy`: recursive results and concatenation changed to lazy `Text`, with
  the original `Text.concatMap` escaping and one final `toStrict`.
- `builder`: recursive results changed to `Text.Lazy.Builder`, keeping the
  original escaping, with one final `toLazyText` / `toStrict`.
- `final`: the builder plus the current escaping: preserve the plain
  prefix as a slice and replace special characters in the remaining text
  in whole passes, starting with ampersands.

Mean time per render (ms):

| Case | Strict | Lazy | Builder, original escaping | Final |
| --- | ---: | ---: | ---: | ---: |
| 10000 rows | 39.04 | 59.03 | 41.87 | 25.31 |
| 1000 levels around 64 KiB | 30.16 | 75.86 | 3.91 | 0.388 |
| 1 MiB plain text | 78.91 | 78.27 | 76.12 | 1.03 |
| Escape-heavy Unicode text | 27.55 | 26.68 | 27.49 | 11.14 |
| 1000 inputs with escaped attributes | 3.11 | 3.60 | 3.28 | 2.10 |

Bytes allocated per render, expressed in MiB:

| Case | Strict | Lazy | Builder, original escaping | Final |
| --- | ---: | ---: | ---: | ---: |
| 10000 rows | 224.70 | 286.52 | 235.78 | 63.40 |
| 1000 levels around 64 KiB | 144.93 | 662.65 | 9.98 | 1.42 |
| 1 MiB plain text | 139.00 | 138.00 | 138.00 | 1.00 |
| Escape-heavy Unicode text | 56.31 | 54.38 | 54.38 | 32.32 |
| 1000 inputs with escaped attributes | 19.60 | 20.18 | 17.99 | 7.69 |

[results.csv](results.csv) records all 13 cases, with times in microseconds
and allocations in bytes. These short local runs show the scale of the
differences; small timing differences are noise rather than a performance
guarantee. The small-card case stayed around 2 microseconds.

Strict `Text` already concatenates a list of siblings efficiently via
`mconcat`; the repeated subtree copying is across ancestor boundaries.
Lazy text avoids copying the bytes there but still repeatedly rebuilds
chunk-list spines. A builder fixes that part, but the original escaping
still allocates a singleton `Text` for every ordinary character. Both
parts need to change for the long-text cases to improve.

Two other escaping approaches were tried before choosing the final one:
appending each plain run and entity directly to the builder, and folding
characters with `Builder.singleton`. They reduced plain-text allocation
but made allocation worse on dense escapes. The escape-heavy case is
retained to catch that tradeoff in future changes.
