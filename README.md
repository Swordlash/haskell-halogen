# haskell-halogen

A port of [purescript-halogen](https://github.com/purescript-halogen/purescript-halogen/) library to GHC Haskell.

## Running the project

While the library itself compiles under any GHC (tested with 9.6-9.12) to compile the example app you need a `javascript-unknown-ghcjs-ghc-9.12.1`, cross-compiled GHC executable capable of compiling sources to JavaScript (9.10 works as well).

The easiest way to get it is to use `ghcup` precompiled binaries from [here](https://www.haskell.org/ghcup/guide/#cross-support), then run

```
./run_dev_server_minify.sh
```

to compile the JS sources and serve them on localhost. 

You may need to run `npm i -g http-server` if you don't have the `http-server` already installed.
