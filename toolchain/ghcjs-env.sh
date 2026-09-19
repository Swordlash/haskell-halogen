# Resolve the GHC JavaScript toolchain named by cabal-ghcjs.project.
#
# Cabal validates whatever ghc-pkg it finds on PATH against the selected
# compiler and rejects a minor-version difference, so a host GHC from another
# series (9.14 against the pinned 9.12 ghcjs) fails every build with "Version
# mismatch between ghc and ghc-pkg". It only fires when the build plan has to
# be re-resolved, which is what makes it look intermittent: a cached plan
# never re-checks. with-hc-pkg in the project file is not consulted in time to
# prevent it, but --with-compiler on the command line is.
#
# test-wasm.sh has passed these since it was written, which is why only the
# ghcjs scripts were exposed.
#
# Sourced, not executed: callers must already be at the repo root.

ghcjs_ghc_name=$(sed -n 's/^with-compiler:[[:space:]]*//p' cabal-ghcjs.project)
ghcjs_ghc=$(command -v "$ghcjs_ghc_name")
ghcjs_ghc_pkg=$(command -v "$(printf '%s' "$ghcjs_ghc_name" | sed 's/-ghc-/-ghc-pkg-/')")
