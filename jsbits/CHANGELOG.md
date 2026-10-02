# Changelog

## Unreleased

Embedded scripts are separated with explicit semicolons so expression boundaries
retain their meaning across files. Package metadata declares Apache-2.0, matching
the bundled license and the other Halogen packages.

A compile-time bridge embeds shared jsbits into WebAssembly FFI, keeping browser
logic in one source across backends without requiring applications to load extra
scripts. Sources are tracked for recompilation and initialization is retained
once per splice. WASM consumers must enable shared libraries for the compiler's
Template Haskell interpreter.
