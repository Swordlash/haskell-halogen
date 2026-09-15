#!/bin/sh
# Build every example in examples/ and generate the index page that links them.
# This is what the GitHub Pages deploy uploads.
set -eu

cd "$(dirname "$0")/.."

public_dir=dist-newstyle/wasm/public

# Start clean so a renamed or removed example cannot leave stale files behind
# in what the Pages deploy uploads.
rm -rf "$public_dir"
mkdir -p "$public_dir"

examples=$(find examples -mindepth 1 -maxdepth 1 -type d -exec basename {} \; | sort)

for example in $examples; do
  printf '\n=== %s ===\n' "$example"
  sh toolchain/build-wasm.sh "$example"
done

{
  cat <<'EOF'
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>haskell-halogen examples</title>
    <style>
      :root { color-scheme: light dark; }
      body { margin: 0; padding: 3rem 1.5rem; font: 16px/1.6 system-ui, sans-serif; }
      main { max-width: 42rem; margin: 0 auto; }
      h1 { font-size: 1.6rem; margin-bottom: .25rem; }
      p { opacity: .75; margin-top: 0; }
      ul { list-style: none; padding: 0; }
      li { margin: .5rem 0; }
      a { display: block; padding: .9rem 1.1rem; border: 1px solid currentColor;
          border-radius: .5rem; text-decoration: none; color: inherit; }
      a:hover { background: rgba(127,127,127,.12); }
    </style>
  </head>
  <body>
    <main>
      <h1>haskell-halogen examples</h1>
      <p>Compiled to WebAssembly with the GHC wasm backend.</p>
      <ul>
EOF
  for example in $examples; do
    printf '        <li><a href="./%s/">%s</a></li>\n' "$example" "$example"
  done
  cat <<'EOF'
      </ul>
    </main>
  </body>
</html>
EOF
} > "$public_dir/index.html"

printf '\nAll examples built in %s.\n' "$public_dir"
