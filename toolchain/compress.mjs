// Write a brotli-compressed <file>.br next to each file given, and report the
// raw and compressed sizes so a change in bundle size shows up in the build
// log (and in the job summary, when run on GitHub Actions).
//
//   node toolchain/compress.mjs dist/main.js dist/main.css
import { appendFileSync, readFileSync, writeFileSync } from "node:fs";
import { brotliCompressSync, constants } from "node:zlib";

const kib = (bytes) => `${(bytes / 1024).toFixed(1)} KiB`;

const rows = process.argv.slice(2).map((file) => {
  const raw = readFileSync(file);
  const compressed = brotliCompressSync(raw, {
    params: {
      [constants.BROTLI_PARAM_QUALITY]: constants.BROTLI_MAX_QUALITY,
      [constants.BROTLI_PARAM_SIZE_HINT]: raw.length,
    },
  });
  writeFileSync(`${file}.br`, compressed);
  return { file, raw: raw.length, compressed: compressed.length };
});

for (const { file, raw, compressed } of rows) {
  console.log(`${file}\t${kib(raw)}\tbrotli ${kib(compressed)}`);
}

if (process.env.GITHUB_STEP_SUMMARY) {
  appendFileSync(
    process.env.GITHUB_STEP_SUMMARY,
    [
      "| File | Size | Brotli |",
      "| --- | ---: | ---: |",
      ...rows.map(
        ({ file, raw, compressed }) =>
          `| \`${file}\` | ${kib(raw)} | ${kib(compressed)} |`,
      ),
      "",
    ].join("\n"),
  );
}
