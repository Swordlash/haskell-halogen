// Write a brotli-compressed <file>.br next to each file given, and report the
// raw, gzip and brotli sizes so a change in bundle size shows up in the build
// log (and in the job summary, when run on GitHub Actions).
//
//   node toolchain/compress.mjs [--no-write] dist/main.js dist/main.css
//
// --no-write only reports, for output whose host compresses on the fly.
import { appendFileSync, readFileSync, writeFileSync } from "node:fs";
import { brotliCompressSync, constants, gzipSync } from "node:zlib";

const args = process.argv.slice(2);
const write = !args.includes("--no-write");
const files = args.filter((arg) => arg !== "--no-write");

const kib = (bytes) => `${(bytes / 1024).toFixed(1)} KiB`;

const rows = files.map((file) => {
  const raw = readFileSync(file);
  const brotli = brotliCompressSync(raw, {
    params: {
      [constants.BROTLI_PARAM_QUALITY]: constants.BROTLI_MAX_QUALITY,
      [constants.BROTLI_PARAM_SIZE_HINT]: raw.length,
    },
  });
  if (write) writeFileSync(`${file}.br`, brotli);
  const gzip = gzipSync(raw, { level: 9 });
  return { file, raw: raw.length, gzip: gzip.length, brotli: brotli.length };
});

for (const { file, raw, gzip, brotli } of rows) {
  console.log(
    `${file}\t${kib(raw)}\tgzip ${kib(gzip)}\tbrotli ${kib(brotli)}`,
  );
}

if (process.env.GITHUB_STEP_SUMMARY) {
  appendFileSync(
    process.env.GITHUB_STEP_SUMMARY,
    [
      "| File | Size | Gzip | Brotli |",
      "| --- | ---: | ---: | ---: |",
      ...rows.map(
        ({ file, raw, gzip, brotli }) =>
          `| \`${file}\` | ${kib(raw)} | ${kib(gzip)} | ${kib(brotli)} |`,
      ),
      "",
    ].join("\n"),
  );
}
