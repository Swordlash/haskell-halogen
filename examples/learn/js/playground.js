// The compiler: GHC built to WebAssembly, run in the page. The rootfs
// (toolchain/build-learn-rootfs.sh, explained in learn/README.md) is an
// archive of GHC's libdir, our libraries and libplayground.so
// (learn/Playground.hs); this module downloads it (through the Cache API, so
// a second visit does not), extracts it with bsdtar.wasm into
// browser_wasi_shim's in-memory file system, loads libplayground.so with GHC's
// dynamic linker (dyld.mjs) and hands back its run function.
//
// Everything is relative to the rootfs directory the caller names, which
// holds rootfs.json, rootfs.tar.zst, layers/, dyld.mjs, browser_wasi_shim.js
// and bsdtar.wasm.
//
// browser_wasi_shim is imported from the rootfs' browser_wasi_shim.js, the
// file the rootfs' copy of dyld.mjs imports, so that the page and dyld share
// one instance of the module (dyld checks the file system's objects against
// its own classes). The rootfs build copies it there from learn/vendor/ and
// points dyld at it, so nothing the compiler needs comes from another site.

const cacheName = "halogen-learn-rootfs";

// What every compile passes to GHC before the lesson's own arguments.
// -Wno-missed-extra-shared-lib: the RTS's package names libdl, which wasi has
// no shared library for and nothing needs, and the interpreter warns that it
// could not load it on every run. -i/tmp: Main imports Lesson, and both are
// written to /tmp.
const baseArgs = ["-package", "haskell-halogen-core", "-Wno-missed-extra-shared-lib", "-i/tmp"];

// The Main that ends whatever the last run mounted, for when the page takes
// the result's element away: see endRun.
const endMain = `module Main (main) where

import Halogen.IO.Util (endPreviousRun)

main :: IO ()
main = endPreviousRun
`;

const manifests = new Map();
let compiler = null; // { shim, rootfs, run, manifest, layers: Map<name, Promise>, failed: Map<name, Error>, bsdtar }
let starting = null;
let progressSink = () => {};
let outputSink = null;
let queue = Promise.resolve();
let loadedBytes = 0;

// rootfs.json (learn/README.md describes it), with what the page adds to
// each archive: its address (the name its parts join to, which the cache
// keeps it under), the addresses of its parts, what it downloads, and the
// version it is cached under. The manifest's own fields are used when it has
// them: `version` at the top, a hash of every archive, and each archive's
// `parts` and `compressedSize`. A manifest without them (an older build, or
// one written by hand) gets them from a HEAD request per part instead: its
// Content-Length, and the ETag or Last-Modified the server sends, which change
// whenever the file does.
export function manifest(root) {
  const base = new URL(root, document.baseURI).href;
  if (!manifests.has(base)) {
    const load = (async () => {
      const response = await fetch(new URL("rootfs.json", base));
      if (!response.ok) throw new Error(`rootfs.json: ${response.status} ${response.statusText}`);
      const json = await response.json();
      const describe = async (name, entry) => {
        const parts = (entry.parts ?? [{ path: entry.archive }]).map((part) => ({
          url: new URL(part.path, base).href,
          size: part.size ?? null,
        }));
        const sum = (sizes) => (sizes.every((size) => size != null) ? sizes.reduce((a, b) => a + b, 0) : null);
        let size = entry.compressedSize ?? sum(parts.map((part) => part.size));
        let version = json.version ?? null;
        if (size == null || version == null) {
          try {
            const heads = await Promise.all(parts.map((part) => fetch(part.url, { method: "HEAD" })));
            size ??= sum(heads.map((head) => Number(head.headers.get("content-length")) || null));
            const tags = heads.map((head) => head.headers.get("etag") ?? head.headers.get("last-modified"));
            version ??= tags.every(Boolean) ? tags.join(" ") : null;
          } catch {
            // The download says what is wrong, if anything is.
          }
        }
        return { name, url: new URL(entry.archive, base).href, parts, size, version, versioned: json.version != null, base };
      };
      // The first manifests had the base archive's name at the top, if at all.
      const rootfs = json.rootfs ?? { archive: json.archive ?? "rootfs.tar.zst", compressedSize: json.compressedSize };
      const [archive, ...layers] = await Promise.all([
        describe("base", rootfs),
        ...json.layers.map((layer) => describe(layer.name, layer)),
      ]);
      return {
        ...json,
        base,
        archive,
        layers: json.layers.map((layer, i) => ({ ...layer, ...layers[i] })),
      };
    })();
    // A failure is not kept: Retry asks again.
    load.catch(() => manifests.delete(base));
    manifests.set(base, load);
  }
  return manifests.get(base);
}

// The manifest as the page shows it: the GHC version and what each archive
// downloads.
export async function describe(root) {
  const m = await manifest(root);
  return {
    ghc: m.ghc,
    base: m.archive.size,
    layers: m.layers.map((layer) => ({ name: layer.name, size: layer.size })),
  };
}

function report(progress) {
  try {
    progressSink(progress);
  } catch (error) {
    console.error("learn: progress", error);
  }
}

// Each download's progress, added up, so that the bar is one bar however many
// archives are on their way.
const downloads = new Map();
function downloaded(name, loaded, total) {
  downloads.set(name, { loaded, total });
  let sum = 0;
  let of = 0;
  let known = true;
  for (const d of downloads.values()) {
    sum += d.loaded;
    if (d.total == null) known = false;
    else of += d.total;
  }
  report({ phase: "download", loaded: sum, total: known ? of : null, mb: (loadedBytes + sum) / 1e6 });
}

async function openCache() {
  try {
    return await caches.open(cacheName);
  } catch {
    // No Cache API (an insecure origin, a private window that refuses it):
    // every visit downloads again.
    return null;
  }
}

// An archive's bytes: from the cache when it holds this version, else from
// the network, its parts one after the other and joined, with its progress,
// and then into the cache in place of any other version of it. When the
// version is the manifest's, which is one for every archive, whatever the
// cache holds of the same rootfs under another version goes too, archives no
// longer in the manifest included.
//
// Reading the cache is a phase of its own ("cache"), not a download: it is
// reported when nothing is on its way from the network, whose progress the
// bar shows otherwise, and the bytes read from it are not counted in that
// progress. A download that fails takes its progress out of the sum, so that
// the next one's total is its own.
async function fetchArchive(archive) {
  const cache = archive.version ? await openCache() : null;
  const key = new URL(`${archive.url}?v=${encodeURIComponent(archive.version)}`).href;
  if (cache) {
    const hit = await cache.match(key).catch(() => undefined);
    if (hit) {
      if (downloads.size === 0) report({ phase: "cache", mb: totalMb() });
      const bytes = new Uint8Array(await hit.arrayBuffer());
      return { bytes, cached: true };
    }
  }
  try {
    return await download(archive, cache, key);
  } catch (error) {
    downloads.delete(archive.name);
    throw error;
  }
}

async function download(archive, cache, key) {
  let total = archive.size;
  const chunks = [];
  let loaded = 0;
  for (const part of archive.parts) {
    const response = await fetch(part.url);
    if (!response.ok) throw new Error(`${part.url}: ${response.status} ${response.statusText}`);
    if (archive.size == null && archive.parts.length === 1) total = Number(response.headers.get("content-length")) || null;
    const reader = response.body.getReader();
    for (;;) {
      const { done, value } = await reader.read();
      if (done) break;
      chunks.push(value);
      loaded += value.byteLength;
      // A server that compresses on the fly sends the compressed length, and
      // the body arrives uncompressed: then the total says nothing.
      if (total != null && loaded > total) total = null;
      downloaded(archive.name, loaded, total);
    }
  }
  const bytes = new Uint8Array(loaded);
  let at = 0;
  for (const chunk of chunks) {
    bytes.set(chunk, at);
    at += chunk.byteLength;
  }
  if (cache) {
    try {
      await cache.put(key, new Response(bytes, { headers: { "content-type": "application/octet-stream" } }));
      const version = `?v=${encodeURIComponent(archive.version)}`;
      for (const request of await cache.keys()) {
        if (request.url === key) continue;
        const sameArchive = request.url.startsWith(`${archive.url}?v=`);
        const otherVersion = archive.versioned && request.url.startsWith(archive.base) && !request.url.endsWith(version);
        if (sameArchive || otherVersion) await cache.delete(request);
      }
    } catch (error) {
      // A full quota: the next visit downloads again.
      console.warn("learn: could not keep the archive in the cache:", error);
    }
  }
  return { bytes, cached: false };
}

// What a layer's packages need on the page besides their libraries (the
// manifest's `assets`: material's material.js, which its bindings call, and
// material.css), added before any compile can use the layer, and waited for.
function addAssets(m, layer) {
  return Promise.all(
    (layer.assets ?? []).map(
      (asset) =>
        new Promise((resolve, reject) => {
          const href = new URL(asset.path, m.base).href;
          const element =
            asset.kind === "stylesheet"
              ? Object.assign(document.createElement("link"), { rel: "stylesheet", href })
              : Object.assign(document.createElement("script"), { src: href });
          element.addEventListener("load", resolve);
          element.addEventListener("error", () => reject(new Error(`${href} did not load`)));
          document.head.append(element);
        }),
    ),
  );
}

// bsdtar -x reads the archive from stdin and extracts it into the directory
// it is given as its working directory: the root.
async function extract(shim, bsdtar, rootfs, bytes, name) {
  const wasi = new shim.WASI(
    ["bsdtar.wasm", "-x"],
    [],
    [
      new shim.OpenFile(new shim.File(bytes, { readonly: true })),
      shim.ConsoleStdout.lineBuffered((msg) => console.info(`bsdtar: ${msg}`)),
      shim.ConsoleStdout.lineBuffered((msg) => console.warn(`bsdtar: ${msg}`)),
      rootfs,
    ],
    { debug: false },
  );
  const instance = await WebAssembly.instantiate(bsdtar, { wasi_snapshot_preview1: wasi.wasiImport });
  const code = wasi.start(instance);
  if (code !== 0) throw new Error(`bsdtar exited with ${code} on ${name}`);
}

// Download, extract and start the compiler, and the layers named alongside;
// report what it is doing to onProgress, here and in every later
// ensureLayers. The promise resolves once the compiler has started, and
// rejects only when the compiler itself did not: a layer that did not load
// leaves the compiler usable for every lesson that does not need it, and the
// first ensureLayers that names it rejects with why (the next one tries it
// again). Called again while it is under way, or after it has succeeded, it
// gives the same promise; after it has failed, it starts again. A failure
// lets go of onProgress, so that the caller may free it once the promise has
// rejected.
export function start(root, layers, onProgress) {
  if (onProgress) progressSink = onProgress;
  if (!starting) {
    starting = boot(root, layers).catch((error) => {
      starting = null;
      downloads.clear();
      report({ phase: "error", message: String(error?.message ?? error) });
      progressSink = () => {};
      throw error;
    });
  }
  return starting;
}

async function boot(root, wanted) {
  const timings = {};
  let t = performance.now();
  const lap = (name) => {
    const now = performance.now();
    timings[name] = Math.round(now - t);
    t = now;
  };
  report({ phase: "manifest" });
  const m = await manifest(root);
  const shim = await import(new URL("browser_wasi_shim.js", m.base).href);
  const layers = m.layers.filter((layer) => wanted.includes(layer.name));
  // A layer's failure is kept for ensureLayers to report, not the boot's.
  const settled = (promise) => promise.then((archive) => ({ archive }), (error) => ({ error }));
  const [bsdtar, base, ...extra] = await Promise.all([
    WebAssembly.compileStreaming(fetch(new URL("bsdtar.wasm", m.base))),
    fetchArchive(m.archive),
    ...layers.map((layer) => settled(fetchArchive(layer))),
  ]);
  lap("fetch");
  const rootfs = new shim.PreopenDirectory("/", []);
  report({ phase: "extract", mb: totalMb() });
  await nextFrame();
  await extract(shim, bsdtar, rootfs, base.bytes, m.archive.url);
  loadedBytes += base.bytes.byteLength;
  const loaded = [];
  const failed = new Map();
  for (const [i, layer] of layers.entries()) {
    try {
      if (extra[i].error) throw extra[i].error;
      await extract(shim, bsdtar, rootfs, extra[i].archive.bytes, layer.url);
      await addAssets(m, layer);
      loadedBytes += extra[i].archive.bytes.byteLength;
      loaded.push(layer.name);
    } catch (error) {
      console.warn(`learn: the layer ${layer.name} did not load:`, error);
      failed.set(layer.name, error);
    }
  }
  lap("extract");
  downloads.clear();
  report({ phase: "start", mb: totalMb() });
  await nextFrame();
  const { DyLDBrowserHost, main } = await import(new URL("dyld.mjs", m.base).href);
  const line = (stream) => (msg) => {
    if (outputSink) outputSink[stream].push(msg);
    else console.info(`ghc: ${msg}`);
  };
  const dyld = await main({
    rpc: new DyLDBrowserHost({ rootfs, stdout: line("stdout"), stderr: line("stderr") }),
    searchDirs: m.searchDirs,
    mainSoPath: m.mainSoPath,
    // +RTS -c: compacting collection of the old generation, which keeps
    // GHC's heap smaller after a few runs (learn/README.md).
    args: ["libplayground.so", "+RTS", "-c", "-RTS"],
    isIserv: false,
  });
  // The compiled programs run in this dyld, and Halogen.Lazy.Dyld links
  // further libraries into them through it (lesson 18's demo library): it
  // fetches a library, writes it into this file system with this shim's
  // classes, and has dyld load it.
  globalThis.halogenDyld = { dyld, rootfs, shim };
  const run = await dyld.exportFuncs.playground(m.libdir);
  lap("start");
  compiler = { shim, rootfs, run, manifest: m, layers: new Map(loaded.map((name) => [name, Promise.resolve()])), failed, bsdtar };
  console.info(`learn: compiler ready (${JSON.stringify(timings)})`);
  report({ phase: "ready", mb: totalMb(), timings });
}

function totalMb() {
  return loadedBytes / 1e6;
}

// Give the page time to show the phase before a long synchronous step: the
// app hears of it through its runtime's scheduler, which needs a few tasks
// of its own to render it.
function nextFrame() {
  return new Promise((resolve) => setTimeout(resolve, 50));
}

// Extract the layers named that are not extracted yet. Resolves when all of
// them are; reports the download as start does. A layer that does not load
// rejects the promise, with why, and is tried again by the next call that
// names it (one that failed while the compiler started is reported here
// first, and tried again after that); the compiler stays ready, and is
// reported so, for the lessons that do not need it.
export async function ensureLayers(names) {
  if (!compiler) throw new Error("the compiler has not started");
  const c = compiler;
  const missing = names.filter((name) => !c.layers.has(name));
  for (const name of missing) {
    const layer = c.manifest.layers.find((l) => l.name === name);
    if (!layer) throw new Error(`no layer named ${name} in rootfs.json`);
    const failure = c.failed.get(name);
    c.failed.delete(name);
    c.layers.set(
      name,
      (async () => {
        if (failure) throw failure;
        const { bytes } = await fetchArchive(layer);
        await extract(c.shim, c.bsdtar, c.rootfs, bytes, layer.url);
        await addAssets(c.manifest, layer);
        loadedBytes += bytes.byteLength;
        downloads.delete(layer.name);
      })().catch((error) => {
        c.layers.delete(name);
        throw error;
      }),
    );
  }
  if (missing.length) report({ phase: "layers" });
  try {
    await Promise.all(names.map((name) => c.layers.get(name)));
  } finally {
    if (missing.length) report({ phase: "ready", mb: totalMb() });
  }
}

function writeFile(path, text) {
  const parts = path.split("/").filter(Boolean);
  const name = parts.pop();
  let dir = compiler.rootfs.dir;
  for (const part of parts) {
    let next = dir.contents.get(part);
    if (!next) {
      next = new compiler.shim.Directory([]);
      dir.contents.set(part, next);
    }
    dir = next;
  }
  dir.contents.set(name, new compiler.shim.File(new TextEncoder().encode(text)));
}

// One run at a time: the GHC session is not safe to use from two at once.
function enqueue(job) {
  const next = queue.then(job, job);
  queue = next.catch(() => {});
  return next;
}

// Compile and run: request is { lesson, main, args, layers }, the sources of
// /tmp/Lesson.hs and /tmp/Main.hs, the lesson's own GHC arguments and the
// layers it uses (which must have been ensured). Resolves, never rejects,
// with { ok, compiled, error, stdout, stderr, ms }: ok when main ran to the
// end, compiled when GHC got as far as running it.
export function compile(request) {
  return enqueue(async () => {
    if (!compiler) throw new Error("the compiler has not started");
    const dbs = request.layers.flatMap((name) => {
      const layer = compiler.manifest.layers.find((l) => l.name === name);
      return layer ? ["-package-db", layer.packageDb] : [];
    });
    writeFile("/tmp/Lesson.hs", request.lesson);
    const output = { stdout: [], stderr: [] };
    outputSink = output;
    const t = performance.now();
    let ok = true;
    let error = null;
    try {
      await compiler.run([...baseArgs, ...dbs, ...request.args].join(" "), request.main);
    } catch (e) {
      ok = false;
      error = String(e?.message ?? e);
    } finally {
      outputSink = null;
    }
    const ms = Math.round(performance.now() - t);
    const stderr = output.stderr.join("\n");
    // Playground.hs fails with this when GHC reports errors, and GHC's
    // error handler prints it and exits; anything else was the program's
    // own main failing.
    const compiled = ok || !stderr.includes("the program did not compile");
    // GHC's progress ("[1 of 2] Compiling Lesson") goes to stdout, before
    // what the program writes there.
    const stdout = output.stdout.filter((l) => !/^\[\s*\d+ of \d+\] Compiling /.test(l)).join("\n");
    // What the program died of is what GHC's error handler printed last;
    // the exception itself is only the exit code.
    const last = output.stderr.filter((l) => l.trim() !== "").pop();
    return { ok, compiled, error: ok ? null : (last ?? error), stdout, stderr, ms };
  });
}

// End the last run's tree, before the page takes away the element it is
// mounted in: its subscriptions, forks and timers would go on otherwise,
// against an element no longer on the page. The tree lives in the
// compiler's Haskell, and what ends it is the reference
// Halogen.IO.Util.keepForNextRun kept there, which only Haskell in that
// session can reach; so this runs a Main of its own whose main is
// endPreviousRun. It costs a compile of that small module.
export function endRun() {
  if (!compiler) return Promise.resolve();
  return enqueue(async () => {
    outputSink = { stdout: [], stderr: [] };
    try {
      await compiler.run(baseArgs.join(" "), endMain);
    } catch (error) {
      console.warn("learn: ending the last run failed:", error);
    } finally {
      outputSink = null;
    }
  });
}

export function ready() {
  return compiler !== null;
}
