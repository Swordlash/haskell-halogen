// What the page (index.js) and its worker script (worker.js) both do to
// start the compiler: fetch an archive the manifest lists, extract it with
// bsdtar.wasm into browser_wasi_shim's in-memory file system, and load
// libplayground.so with GHC's wasm dynamic linker. Nothing here touches the
// page's document, so it runs in a Web Worker as it does on the page.
//
// Addresses are resolved against `base`, the directory of rootfs.json: the
// page's own address on the page, and this module's in a worker, which a
// Blob started and so has no address of its own to resolve against.
import { ConsoleStdout, File, OpenFile, PreopenDirectory, WASI } from "./browser_wasi_shim.js";
import { DyLDBrowserHost, main } from "./dyld.mjs";

export { Directory, File } from "./browser_wasi_shim.js";

// An archive as the manifest lists it: its parts, fetched in order and
// joined (an archive larger than a file served may be is split), with the
// bytes so far against the total, for a progress line. The first manifest had
// only `archive`.
export async function fetchArchive(base, entry, progress) {
  const parts = entry.parts ?? [{ path: entry.archive, size: entry.compressedSize ?? 0 }];
  const total = entry.compressedSize ?? parts.reduce((sum, part) => sum + part.size, 0);
  const chunks = [];
  let received = 0;
  for (const part of parts) {
    const response = await fetch(new URL(part.path, base));
    if (!response.ok) throw new Error(`${part.path}: ${response.status}`);
    const reader = response.body.getReader();
    for (;;) {
      const { done, value } = await reader.read();
      if (done) break;
      chunks.push(value);
      received += value.byteLength;
      progress?.(received, total);
    }
  }
  const bytes = new Uint8Array(received);
  let offset = 0;
  for (const chunk of chunks) {
    bytes.set(chunk, offset);
    offset += chunk.byteLength;
  }
  return bytes;
}

// bsdtar -x reads the archive from stdin and extracts it into the directory
// it is given as its working directory: the root, each layer's on top of the
// base one's.
export async function extract(bsdtar, rootfs, bytes, what) {
  const wasi = new WASI(
    ["bsdtar.wasm", "-x"],
    [],
    [
      new OpenFile(new File(bytes, { readonly: true })),
      ConsoleStdout.lineBuffered((msg) => console.info(`bsdtar: ${msg}`)),
      ConsoleStdout.lineBuffered((msg) => console.warn(`bsdtar: ${msg}`)),
      rootfs,
    ],
    { debug: false },
  );
  const instance = await WebAssembly.instantiate(bsdtar, { wasi_snapshot_preview1: wasi.wasiImport });
  const code = wasi.start(instance);
  if (code !== 0) throw new Error(`bsdtar exited with ${code} on ${what}`);
}

export function emptyRoot() {
  return new PreopenDirectory("/", []);
}

// Load libplayground.so from the extracted root, with these RTS options and
// these line sinks for what GHC and the programs it runs write, and start a
// session: the dyld instance and the session's run function.
export async function startPlayground(config, rootfs, { rts, stdout, stderr }) {
  const dyld = await main({
    rpc: new DyLDBrowserHost({ rootfs, stdout, stderr }),
    searchDirs: config.searchDirs,
    mainSoPath: config.mainSoPath,
    args: ["libplayground.so", "+RTS", ...rts, "-RTS"],
    isIserv: false,
  });
  const run = await dyld.exportFuncs.playground(config.libdir);
  return { dyld, run };
}
