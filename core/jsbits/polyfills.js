// Functions GHC's JavaScript runtime does not provide, which libraries call
// through `foreign import ccall` and the JavaScript backend then looks for as
// `h$<name>`: hspec, for one, reaches them through directory and unix
// (looking up the home directory for its config files) and through QuickCheck
// and splitmix (seeding its generator).
//
// They are js-sources of haskell-halogen-core, so every program that links
// the library gets them, a test suite in a package that depends on it
// included. Each is written for a page as well as for Node: a page has no
// `process`, no file system and no users, and says so the way the C function
// would. POLYFILLS.md at the root of the repository lists them, and which
// project each belongs in.

function h$readlink(path, pathOffset, buffer, bufferOffset, bufferSize) {
  if (!h$isNode()) {
    return h$unsupported(-1);
  }

  try {
    const target = h$encodeUtf8(
      h$fs.readlinkSync(h$decodeUtf8z(path, pathOffset)),
    );
    const length = Math.min(target.len, bufferSize);
    h$copyMutableByteArray(target, 0, buffer, bufferOffset, length);
    return length;
  } catch (error) {
    h$setErrno(error);
    return -1;
  }
}

function h$geteuid() {
  return typeof process !== "undefined" && typeof process.geteuid === "function" ? process.geteuid() : 0;
}

function h$getpwuid_r(
  _uid,
  _password,
  _passwordOffset,
  _buffer,
  _bufferOffset,
  _bufferSize,
  result,
  resultOffset,
) {
  // No getpwuid_r anywhere here: report "no matching entry" with the
  // successful-return, null-result convention. directory takes the home
  // directory from the environment first, and needs this only as a fallback.
  if (!result.arr) {
    result.arr = [];
  }
  result.arr[resultOffset] = null;
  result.dv.setInt32(resultOffset, 0, true);
  return 0;
}

function h$sysconf(_name) {
  // A conservative buffer size for getpwuid_r(_SC_GETPW_R_SIZE_MAX).
  return 16384;
}

function h$splitmix_init() {
  const seed = new Uint32Array(2);
  globalThis.crypto.getRandomValues(seed);
  h$ret1 = seed[1];
  return seed[0];
}

function h$realloc(buffer, bufferOffset, size) {
  const resized = h$newByteArray(size);
  if (buffer !== null) {
    const available = Math.max(0, buffer.len - bufferOffset);
    resized.u8.set(buffer.u8.subarray(bufferOffset, bufferOffset + Math.min(available, size)));
  }
  h$ret1 = 0;
  return resized;
}
