// Compatibility functions missing from GHC's JavaScript runtime. Hspec pulls
// these symbols in through directory/unix and QuickCheck/splitmix.

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
  return typeof process.geteuid === "function" ? process.geteuid() : 0;
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
  // Node has no getpwuid_r equivalent. Report "no matching entry" using the
  // successful-return/null-result convention; directory normally obtains the
  // home directory from the environment and does not need this fallback.
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
