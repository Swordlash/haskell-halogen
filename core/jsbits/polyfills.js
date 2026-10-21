// Functions GHC's JavaScript runtime does not provide, which libraries call
// through `foreign import ccall` (or `capi`) and the JavaScript backend then
// looks for as `h$<name>`: hspec, for one, reaches them through directory and
// unix (looking up the home directory for its config files) and through
// QuickCheck and splitmix (seeding its generator), and hashable hashes every
// Text and ByteString with XXH3, whose C is not compiled here.
//
// They are js-sources of haskell-halogen-core, so every program that links
// the library gets them, a test suite in a package that depends on it
// included. Each is written for a page as well as for Node: a page has no
// `process`, no file system and no users, and says so the way the C function
// would. POLYFILLS.md at the root of the repository lists them, and which
// project each belongs in.

var h$readlink = typeof h$readlink === "function" ? h$readlink : function (path, pathOffset, buffer, bufferOffset, bufferSize) {
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
};

var h$geteuid = typeof h$geteuid === "function" ? h$geteuid : function () {
  return typeof process !== "undefined" && typeof process.geteuid === "function" ? process.geteuid() : 0;
};

var h$getpwuid_r = typeof h$getpwuid_r === "function" ? h$getpwuid_r : function (
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
};

var h$sysconf = typeof h$sysconf === "function" ? h$sysconf : function (_name) {
  // A conservative buffer size for getpwuid_r(_SC_GETPW_R_SIZE_MAX).
  return 16384;
};

var h$splitmix_init = typeof h$splitmix_init === "function" ? h$splitmix_init : function () {
  const seed = new Uint32Array(2);
  globalThis.crypto.getRandomValues(seed);
  h$ret1 = seed[1];
  return seed[0];
};

var h$realloc = typeof h$realloc === "function" ? h$realloc : function (buffer, bufferOffset, size) {
  const resized = h$newByteArray(size);
  if (buffer !== null) {
    const available = Math.max(0, buffer.len - bufferOffset);
    resized.u8.set(buffer.u8.subarray(bufferOffset, bufferOffset + Math.min(available, size)));
  }
  h$ret1 = 0;
  return resized;
};

// XXH3-64, the with-seed variants, ported from xxHash 0.8.3 (the copy
// hashable 1.5.1.0 compiles natively), so that a hash is the same 64-bit
// value on the JavaScript backend as in C. xxHash is Copyright (C) 2012-2023
// Yann Collet, under the BSD 2-Clause License (whose notice core/NOTICE
// carries in full); this port is a derived work of its xxhash.h,
// https://github.com/Cyan4973/xxHash. Arithmetic is on BigInt, reduced
// to 64 bits after every operation that can overflow; hashable hashes small
// keys, for which that is fast enough.
var h$xxh3 = typeof h$xxh3 === "function" ? h$xxh3 : (function () {
  const M64 = (1n << 64n) - 1n;
  const PRIME32_1 = 0x9E3779B1n;
  const PRIME32_2 = 0x85EBCA77n;
  const PRIME32_3 = 0xC2B2AE3Dn;
  const PRIME64_1 = 0x9E3779B185EBCA87n;
  const PRIME64_2 = 0xC2B2AE3D27D4EB4Fn;
  const PRIME64_3 = 0x165667B19E3779F9n;
  const PRIME64_4 = 0x85EBCA77C2B2AE63n;
  const PRIME64_5 = 0x27D4EB2F165667C5n;
  const PRIME_MX1 = 0x165667919E3779F9n;
  const PRIME_MX2 = 0x9FB21C651E98DF25n;
  const SECRET_SIZE_MIN = 136;
  const STRIPE_LEN = 64;
  const SECRET_CONSUME_RATE = 8;
  const kSecret = new Uint8Array([
    0xb8, 0xfe, 0x6c, 0x39, 0x23, 0xa4, 0x4b, 0xbe, 0x7c, 0x01, 0x81, 0x2c, 0xf7, 0x21, 0xad, 0x1c,
    0xde, 0xd4, 0x6d, 0xe9, 0x83, 0x90, 0x97, 0xdb, 0x72, 0x40, 0xa4, 0xa4, 0xb7, 0xb3, 0x67, 0x1f,
    0xcb, 0x79, 0xe6, 0x4e, 0xcc, 0xc0, 0xe5, 0x78, 0x82, 0x5a, 0xd0, 0x7d, 0xcc, 0xff, 0x72, 0x21,
    0xb8, 0x08, 0x46, 0x74, 0xf7, 0x43, 0x24, 0x8e, 0xe0, 0x35, 0x90, 0xe6, 0x81, 0x3a, 0x26, 0x4c,
    0x3c, 0x28, 0x52, 0xbb, 0x91, 0xc3, 0x00, 0xcb, 0x88, 0xd0, 0x65, 0x8b, 0x1b, 0x53, 0x2e, 0xa3,
    0x71, 0x64, 0x48, 0x97, 0xa2, 0x0d, 0xf9, 0x4e, 0x38, 0x19, 0xef, 0x46, 0xa9, 0xde, 0xac, 0xd8,
    0xa8, 0xfa, 0x76, 0x3f, 0xe3, 0x9c, 0x34, 0x3f, 0xf9, 0xdc, 0xbb, 0xc7, 0xc7, 0x0b, 0x4f, 0x1d,
    0x8a, 0x51, 0xe0, 0x4b, 0xcd, 0xb4, 0x59, 0x31, 0xc8, 0x9f, 0x7e, 0xc9, 0xd9, 0x78, 0x73, 0x64,
    0xea, 0xc5, 0xac, 0x83, 0x34, 0xd3, 0xeb, 0xc3, 0xc5, 0x81, 0xa0, 0xff, 0xfa, 0x13, 0x63, 0xeb,
    0x17, 0x0d, 0xdd, 0x51, 0xb7, 0xf0, 0xda, 0x49, 0xd3, 0x16, 0x55, 0x26, 0x29, 0xd4, 0x68, 0x9e,
    0x2b, 0x16, 0xbe, 0x58, 0x7d, 0x47, 0xa1, 0xfc, 0x8f, 0xf8, 0xb8, 0xd1, 0x7a, 0xd0, 0x31, 0xce,
    0x45, 0xcb, 0x3a, 0x8f, 0x95, 0x16, 0x04, 0x28, 0xaf, 0xd7, 0xfb, 0xca, 0xbb, 0x4b, 0x40, 0x7e,
  ]);

  const view = (bytes) => new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  const read64 = (dv, at) => dv.getBigUint64(at, true);
  const read32 = (dv, at) => BigInt(dv.getUint32(at, true));
  const add = (a, b) => (a + b) & M64;
  const sub = (a, b) => (a - b) & M64;
  const mul = (a, b) => (a * b) & M64;
  const rotl = (x, r) => ((x << BigInt(r)) | (x >> BigInt(64 - r))) & M64;
  const swap32 = (x) => {
    x &= 0xFFFFFFFFn;
    return ((x & 0xFFn) << 24n) | ((x & 0xFF00n) << 8n) | ((x >> 8n) & 0xFF00n) | (x >> 24n);
  };
  const swap64 = (x) => (swap32(x) << 32n) | swap32(x >> 32n);
  const mul128fold64 = (a, b) => {
    const product = a * b;
    return (product & M64) ^ (product >> 64n);
  };
  const xxh64Avalanche = (h) => {
    h ^= h >> 33n;
    h = mul(h, PRIME64_2);
    h ^= h >> 29n;
    h = mul(h, PRIME64_3);
    return h ^ (h >> 32n);
  };
  const avalanche = (h) => {
    h ^= h >> 37n;
    h = mul(h, PRIME_MX1);
    return h ^ (h >> 32n);
  };
  const rrmxmx = (h, len) => {
    h ^= rotl(h, 49) ^ rotl(h, 24);
    h = mul(h, PRIME_MX2);
    h ^= add(h >> 35n, BigInt(len));
    h = mul(h, PRIME_MX2);
    return h ^ (h >> 28n);
  };
  const mix16B = (input, at, secret, sat, seed) =>
    mul128fold64(
      read64(input, at) ^ add(read64(secret, sat), seed),
      read64(input, at + 8) ^ sub(read64(secret, sat + 8), seed),
    );

  function len0to16(input, len, secret, seed) {
    if (len > 8) {
      const bitflip1 = add(read64(secret, 24) ^ read64(secret, 32), seed);
      const bitflip2 = sub(read64(secret, 40) ^ read64(secret, 48), seed);
      const lo = read64(input, 0) ^ bitflip1;
      const hi = read64(input, len - 8) ^ bitflip2;
      return avalanche(add(add(add(BigInt(len), swap64(lo)), hi), mul128fold64(lo, hi)));
    }
    if (len >= 4) {
      seed ^= swap32(seed) << 32n;
      const input1 = read32(input, 0);
      const input2 = read32(input, len - 4);
      const bitflip = sub(read64(secret, 8) ^ read64(secret, 16), seed);
      return rrmxmx((input2 + (input1 << 32n)) ^ bitflip, len);
    }
    if (len > 0) {
      const c1 = BigInt(input.getUint8(0));
      const c2 = BigInt(input.getUint8(len >> 1));
      const c3 = BigInt(input.getUint8(len - 1));
      const combined = ((c1 << 16n) | (c2 << 24n) | c3 | (BigInt(len) << 8n)) & 0xFFFFFFFFn;
      const bitflip = add(read32(secret, 0) ^ read32(secret, 4), seed);
      return xxh64Avalanche(combined ^ bitflip);
    }
    return xxh64Avalanche(seed ^ read64(secret, 56) ^ read64(secret, 64));
  }

  function len17to128(input, len, secret, seed) {
    let acc = mul(BigInt(len), PRIME64_1);
    if (len > 32) {
      if (len > 64) {
        if (len > 96) {
          acc = add(acc, mix16B(input, 48, secret, 96, seed));
          acc = add(acc, mix16B(input, len - 64, secret, 112, seed));
        }
        acc = add(acc, mix16B(input, 32, secret, 64, seed));
        acc = add(acc, mix16B(input, len - 48, secret, 80, seed));
      }
      acc = add(acc, mix16B(input, 16, secret, 32, seed));
      acc = add(acc, mix16B(input, len - 32, secret, 48, seed));
    }
    acc = add(acc, mix16B(input, 0, secret, 0, seed));
    acc = add(acc, mix16B(input, len - 16, secret, 16, seed));
    return avalanche(acc);
  }

  function len129to240(input, len, secret, seed) {
    let acc = mul(BigInt(len), PRIME64_1);
    const rounds = Math.floor(len / 16);
    for (let i = 0; i < 8; i++) acc = add(acc, mix16B(input, 16 * i, secret, 16 * i, seed));
    let accEnd = mix16B(input, len - 16, secret, SECRET_SIZE_MIN - 17, seed);
    acc = avalanche(acc);
    for (let i = 8; i < rounds; i++) accEnd = add(accEnd, mix16B(input, 16 * i, secret, 16 * (i - 8) + 3, seed));
    return avalanche(add(acc, accEnd));
  }

  function accumulate512(acc, input, at, secret, sat) {
    for (let lane = 0; lane < 8; lane++) {
      const value = read64(input, at + lane * 8);
      const key = value ^ read64(secret, sat + lane * 8);
      acc[lane ^ 1] = add(acc[lane ^ 1], value);
      acc[lane] = add(acc[lane], (key & 0xFFFFFFFFn) * (key >> 32n));
    }
  }

  function scramble(acc, secret, sat) {
    for (let lane = 0; lane < 8; lane++) {
      let a = acc[lane];
      a ^= a >> 47n;
      a ^= read64(secret, sat + lane * 8);
      acc[lane] = mul(a, PRIME32_1);
    }
  }

  function hashLong(input, len, secret, secretSize) {
    const acc = [PRIME32_3, PRIME64_1, PRIME64_2, PRIME64_3, PRIME64_4, PRIME32_2, PRIME64_5, PRIME32_1];
    const stripesPerBlock = (secretSize - STRIPE_LEN) / SECRET_CONSUME_RATE;
    const blockLen = STRIPE_LEN * stripesPerBlock;
    const blocks = Math.floor((len - 1) / blockLen);
    for (let n = 0; n < blocks; n++) {
      for (let s = 0; s < stripesPerBlock; s++) {
        accumulate512(acc, input, n * blockLen + s * STRIPE_LEN, secret, s * SECRET_CONSUME_RATE);
      }
      scramble(acc, secret, secretSize - STRIPE_LEN);
    }
    const stripes = Math.floor((len - 1 - blockLen * blocks) / STRIPE_LEN);
    for (let s = 0; s < stripes; s++) {
      accumulate512(acc, input, blocks * blockLen + s * STRIPE_LEN, secret, s * SECRET_CONSUME_RATE);
    }
    accumulate512(acc, input, len - STRIPE_LEN, secret, secretSize - STRIPE_LEN - 7);
    let result = mul(BigInt(len), PRIME64_1);
    for (let i = 0; i < 4; i++) {
      result = add(result, mul128fold64(acc[2 * i] ^ read64(secret, 11 + 16 * i), acc[2 * i + 1] ^ read64(secret, 11 + 16 * i + 8)));
    }
    return avalanche(result);
  }

  // The default secret with the seed folded in, as XXH3_initCustomSecret.
  function customSecret(seed) {
    const secret = new Uint8Array(kSecret.length);
    const from = view(kSecret);
    const to = view(secret);
    for (let i = 0; i < kSecret.length; i += 16) {
      to.setBigUint64(i, add(read64(from, i), seed), true);
      to.setBigUint64(i + 8, sub(read64(from, i + 8), seed), true);
    }
    return secret;
  }

  const defaultSecret = view(kSecret);

  // XXH3_64bits_withSeed over bytes (a Uint8Array) and a BigInt seed.
  return function (bytes, seed) {
    const input = view(bytes);
    const len = bytes.length;
    if (len <= 16) return len0to16(input, len, defaultSecret, seed);
    if (len <= 128) return len17to128(input, len, defaultSecret, seed);
    if (len <= 240) return len129to240(input, len, defaultSecret, seed);
    const secret = seed === 0n ? kSecret : customSecret(seed);
    return hashLong(input, len, view(secret), secret.length);
  };
})();

// hashable's foreign imports of XXH3 (Data.Hashable.FFI). The JavaScript
// backend passes a Word64 as two unsigned 32-bit halves, high first, and
// takes one back as the high half returned with the low half in h$ret1; a
// CSize is one number (the target's word is 32 bits); and an Addr#, a
// ByteArray# and a MutableByteArray# each arrive as a byte array and an
// offset (always 0 for the two arrays, whose offsets are hashable's own
// arguments).
function h$xxh3Word64(high, low) {
  return (BigInt(high >>> 0) << 32n) | BigInt(low >>> 0);
}

function h$xxh3Return(value) {
  h$ret1 = Number(value & 0xFFFFFFFFn) >>> 0;
  return Number(value >> 32n) >>> 0;
}

// A Word64 or Word32 hashed as its little-endian bytes, as the C hashes the
// value in memory.
function h$xxh3Bytes(value, width) {
  const bytes = new Uint8Array(width);
  const view = new DataView(bytes.buffer);
  if (width === 8) view.setBigUint64(0, value, true);
  else view.setUint32(0, Number(value), true);
  return bytes;
}

var h$XXH3_64bits_withSeed = typeof h$XXH3_64bits_withSeed === "function" ? h$XXH3_64bits_withSeed : function (buffer, offset, length, seedHigh, seedLow) {
  return h$xxh3Return(h$xxh3(buffer.u8.subarray(offset, offset + length), h$xxh3Word64(seedHigh, seedLow)));
};

var h$hs_XXH3_64bits_withSeed_offset = typeof h$hs_XXH3_64bits_withSeed_offset === "function" ? h$hs_XXH3_64bits_withSeed_offset : function (array, _arrayOffset, offset, length, seedHigh, seedLow) {
  return h$xxh3Return(h$xxh3(array.u8.subarray(offset, offset + length), h$xxh3Word64(seedHigh, seedLow)));
};

var h$hs_XXH3_64bits_withSeed_u64 = typeof h$hs_XXH3_64bits_withSeed_u64 === "function" ? h$hs_XXH3_64bits_withSeed_u64 : function (high, low, seedHigh, seedLow) {
  return h$xxh3Return(h$xxh3(h$xxh3Bytes(h$xxh3Word64(high, low), 8), h$xxh3Word64(seedHigh, seedLow)));
};

var h$hs_XXH3_64bits_withSeed_u32 = typeof h$hs_XXH3_64bits_withSeed_u32 === "function" ? h$hs_XXH3_64bits_withSeed_u32 : function (value, seedHigh, seedLow) {
  return h$xxh3Return(h$xxh3(h$xxh3Bytes(BigInt(value >>> 0), 4), h$xxh3Word64(seedHigh, seedLow)));
};

// The incremental API. XXH3 guarantees that a stream hashes to what the
// one-shot function makes of the same bytes with the same seed, so the state
// keeps the seed and the bytes so far, on the JavaScript object of the
// MutableByteArray# hashable allocates for it, and the digest is the one-shot
// hash of them (and leaves the state as it was, as the C digest does).
// hashable streams only lazy ByteStrings and Texts, chunk by chunk.
var h$hs_XXH3_sizeof_state_s = typeof h$hs_XXH3_sizeof_state_s === "function" ? h$hs_XXH3_sizeof_state_s : function () {
  return 8;
};

function h$xxh3Append(state, bytes) {
  const xxh3 = state.xxh3;
  if (xxh3.length + bytes.length > xxh3.bytes.length) {
    const grown = new Uint8Array(Math.max(xxh3.bytes.length * 2, xxh3.length + bytes.length));
    grown.set(xxh3.bytes.subarray(0, xxh3.length));
    xxh3.bytes = grown;
  }
  xxh3.bytes.set(bytes, xxh3.length);
  xxh3.length += bytes.length;
}

var h$XXH3_INITSTATE = typeof h$XXH3_INITSTATE === "function" ? h$XXH3_INITSTATE : function (state, _stateOffset) {
  state.xxh3 = { seed: 0n, bytes: new Uint8Array(64), length: 0 };
};

var h$XXH3_64bits_reset_withSeed = typeof h$XXH3_64bits_reset_withSeed === "function" ? h$XXH3_64bits_reset_withSeed : function (state, _stateOffset, seedHigh, seedLow) {
  state.xxh3 = { seed: h$xxh3Word64(seedHigh, seedLow), bytes: new Uint8Array(64), length: 0 };
  return 0;
};

var h$XXH3_64bits_update = typeof h$XXH3_64bits_update === "function" ? h$XXH3_64bits_update : function (state, _stateOffset, buffer, offset, length) {
  h$xxh3Append(state, buffer.u8.subarray(offset, offset + length));
  return 0;
};

var h$hs_XXH3_64bits_update_offset = typeof h$hs_XXH3_64bits_update_offset === "function" ? h$hs_XXH3_64bits_update_offset : function (state, _stateOffset, array, _arrayOffset, offset, length) {
  h$xxh3Append(state, array.u8.subarray(offset, offset + length));
};

var h$hs_XXH3_64bits_update_u64 = typeof h$hs_XXH3_64bits_update_u64 === "function" ? h$hs_XXH3_64bits_update_u64 : function (state, _stateOffset, high, low) {
  h$xxh3Append(state, h$xxh3Bytes(h$xxh3Word64(high, low), 8));
};

var h$hs_XXH3_64bits_update_u32 = typeof h$hs_XXH3_64bits_update_u32 === "function" ? h$hs_XXH3_64bits_update_u32 : function (state, _stateOffset, value) {
  h$xxh3Append(state, h$xxh3Bytes(BigInt(value >>> 0), 4));
};

var h$XXH3_64bits_digest = typeof h$XXH3_64bits_digest === "function" ? h$XXH3_64bits_digest : function (state, _stateOffset) {
  const xxh3 = state.xxh3;
  return h$xxh3Return(h$xxh3(xxh3.bytes.subarray(0, xxh3.length), xxh3.seed));
};
