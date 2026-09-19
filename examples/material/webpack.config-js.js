// Builds the material example with the GHC JavaScript backend.
//
// The entry point is cabal-ghcjs.project: toolchain/haskell-loader.mjs is
// registered against it, so "resolving" that file actually shells out to cabal
// with the GHCJS cross-compiler and returns the generated all.js.
//
// The cross-compiler is expected to be on PATH already: locally via ghcup, and
// in CI via the "Install GHC JavaScript toolchain" step in build.yml.
//
// NODE_ENV=development skips minification and brotli for fast rebuilds; see
// build-js and build-js-dev in the root package.json.
const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const zlib = require('zlib');
const CompressionPlugin = require('compression-webpack-plugin');
const webpack = require('webpack');
const fs = require('fs');

const isDevelopment = process.env.NODE_ENV === 'development';

// Cabal rejects a ghc-pkg whose minor version differs from the selected
// compiler, and the one it picks off PATH is the host GHC unless we name the
// pair explicitly. Read the compiler out of the project file so there is one
// source of truth; see toolchain/ghcjs-env.sh for the same fix in the shell
// scripts.
const ghcjsGhc = fs
  .readFileSync(path.resolve(__dirname, '../../cabal-ghcjs.project'), 'utf8')
  .match(/^with-compiler:\s*(\S+)/m)[1];

const haskellLoader = {
  loader: path.resolve(__dirname, '../../toolchain/haskell-loader.mjs'),
  options: {
    'build-directory': 'dist-newstyle/javascript',
    'system-tools': true,
    'executable': 'halogen-example-material',
    'with-compiler': ghcjsGhc,
    'with-hc-pkg': ghcjsGhc.replace('-ghc-', '-ghc-pkg-'),
  },
};

module.exports = {
  // Entries are relative to this file, not the repo root we are invoked from.
  context: __dirname,
  entry:
    [ '../../cabal-ghcjs.project'
    , './style.scss'
    ],
  output: {
    filename: 'main.js',
    path: path.resolve(__dirname, '../../dist'),
  },
  mode: isDevelopment ? 'development' : 'production',
  resolve: {
    fallback: {
      os: false,
      fs: false,
      child_process: false,
      path: false,
      // The GHC JS RTS require()s this Node-only profiling shim behind an
      // h$isNode() guard in a try/catch. It is never reached in a browser,
      // but webpack still resolves the call site statically.
      'ghcjs-profiling': false,
    }
  },
  module: {
    rules: [
      {
        test: /\.(cabal|project)$/,
        // swc buys ~0.6% on the final asset once webpack's Terser has run, so
        // it is not worth the extra pass on a 7 MB module during development.
        use: isDevelopment
          ? [haskellLoader]
          : [{ loader: 'swc-loader' }, haskellLoader],
      },
      {
        test: /\.s[ac]ss$/i,
        use: [ 'style-loader', 'css-loader', 'sass-loader'],
      },
      ...(isDevelopment ? [] : [
        {
          test: /\.m?js$/,
          exclude: /(node_modules)/,
          use: {
            loader: 'swc-loader'
          }
        }
      ]),
    ],
  },
  plugins:
    [ new HtmlWebpackPlugin({
        title: 'Halogen Material Components'
    })
    , ...(isDevelopment ? [] : [
        new CompressionPlugin({
          filename: '[path][base].br',
          algorithm: 'brotliCompress',
          test: /\.(js|css|html|svg)$/,
          compressionOptions: {
            params: {
              [zlib.constants.BROTLI_PARAM_QUALITY]: 11,
            },
          },
          threshold: 10240,
          minRatio: 0.8,
          deleteOriginalAssets: false,
        })
      ])
    , new webpack.ProgressPlugin()
    ]
};
