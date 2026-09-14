// Builds the material example with the GHC JavaScript backend.
//
// The entry point is cabal-ghcjs.project: toolchain/haskell-loader.mjs is
// registered against it, so "resolving" that file actually shells out to cabal
// with the GHCJS cross-compiler and returns the generated all.js.
//
// Two environment variables select the variant:
//
//   NODE_ENV=development       skip minification and brotli, for fast rebuilds
//   GHCJS_INSTALL_TOOLCHAIN=1  provision GHC and cabal through ghcup instead of
//                              expecting a cross-compiler already on PATH
//
// See build-js, build-js-dev and build-js-ci in the root package.json.
const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const zlib = require('zlib');
const CompressionPlugin = require('compression-webpack-plugin');
const webpack = require('webpack');

// Keep in step with with-compiler in cabal-ghcjs.project, and with the
// ghc-installer step in .github/workflows/build.yml.
const GHC_VERSION = '9.12.2';
const CABAL_VERSION = '3.14.1.1';

const isDevelopment = process.env.NODE_ENV === 'development';
const installToolchain = process.env.GHCJS_INSTALL_TOOLCHAIN === '1';

const haskellLoader = {
  loader: path.resolve(__dirname, '../../toolchain/haskell-loader.mjs'),
  options: {
    'build-directory': 'dist-newstyle/javascript',
    'with-hsc2hs': `javascript-unknown-ghcjs-hsc2hs-${GHC_VERSION}`,
    'system-tools': !installToolchain,
    'executable': 'halogen-example-material',
    ...(installToolchain && {
      'with-compiler': `javascript-unknown-ghcjs-ghc-${GHC_VERSION}`,
      'with-hc-pkg': `javascript-unknown-ghcjs-ghc-pkg-${GHC_VERSION}`,
      'install-ghc': GHC_VERSION,
      'install-cabal': CABAL_VERSION,
    }),
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
