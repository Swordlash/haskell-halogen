const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const zlib = require("zlib");
const CompressionPlugin = require("compression-webpack-plugin");
const webpack = require('webpack');

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
  mode: "production",
  resolve: {
    fallback: {
      os: false,
      fs: false,
      child_process: false,
      path: false,
      // The GHC JS RTS require()s this Node-only profiling shim behind an
      // h$isNode() guard in a try/catch. It is never reached in a browser,
      // but webpack still resolves the call site statically.
      "ghcjs-profiling": false,
    }
  },
  module: {
    rules: [
      {
        test: /\.(cabal|project)$/,
        use: [
          {
            loader: "swc-loader"
          },
          {
            loader: path.resolve(__dirname, "../../toolchain/haskell-loader.mjs"),
            options: {
              "build-directory": "dist-newstyle/javascript",
              "with-hsc2hs": "javascript-unknown-ghcjs-hsc2hs-9.12.2",
              "system-tools": true,
              "executable": "halogen-example-material"
            }
          }
        ]
      },
      {
        test: /\.s[ac]ss$/i,
        use: [ "style-loader", "css-loader", "sass-loader"],
      },
      {
        test: /\.m?js$/,
        exclude: /(node_modules)/,
        use: {
          loader: "swc-loader"
        }
      }
    ],
  },
  plugins: 
    [ new HtmlWebpackPlugin({
        title: 'Halogen Material Components'
    })
    , new CompressionPlugin({
        filename: "[path][base].br",
        algorithm: "brotliCompress",
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
    , new webpack.ProgressPlugin()
    ]
};
