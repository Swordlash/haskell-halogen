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
              "with-compiler": "javascript-unknown-ghcjs-ghc-9.12.1",
              "with-hc-pkg": "javascript-unknown-ghcjs-ghc-pkg-9.12.1",
              "with-hsc2hs": "javascript-unknown-ghcjs-hsc2hs-9.12.1",
              "system-tools": false,
              "install-ghc": "9.12.1",
              "install-cabal": "3.14.1.1",
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
    , new webpack.ProgressPlugin()
    ]
};
