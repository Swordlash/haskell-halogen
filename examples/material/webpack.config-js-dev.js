const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
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
  mode: "development",
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
        use:
          {
            loader: path.resolve(__dirname, "../../toolchain/haskell-loader.mjs"),
            options: {
              "build-directory": "dist-newstyle/javascript",
              "with-hsc2hs": "javascript-unknown-ghcjs-hsc2hs-9.12.2",
              "system-tools": true,
              "executable": "halogen-example-material"
            }
          }
      },
      {
        test: /\.s[ac]ss$/i,
        use: [ "style-loader", "css-loader", "sass-loader"],
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
