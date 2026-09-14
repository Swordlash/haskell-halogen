const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const zlib = require("zlib");
const CompressionPlugin = require("compression-webpack-plugin");
const webpack = require('webpack');

module.exports = {
  entry: 
    [ './cabal-ghcjs.project'
    , './dev/style.scss'
    ],
  output: {
    filename: 'main.js',
    path: path.resolve(__dirname, 'dist'),
  },
  mode: "development",
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
        use:
          {
            loader: path.resolve(__dirname, "toolchain/haskell-loader.mjs"),
            options: {
              "build-directory": "dist-newstyle/javascript",
              "with-hsc2hs": "javascript-unknown-ghcjs-hsc2hs-9.12.2",
              "system-tools": true,
              "executable": "halogen-material-app"
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
