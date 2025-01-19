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
            loader: "@haskell-org/haskell-loader",
            options: {
              "system-tools": false,
              "install-ghc": "9.12.1",
              "install-cabal": "3.14.1.1",
              "executable": "halogen-material-app"
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