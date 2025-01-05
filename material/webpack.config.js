const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const zlib = require("zlib");
const CompressionPlugin = require("compression-webpack-plugin");


module.exports = {
  entry: 
    [ './jsbits/halogen.js'
    , './dev/index.js'
    , './dev/style.scss'
    ],
  output: {
    filename: 'main.js',
    path: path.resolve(__dirname, 'dist'),
  },
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
        test: /\.s[ac]ss$/i,
        use: [
          // Creates `style` nodes from JS strings
          "style-loader",
          // Translates CSS into CommonJS
          "css-loader",
          // Compiles Sass to CSS
          "sass-loader",
        ],
      },
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
      }),
    ]
};