const path = require("path");

module.exports = {
  // Entries are relative to this file, not the repo root we are invoked from.
  context: __dirname,
  entry: ["../../material/jsbits/material.js", "./style.scss"],
  output: {
    filename: "material.js",
    path: path.resolve(
      __dirname,
      "../..",
      process.env.WASM_PUBLIC_DIR || "dist-newstyle/wasm/public/material",
    ),
  },
  mode: process.env.NODE_ENV === "development" ? "development" : "production",
  module: {
    rules: [
      {
        test: /\.s[ac]ss$/i,
        use: ["style-loader", "css-loader", "sass-loader"],
      },
    ],
  },
};
