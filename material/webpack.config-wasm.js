const path = require("path");

module.exports = {
  entry: ["./jsbits/material.js", "./dev/style.scss"],
  output: {
    filename: "material.js",
    path: path.resolve(
      __dirname,
      process.env.WASM_BUILD_DIR || "dist-newstyle/wasm",
      "public",
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
