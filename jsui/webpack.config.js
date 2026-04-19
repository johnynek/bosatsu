const scalajsWebpackConfig = require("./scalajs.webpack.config.js");
const generatedScalaJsBundlePattern = /compiler-jsui-(fastopt|opt)\.js$/;
// Scala.js 1.21 emits bundle mappings that webpack's source-map-loader rejects.
const scalajsRules = ((scalajsWebpackConfig.module || {}).rules || []).map(
  (rule) =>
    rule.enforce === "pre" && JSON.stringify(rule.use).includes("source-map-loader")
      ? {
          ...rule,
          exclude: generatedScalaJsBundlePattern
        }
      : rule
);

module.exports = {
  ...scalajsWebpackConfig,
  module: {
    ...(scalajsWebpackConfig.module || {}),
    rules: scalajsRules
  },
  resolve: {
    ...(scalajsWebpackConfig.resolve || {}),
    fallback: {
      ...((scalajsWebpackConfig.resolve || {}).fallback || {}),
      fs: false,
      path: false,
      os: false,
      crypto: false,
      perf_hooks: false
    }
  }
};
