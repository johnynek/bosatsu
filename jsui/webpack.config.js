const scalajsWebpackConfig = require("./scalajs.webpack.config.js");

module.exports = {
  ...scalajsWebpackConfig,
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
