/* config-overrides.js */

module.exports = function override(config, env) {
  //do stuff with the webpack config...
  config.resolve["fallback"] = { fs: false, path: false };
  return config;
};
