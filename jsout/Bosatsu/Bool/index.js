// Import runtime
// require("./../../_runtime.js") or import from "./../../_runtime.js"

const not = b => (b[0] === 1) ? [0] : [1];
const test = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("not tests"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0,
      _a0,
      _a1])([1], _js_to_bosatsu_string("not(True)")), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1) => [0,
        _a0,
        _a1])([1], _js_to_bosatsu_string("not(False)")), [0])));
export {not};