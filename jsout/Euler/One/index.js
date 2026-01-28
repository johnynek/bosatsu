// Import runtime
// require("./../../_runtime.js") or import from "./../../_runtime.js"

const keep = i => (() => {
  const _anon0 = ((3 ? i % 3 : i) === 0) ? [1] : [0];
  return (_anon0[0] === 1) ? [1] : ((5 ? i % 5 : i) === 0) ? [1] : [0];
})();
const computed = foldl_List(flat_map_List(range(1000), i => (() => {
      const _anon1 = Euler_One$keep(i);
      return (_anon1[0] === 1) ? ((_a0, _a1) => [1, _a0, _a1])(i, [0]) : [0];
    })()), 0, (_a0, _a1) => _a0 + _a1);
const test = ((_a0, _a1) => [0,
  _a0,
  _a1])((Euler_One$computed === 233168) ? [1] : [0], _concat_String(((_a0, _a1) => [1,
      _a0,
      _a1])(_js_to_bosatsu_string("expected 233168 got "), ((_a0, _a1) => [1,
        _a0,
        _a1])(_int_to_String(Euler_One$computed), [0]))));
export {keep};