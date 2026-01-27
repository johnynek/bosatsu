// Import runtime
// require("./../../_runtime.js") or import from "./../../_runtime.js"

const sum = (fn, n) => _int_loop(n, 0, (_slots => (i, r) => (() => {
    const i_1 = (-1) + i;
    return ((_a0, _a1) => [_a0, _a1])(i_1, r + _slots[0](i_1));
  })())([fn]));
const diff = n => Euler_P6$sum(x => (() => {
    const x1 = 1 + x;
    return x * (x1 * x1);
  })(), n);
const test0 = ((_a0, _a1) => [0,
  _a0,
  _a1])((Euler_P6$diff(10) === 2640) ? [1] : [0], _js_to_bosatsu_string("matched problem"));
const test1 = ((_a0, _a1) => [0,
  _a0,
  _a1])((Euler_P6$diff(100) === 25164150) ? [1] : [0], _js_to_bosatsu_string("matched problem"));
const tests = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("two examples"), ((_a0, _a1) => [1,
    _a0,
    _a1])(Euler_P6$test0, ((_a0, _a1) => [1, _a0, _a1])(Euler_P6$test1, [0])));
export {sum};