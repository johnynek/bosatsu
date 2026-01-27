// Import runtime
// require("./../../_runtime.js") or import from "./../../_runtime.js"

const factorial = n => _int_loop(n, 1, (i, p) => ((_a0, _a1) => [_a0,
    _a1])(i - 1, p * i));
const max_candidate = Euler_P5$factorial(10);
const int_loop_up = (top, res, fn) => _int_loop(top, res, (_slots => (i, res_1) => (() => {
    const _anon0 = _slots[0](_slots[1] - i, res_1);
    return (() => {
      const next_rev = _anon0[0];
      return (() => {
        const next_res = _anon0[1];
        return ((_a0, _a1) => [_a0, _a1])(_slots[1] - next_rev, next_res);
      })();
    })();
  })())([fn, top]));
const bound = 1 + Euler_P5$max_candidate;
const factors = range(10);
const div_all = Euler_P5$int_loop_up(Euler_P5$bound, 0, (i, a) => (() => {
    const cand = 1 + i;
    return (() => {
      const _anon1 = Bosatsu_List$for_all(Euler_P5$factors, (_slots => f => (((1 + f) ? _slots[0] % (1 + f) : _slots[0]) === 0) ? [1] : [0])([cand]));
      return (_anon1[0] === 1) ? ((_a0, _a1) => [_a0,
          _a1])(Euler_P5$bound, cand) : ((_a0, _a1) => [_a0, _a1])(1 + i, 0);
    })();
  })());
const test = ((_a0, _a1) => [0,
  _a0,
  _a1])((Euler_P5$div_all === 2520) ? [1] : [0], _js_to_bosatsu_string("test"));
export {factorial};