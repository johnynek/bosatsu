// Runtime import (for standalone module use)
if (typeof require !== 'undefined') { require("./../../_runtime.js"); }

const validate_fib_input = n => (() => {
  const _anon1 = (n < 0) ? [0] : (n === 0) ? [1] : [2];
  return (_anon1[0] === 0) ? [0] : (() => {
      const _anon0 = (n < 40) ? [0] : (n === 40) ? [1] : [2];
      return (_anon0[0] === 2) ? [0] : [1];
    })();
})();
const validate_fact_input = n => (() => {
  const _anon3 = (n < 0) ? [0] : (n === 0) ? [1] : [2];
  return (_anon3[0] === 0) ? [0] : (() => {
      const _anon2 = (n < 20) ? [0] : (n === 20) ? [1] : [2];
      return (_anon2[0] === 2) ? [0] : [1];
    })();
})();
const compute_fib_sequence = n => Bosatsu_Predef$map_List(range(1 + n), i => ((_a0, _a1) => [_a0,
    _a1])(i, Demo_Compute$fib(i)));
const compute_factorial_table = n => Bosatsu_Predef$map_List(range(1 + n), i => ((_a0, _a1) => [_a0,
    _a1])(i, Demo_Compute$factorial(i)));
export {validate_fib_input};
export {validate_fact_input};
export {compute_fib_sequence};
export {compute_factorial_table};