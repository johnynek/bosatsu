// Runtime import (for standalone module use)
if (typeof require !== 'undefined') { require("./../../_runtime.js"); }

const fib = n => (() => {
  const _anon0 = _int_loop(n, ((_a0, _a1) => [_a0,
      _a1])(0, 1), (i, acc) => (() => {
      const a = acc[0];
      return (() => {
        const b = acc[1];
        return ((_a0, _a1) => [_a0, _a1])((-1) + i, ((_a0, _a1) => [_a0,
            _a1])(b, a + b));
      })();
    })());
  return _anon0[0];
})();
const factorial = n => _int_loop(n, 1, (i, acc) => ((_a0, _a1) => [_a0,
    _a1])((-1) + i, acc * i));
const collatz_single = n => (() => {
  const _anon3 = _int_loop(1000, ((_a0, _a1) => [_a0,
      _a1])(n, 0), (remaining, state) => (() => {
      const value = state[0];
      return (() => {
        const steps = state[1];
        return (() => {
          const _anon2 = (value < 1) ? [0] : (value === 1) ? [1] : [2];
          return (_anon2[0] === 2) ? ((_a0, _a1) => [_a0,
              _a1])((-1) + remaining, ((_a0, _a1) => [_a0, _a1])((() => {
                  const _anon1 = 2 ? value % 2 : value;
                  return (_anon1 === 0) ? 2 ? Math.trunc(value / 2) : 0 : 1 + (value + (value + value));
                })(), 1 + steps)) : ((_a0, _a1) => [_a0,
              _a1])((-1) + remaining, ((_a0, _a1) => [_a0, _a1])(value, steps));
        })();
      })();
    })());
  return _anon3[1];
})();
const collatz_steps = n => _int_loop(n, 0, (_slots => (i, max_steps) => (() => {
    const steps = Demo_Compute$collatz_single(1 + (_slots[0] - i));
    return ((_a0, _a1) => [_a0, _a1])((-1) + i, (() => {
        const _anon4 = (steps < max_steps) ? [0] : (steps === max_steps) ? [1] : [2];
        return (_anon4[0] === 2) ? steps : max_steps;
      })());
  })())([n]));
const main = (_a0 => [_a0])(Bosatsu_Prog$ignore_env(Bosatsu_Prog$pure(0)));
export {fib};
export {factorial};
export {collatz_single};
export {collatz_steps};
export {main};