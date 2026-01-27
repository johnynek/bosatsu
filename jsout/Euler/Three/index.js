// Import runtime
// require("./../../_runtime.js") or import from "./../../_runtime.js"

const smallest_factor = n => (n === 1) ? 1 : _int_loop(n, (-1), (_slots => (i, a) => (() => {
      const trial = 2 + (_slots[0] - i);
      return (() => {
        const _anon0 = ((trial ? _slots[0] % trial : _slots[0]) === 0) ? [1] : [0];
        return (_anon0[0] === 1) ? ((_a0, _a1) => [_a0,
            _a1])(0, trial) : ((_a0, _a1) => [_a0, _a1])((-1) + i, (-1));
      })();
    })())([n]));
const all_factors = n => _int_loop(n, [0], (i, facs) => (() => {
    const next_factor = Euler_Three$smallest_factor(i);
    return (() => {
      const _anon1 = next_factor ? Math.trunc(i / next_factor) : 0;
      return (_anon1 === 1) ? ((_a0, _a1) => [_a0, _a1])(0, ((_a0, _a1) => [1,
            _a0,
            _a1])(next_factor, facs)) : (() => {
          const smaller = _anon1;
          return ((_a0, _a1) => [_a0, _a1])(smaller, ((_a0, _a1) => [1,
              _a0,
              _a1])(next_factor, facs));
        })();
    })();
  })());
const test = ((_a0, _a1) => [0, _a0, _a1])(((() => {
    const _anon2 = Euler_Three$all_factors(600851475143);
    return (_anon2[0] === 0) ? 600851475143 : _anon2[1];
  })() === 6857) ? [1] : [0], _js_to_bosatsu_string("trial"));
export {smallest_factor};