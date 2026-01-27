// Import runtime
// require("./../../_runtime.js") or import from "./../../_runtime.js"

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
const is_prime = x => Bosatsu_Bool$not(Euler_P7$int_loop_up(x, [0], (_slots => (i, div) => (() => {
      const candidate = 2 + i;
      return (() => {
        const _anon1 = (candidate < _slots[0]) ? [0] : (candidate === _slots[0]) ? [1] : [2];
        return (_anon1[0] === 0) ? (div[0] === 1) ? ((_a0, _a1) => [_a0,
              _a1])(_slots[0], [1]) : ((_a0, _a1) => [_a0,
              _a1])(1 + i, ((candidate ? _slots[0] % candidate : _slots[0]) === 0) ? [1] : [0]) : ((_a0, _a1) => [_a0,
            _a1])(_slots[0], [0]);
      })();
    })())([x])));
const ith_prime = total => (() => {
  const max_size = range(total * total);
  return (() => {
    const _anon4 = foldl_List(max_size, ((_a0, _a1) => [_a0,
        _a1])([0], 0), (_slots => (prime_cnt, i) => (() => {
        const candidate = 2 + i;
        return (() => {
          const primes = prime_cnt[0];
          return (() => {
            const cnt = prime_cnt[1];
            return (() => {
              const _anon3 = (cnt < _slots[0]) ? [0] : (cnt === _slots[0]) ? [1] : [2];
              return (_anon3[0] === 0) ? (() => {
                  const _anon2 = Bosatsu_List$for_all(primes, (_slots => p => Bosatsu_Bool$not(((p ? _slots[0] % p : _slots[0]) === 0) ? [1] : [0]))([candidate]));
                  return (_anon2[0] === 1) ? ((_a0, _a1) => [_a0,
                      _a1])(((_a0, _a1) => [1,
                        _a0,
                        _a1])(candidate, primes), 1 + cnt) : prime_cnt;
                })() : prime_cnt;
            })();
          })();
        })();
      })())([total]));
    return (() => {
      const ps = _anon4[0];
      return (ps[0] === 1) ? ps[1] : (-1);
    })();
  })();
})();
const test = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("euler 7"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0,
      _a0,
      _a1])(Euler_P7$is_prime(13), _js_to_bosatsu_string("6th prime is 13")), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1) => [0,
        _a0,
        _a1])((Euler_P7$ith_prime(6) === 13) ? [1] : [0], _js_to_bosatsu_string("6th prime is 13")), ((_a0, _a1) => [1,
        _a0,
        _a1])(((_a0, _a1) => [0,
          _a0,
          _a1])((Euler_P7$ith_prime(11) === 31) ? [1] : [0], _js_to_bosatsu_string("11th prime is 31")), [0]))));
export {int_loop_up};