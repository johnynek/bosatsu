// Import runtime
// require("./../../_runtime.js") or import from "./../../_runtime.js"

const forall_Prop = (rand, name, fn) => (_a0 => [_a0])((_slots => cnt => (() => {
    const rands = Bosatsu_Predef$replicate_List(_slots[0], cnt);
    return (() => {
      const seq = Bosatsu_Rand$sequence_Rand(rands);
      return Bosatsu_Rand$map_Rand(seq, (_slots => as => ((_a0, _a1) => [1,
          _a0,
          _a1])(_slots[0], Bosatsu_Predef$map_List(as, _slots[1])))([_slots[1],
            _slots[2]]));
    })();
  })())([rand, name, fn]));
const suite_Prop = (name, props) => (_a0 => [_a0])((_slots => s => (() => {
    const s_1 = Bosatsu_Rand$sequence_Rand(Bosatsu_Predef$map_List(_slots[0], (_slots => a => (() => {
          const fn = a[0];
          return fn(_slots[0]);
        })())([s])));
    return Bosatsu_Rand$map_Rand(s_1, (_slots => as => ((_a0, _a1) => [1,
        _a0,
        _a1])(_slots[0], as))([_slots[1]]));
  })())([props, name]));
const run_Prop = (prop, trials, seed) => (() => {
  const fn = prop[0];
  return Bosatsu_Rand$run_Rand(fn(trials), seed);
})();
const signed64 = Bosatsu_Rand$map_Rand(Bosatsu_Rand$int_range(1 << 64), i => i - (1 << 63));
const not_law = Bosatsu_Properties$forall_Prop(Bosatsu_Properties$signed64, _js_to_bosatsu_string("not_law"), i => (() => {
    const istr = _int_to_String(i);
    return ((_a0, _a1) => [0,
      _a0,
      _a1])(((~i) === (0 - (1 + i))) ? [1] : [0], _concat_String(((_a0, _a1) => [1,
          _a0,
          _a1])(_js_to_bosatsu_string("~"), ((_a0, _a1) => [1,
            _a0,
            _a1])(istr, ((_a0, _a1) => [1,
              _a0,
              _a1])(_js_to_bosatsu_string(" == (-1 - "), ((_a0, _a1) => [1,
                _a0,
                _a1])(istr, ((_a0, _a1) => [1,
                  _a0,
                  _a1])(_js_to_bosatsu_string(")"), [0])))))));
  })());
const shift_unshift_law = Bosatsu_Properties$forall_Prop(Bosatsu_Rand$prod_Rand(Bosatsu_Properties$signed64, Bosatsu_Rand$int_range(32)), _js_to_bosatsu_string("shift_unshift_law"), a => (() => {
    const i = a[0];
    return (() => {
      const k = a[1];
      return (() => {
        const istr = _int_to_String(i);
        return (() => {
          const kstr = _int_to_String(k);
          return (() => {
            const result = i << k >> k;
            return ((_a0, _a1) => [0,
              _a0,
              _a1])((result === i) ? [1] : [0], _concat_String(((_a0, _a1) => [1,
                  _a0,
                  _a1])(_js_to_bosatsu_string("("), ((_a0, _a1) => [1,
                    _a0,
                    _a1])(istr, ((_a0, _a1) => [1,
                      _a0,
                      _a1])(_js_to_bosatsu_string(" << "), ((_a0, _a1) => [1,
                        _a0,
                        _a1])(kstr, ((_a0, _a1) => [1,
                          _a0,
                          _a1])(_js_to_bosatsu_string(") >> "), ((_a0, _a1) => [1,
                            _a0,
                            _a1])(kstr, ((_a0, _a1) => [1,
                              _a0,
                              _a1])(_js_to_bosatsu_string(" == "), ((_a0, _a1) => [1,
                                _a0,
                                _a1])(istr, ((_a0, _a1) => [1,
                                  _a0,
                                  _a1])(_js_to_bosatsu_string(", got: "), ((_a0, _a1) => [1,
                                    _a0,
                                    _a1])(_int_to_String(result), [0]))))))))))));
          })();
        })();
      })();
    })();
  })());
const positive_and_law = Bosatsu_Properties$forall_Prop(Bosatsu_Rand$prod_Rand(Bosatsu_Properties$signed64, Bosatsu_Properties$signed64), _js_to_bosatsu_string("x & y is >= 0 implies x >= 0 or y >= 0"), a => (() => {
    const x = a[0];
    return (() => {
      const y = a[1];
      return (() => {
        const xs = _int_to_String(x);
        return (() => {
          const ys = _int_to_String(y);
          return ((_a0, _a1) => [0, _a0, _a1])((() => {
              const _anon4 = (() => {
                const _anon0 = (x & y < 0) ? [0] : (x & y === 0) ? [1] : [2];
                return (_anon0[0] === 2) ? [1] : (_anon0[0] === 1) ? [1] : [0];
              })();
              return (_anon4[0] === 1) ? (() => {
                  const _anon3 = (() => {
                    const _anon1 = (x < 0) ? [0] : (x === 0) ? [1] : [2];
                    return (_anon1[0] === 2) ? [1] : (_anon1[0] === 1) ? [1] : [0];
                  })();
                  return (_anon3[0] === 1) ? [1] : (() => {
                      const _anon2 = (y < 0) ? [0] : (y === 0) ? [1] : [2];
                      return (_anon2[0] === 2) ? [1] : (_anon2[0] === 1) ? [1] : [0];
                    })();
                })() : [1];
            })(), _concat_String(((_a0, _a1) => [1,
                _a0,
                _a1])(xs, ((_a0, _a1) => [1,
                  _a0,
                  _a1])(_js_to_bosatsu_string(" & "), ((_a0, _a1) => [1,
                    _a0,
                    _a1])(ys, ((_a0, _a1) => [1,
                      _a0,
                      _a1])(_js_to_bosatsu_string(" is >= 0 implies "), ((_a0, _a1) => [1,
                        _a0,
                        _a1])(xs, ((_a0, _a1) => [1,
                          _a0,
                          _a1])(_js_to_bosatsu_string(" >= 0 or "), ((_a0, _a1) => [1,
                            _a0,
                            _a1])(ys, ((_a0, _a1) => [1,
                              _a0,
                              _a1])(_js_to_bosatsu_string(" >= 0"), [0]))))))))));
        })();
      })();
    })();
  })());
const all_props = Bosatsu_Properties$suite_Prop(_js_to_bosatsu_string("integer props"), ((_a0, _a1) => [1,
    _a0,
    _a1])(Bosatsu_Properties$not_law, ((_a0, _a1) => [1,
      _a0,
      _a1])(Bosatsu_Properties$shift_unshift_law, ((_a0, _a1) => [1,
        _a0,
        _a1])(Bosatsu_Properties$positive_and_law, [0]))));
const test = (() => {
  const fn = Bosatsu_Properties$all_props[0];
  return Bosatsu_Rand$run_Rand(fn(100), 42);
})();
export {forall_Prop};