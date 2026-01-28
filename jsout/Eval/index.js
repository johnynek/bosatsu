// Import runtime
// require("./../_runtime.js") or import from "./../_runtime.js"

const done = _a0 => [0, _a0];
const flat_map = (e, fn) => (_a0 => [1,
  _a0])((_slots => cb => cb(_slots[0], _slots[1]))([e, fn]));
const bind = e => (_slots => fn => (_a0 => [1,
  _a0])((_slots => cb => cb(_slots[0], _slots[1]))([_slots[0], fn])))([e]);
const map = (e, fn) => (_a0 => [1,
  _a0])((_slots => cb => cb(_slots[0], (_slots => x => (_a0 => [0,
      _a0])(_slots[0](x)))([_slots[1]])))([e, fn]));
const run = (budget, arg) => (budget === 0) ? [0] : (() => {
    let _anon4;
    return (() => {
      (() => {
        _anon4 = budget - 1;
        return true;
      })();
      return (() => {
        const balance = _anon4;
        return (() => {
          let _anon0;
          return (arg[0] === 0 && (() => {
            _anon0 = arg[2];
            return true;
          })() && (_anon0[0] === 0)) ? (() => {
              const a = arg[1];
              return (() => {
                const fn = _anon0[1];
                return (_a0 => [1, _a0])(fn(a));
              })();
            })() : (() => {
              let _anon1;
              return (arg[0] === 1 && (() => {
                _anon1 = arg[1];
                return true;
              })() && (_anon1[0] === 0)) ? (() => {
                  const a_1 = _anon1[1];
                  return (() => {
                    const stack = arg[2];
                    return run(balance, ((_a0, _a1) => [0,
                        _a0,
                        _a1])(a_1, stack));
                  })();
                })() : (() => {
                  let _anon2;
                  return (arg[0] === 1 && (() => {
                    _anon2 = arg[1];
                    return true;
                  })() && (_anon2[0] === 1)) ? (() => {
                      const use = _anon2[1];
                      return (() => {
                        const stack_1 = arg[2];
                        return use((_slots => (prev, fn_1) => _slots[0](_slots[1], ((_a0, _a1) => [1,
                              _a0,
                              _a1])(prev, (_a0 => [1,
                                _a0])((_slots => use_1 => use_1(_slots[0], _slots[1]))([fn_1,
                                    _slots[2]])))))([run, balance, stack_1]));
                      })();
                    })() : (() => {
                      let _anon3;
                      return (() => {
                        (() => {
                          _anon3 = arg[2];
                          return true;
                        })();
                        return (() => {
                          const a_2 = arg[1];
                          return (() => {
                            const use_2 = _anon3[1];
                            return use_2((_slots => (fn_2, stack_2) => _slots[0](_slots[1], ((_a0, _a1) => [1,
                                  _a0,
                                  _a1])(fn_2(_slots[2]), stack_2)))([run,
                                  balance,
                                  a_2]));
                          })();
                        })();
                      })();
                    })();
                })();
            })();
        })();
      })();
    })();
  })();
const _eval = (budget, ea) => Eval$run(Bosatsu_Nat$to_Nat(budget), ((_a0, _a1) => [1,
    _a0,
    _a1])(ea, (_a0 => [0, _a0])(a => a)));
export {done};