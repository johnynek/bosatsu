// Import runtime
// require("./../../_runtime.js") or import from "./../../_runtime.js"

const fib_Nat = n => (n === 0) ? 1 : (() => {
    let _anon0;
    return (n > 0 && (() => {
      _anon0 = n - 1;
      return true;
    })() && (_anon0 === 0)) ? 1 : (() => {
        let _anon1;
        return (() => {
          let _anon2;
          return (() => {
            (() => {
              _anon1 = n - 1;
              return true;
            })() && (() => {
              _anon2 = _anon1 - 1;
              return true;
            })();
            return (() => {
              const prev = _anon1;
              return (() => {
                const p1 = _anon2;
                return fib_Nat(prev) + fib_Nat(p1);
              })();
            })();
          })();
        })();
      })();
  })();
const print_fib = str => (() => {
  const _anon3 = _string_to_Int(str);
  return (_anon3[0] === 1) ? (() => {
      const i = _anon3[1];
      return Bosatsu_Prog$println(_concat_String(((_a0, _a1) => [1,
            _a0,
            _a1])(_js_to_bosatsu_string("fib("), ((_a0, _a1) => [1,
              _a0,
              _a1])(str, ((_a0, _a1) => [1,
                _a0,
                _a1])(_js_to_bosatsu_string(") = "), ((_a0, _a1) => [1,
                  _a0,
                  _a1])(_int_to_String(Bosatsu_FibBench$fib_Nat(Bosatsu_Nat$to_Nat(i))), [0]))))));
    })() : Bosatsu_Prog$println(_concat_String(((_a0, _a1) => [1,
          _a0,
          _a1])(_js_to_bosatsu_string("could not parse "), ((_a0, _a1) => [1,
            _a0,
            _a1])(str, [0]))));
})();
const list_len = (lst, acc) => (() => {
  let _anon4;
  return (() => {
    let _anon5;
    return (() => {
      let _anon7;
      return (() => {
        let _anon9;
        return (() => {
          (() => {
            _anon7 = lst;
            return true;
          })();
          return (() => {
            (() => {
              _anon9 = acc;
              return true;
            })();
            return (() => {
              (() => {
                _anon4 = [1];
                return true;
              })();
              return (() => {
                while (_anon4[0] === 1) {
                  (_anon7[0] === 0) ? (() => {
                      (() => {
                        _anon4 = [0];
                        return true;
                      })();
                      return (() => {
                        (() => {
                          _anon5 = _anon9;
                          return true;
                        })();
                        return [];
                      })();
                    })() : (() => {
                      const tail = _anon7[2];
                      return (() => {
                        const _anon6 = tail;
                        return (() => {
                          const _anon8 = 1 + _anon9;
                          return (() => {
                            (() => {
                              _anon7 = _anon6;
                              return true;
                            })();
                            return (() => {
                              (() => {
                                _anon9 = _anon8;
                                return true;
                              })();
                              return [];
                            })();
                          })();
                        })();
                      })();
                    })();
                }
                return _anon5;
              })();
            })();
          })();
        })();
      })();
    })();
  })();
})();
const _a = args => (() => {
  let _anon10;
  return (() => {
    let _anon11;
    return (args[0] === 1 && (() => {
      _anon10 = args[2];
      return true;
    })() && (_anon10[0] === 1 && (() => {
      _anon11 = _anon10[2];
      return true;
    })() && (_anon11[0] === 0))) ? (() => {
        const n = _anon10[1];
        return Bosatsu_FibBench$print_fib(n);
      })() : Bosatsu_Prog$println(_concat_String(((_a0, _a1) => [1,
            _a0,
            _a1])(_js_to_bosatsu_string("expected exactly one arg, got: "), ((_a0, _a1) => [1,
              _a0,
              _a1])(_int_to_String(Bosatsu_FibBench$list_len(args, 0)), [0]))));
  })();
})();
const main = (_a0 => [_a0])(Bosatsu_Prog$_await(Bosatsu_Prog$read_env)(args => Bosatsu_Prog$_await(Bosatsu_Prog$ignore_env(Bosatsu_FibBench$_a(args)))(a => Bosatsu_Prog$pure(0))));
export {fib_Nat};