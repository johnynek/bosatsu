// Import runtime
// require("./../../_runtime.js") or import from "./../../_runtime.js"

const max_opt = (o1, o2) => (o1[0] === 0) ? o2 : (() => {
    const s1 = o1;
    return (() => {
      const v1 = o1[1];
      return (o2[0] === 0) ? s1 : (() => {
          const s2 = o2;
          return (() => {
            const v2 = o2[1];
            return (() => {
              const _anon1 = (() => {
                const _anon0 = (v2 < v1) ? [0] : (v2 === v1) ? [1] : [2];
                return (_anon0[0] === 2) ? [1] : [0];
              })();
              return (_anon1[0] === 1) ? s2 : s1;
            })();
          })();
        })();
    })();
  })();
const max_of = (n, fn) => (() => {
  const loop = (_slots => (nat, n_1, max) => (() => {
    let _anon3;
    return (() => {
      let _anon4;
      return (() => {
        let _anon6;
        return (() => {
          let _anon8;
          return (() => {
            let _anon10;
            return (() => {
              (() => {
                _anon6 = nat;
                return true;
              })();
              return (() => {
                (() => {
                  _anon8 = n_1;
                  return true;
                })();
                return (() => {
                  (() => {
                    _anon10 = max;
                    return true;
                  })();
                  return (() => {
                    (() => {
                      _anon3 = [1];
                      return true;
                    })();
                    return (() => {
                      while (_anon3[0] === 1) {
                        (_anon6 === 0) ? (() => {
                            (() => {
                              _anon3 = [0];
                              return true;
                            })();
                            return (() => {
                              (() => {
                                _anon4 = Euler_Four$max_opt(_anon10, _slots[0](_anon8));
                                return true;
                              })();
                              return [];
                            })();
                          })() : (() => {
                            let _anon2;
                            return (() => {
                              (() => {
                                _anon2 = _anon6 - 1;
                                return true;
                              })();
                              return (() => {
                                const prev_nat = _anon2;
                                return (() => {
                                  const _anon5 = prev_nat;
                                  return (() => {
                                    const _anon7 = (-1) + _anon8;
                                    return (() => {
                                      const _anon9 = Euler_Four$max_opt(_anon10, _slots[0](_anon8));
                                      return (() => {
                                        (() => {
                                          _anon6 = _anon5;
                                          return true;
                                        })();
                                        return (() => {
                                          (() => {
                                            _anon8 = _anon7;
                                            return true;
                                          })();
                                          return (() => {
                                            (() => {
                                              _anon10 = _anon9;
                                              return true;
                                            })();
                                            return [];
                                          })();
                                        })();
                                      })();
                                    })();
                                  })();
                                })();
                              })();
                            })();
                          })();
                      }
                      return _anon4;
                    })();
                  })();
                })();
              })();
            })();
          })();
        })();
      })();
    })();
  })())([fn]);
  return loop(Bosatsu_Nat$to_Nat(n), n, [0]);
})();
const first_of = (n, fn) => (() => {
  const loop = (_slots => (nat, n_1) => (() => {
    let _anon13;
    return (() => {
      let _anon14;
      return (() => {
        let _anon16;
        return (() => {
          let _anon18;
          return (() => {
            (() => {
              _anon16 = nat;
              return true;
            })();
            return (() => {
              (() => {
                _anon18 = n_1;
                return true;
              })();
              return (() => {
                (() => {
                  _anon13 = [1];
                  return true;
                })();
                return (() => {
                  while (_anon13[0] === 1) {
                    (_anon16 === 0) ? (() => {
                        (() => {
                          _anon13 = [0];
                          return true;
                        })();
                        return (() => {
                          (() => {
                            _anon14 = _slots[0](_anon18);
                            return true;
                          })();
                          return [];
                        })();
                      })() : (() => {
                        let _anon12;
                        return (() => {
                          (() => {
                            _anon12 = _anon16 - 1;
                            return true;
                          })();
                          return (() => {
                            const prev_nat = _anon12;
                            return (() => {
                              const _anon11 = _slots[0](_anon18);
                              return (_anon11[0] === 0) ? (() => {
                                  const _anon15 = prev_nat;
                                  return (() => {
                                    const _anon17 = (-1) + _anon18;
                                    return (() => {
                                      (() => {
                                        _anon16 = _anon15;
                                        return true;
                                      })();
                                      return (() => {
                                        (() => {
                                          _anon18 = _anon17;
                                          return true;
                                        })();
                                        return [];
                                      })();
                                    })();
                                  })();
                                })() : (() => {
                                  (() => {
                                    _anon13 = [0];
                                    return true;
                                  })();
                                  return (() => {
                                    (() => {
                                      _anon14 = _anon11;
                                      return true;
                                    })();
                                    return [];
                                  })();
                                })();
                            })();
                          })();
                        })();
                      })();
                  }
                  return _anon14;
                })();
              })();
            })();
          })();
        })();
      })();
    })();
  })())([fn]);
  return loop(Bosatsu_Nat$to_Nat(n), n);
})();
const digit_list = n => Bosatsu_Predef$reverse(_int_loop(n, [0], (n_1, acc) => ((_a0, _a1) => [_a0,
      _a1])(10 ? Math.trunc(n_1 / 10) : 0, ((_a0, _a1) => [1,
        _a0,
        _a1])(10 ? n_1 % 10 : n_1, acc))));
const product_palindrome = (n1, n2) => (() => {
  const prod = n1 * n2;
  return (() => {
    const _anon19 = (() => {
      const digits = Euler_Four$digit_list(prod);
      return Bosatsu_List$eq_List((_a0, _a1) => (_a0 === _a1) ? [1] : [0])(digits, Bosatsu_Predef$reverse(digits));
    })();
    return (_anon19[0] === 1) ? (_a0 => [1, _a0])(prod) : [0];
  })();
})();
const max_pal_opt = Euler_Four$max_of(99, n1 => Euler_Four$first_of(99, (_slots => n2 => Euler_Four$product_palindrome(_slots[0], n2))([n1])));
const max_pal = (Euler_Four$max_pal_opt[0] === 1) ? Euler_Four$max_pal_opt[1] : 0;
const test = ((_a0, _a1) => [0,
  _a0,
  _a1])((Euler_Four$max_pal === 9009) ? [1] : [0], _js_to_bosatsu_string("maximum palindrome"));
export {max_opt};