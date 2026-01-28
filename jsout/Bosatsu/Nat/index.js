// Import runtime
// require("./../../_runtime.js") or import from "./../../_runtime.js"

const cmp_Nat = (a, b) => (() => {
  let _anon2;
  return (() => {
    let _anon3;
    return (() => {
      let _anon5;
      return (() => {
        let _anon7;
        return (() => {
          (() => {
            _anon5 = a;
            return true;
          })();
          return (() => {
            (() => {
              _anon7 = b;
              return true;
            })();
            return (() => {
              (() => {
                _anon2 = [1];
                return true;
              })();
              return (() => {
                while (_anon2[0] === 1) {
                  (_anon5 === 0) ? (() => {
                      (() => {
                        _anon2 = [0];
                        return true;
                      })();
                      return (() => {
                        (() => {
                          _anon3 = (_anon7 === 0) ? [1] : [0];
                          return true;
                        })();
                        return [];
                      })();
                    })() : (() => {
                      let _anon1;
                      return (() => {
                        (() => {
                          _anon1 = _anon5 - 1;
                          return true;
                        })();
                        return (() => {
                          const n = _anon1;
                          return (_anon7 === 0) ? (() => {
                              (() => {
                                _anon2 = [0];
                                return true;
                              })();
                              return (() => {
                                (() => {
                                  _anon3 = [2];
                                  return true;
                                })();
                                return [];
                              })();
                            })() : (() => {
                              let _anon0;
                              return (() => {
                                (() => {
                                  _anon0 = _anon7 - 1;
                                  return true;
                                })();
                                return (() => {
                                  const m = _anon0;
                                  return (() => {
                                    const _anon4 = n;
                                    return (() => {
                                      const _anon6 = m;
                                      return (() => {
                                        (() => {
                                          _anon5 = _anon4;
                                          return true;
                                        })();
                                        return (() => {
                                          (() => {
                                            _anon7 = _anon6;
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
                    })();
                }
                return _anon3;
              })();
            })();
          })();
        })();
      })();
    })();
  })();
})();
const times2 = n => (() => {
  const loop = (n_1, acc) => (() => {
    let _anon9;
    return (() => {
      let _anon10;
      return (() => {
        let _anon12;
        return (() => {
          let _anon14;
          return (() => {
            (() => {
              _anon12 = n_1;
              return true;
            })();
            return (() => {
              (() => {
                _anon14 = acc;
                return true;
              })();
              return (() => {
                (() => {
                  _anon9 = [1];
                  return true;
                })();
                return (() => {
                  while (_anon9[0] === 1) {
                    (_anon12 === 0) ? (() => {
                        (() => {
                          _anon9 = [0];
                          return true;
                        })();
                        return (() => {
                          (() => {
                            _anon10 = _anon14;
                            return true;
                          })();
                          return [];
                        })();
                      })() : (() => {
                        let _anon8;
                        return (() => {
                          (() => {
                            _anon8 = _anon12 - 1;
                            return true;
                          })();
                          return (() => {
                            const prev = _anon8;
                            return (() => {
                              const _anon11 = prev;
                              return (() => {
                                const _anon13 = (n => n + 1)((n => n + 1)(_anon14));
                                return (() => {
                                  (() => {
                                    _anon12 = _anon11;
                                    return true;
                                  })();
                                  return (() => {
                                    (() => {
                                      _anon14 = _anon13;
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
                  }
                  return _anon10;
                })();
              })();
            })();
          })();
        })();
      })();
    })();
  })();
  return loop(n, 0);
})();
const add = (n1, n2) => (() => {
  const loop = (n1_1, n2_1) => (() => {
    let _anon16;
    return (() => {
      let _anon17;
      return (() => {
        let _anon19;
        return (() => {
          let _anon21;
          return (() => {
            (() => {
              _anon19 = n1_1;
              return true;
            })();
            return (() => {
              (() => {
                _anon21 = n2_1;
                return true;
              })();
              return (() => {
                (() => {
                  _anon16 = [1];
                  return true;
                })();
                return (() => {
                  while (_anon16[0] === 1) {
                    (_anon19 === 0) ? (() => {
                        (() => {
                          _anon16 = [0];
                          return true;
                        })();
                        return (() => {
                          (() => {
                            _anon17 = _anon21;
                            return true;
                          })();
                          return [];
                        })();
                      })() : (() => {
                        let _anon15;
                        return (() => {
                          (() => {
                            _anon15 = _anon19 - 1;
                            return true;
                          })();
                          return (() => {
                            const prev_n1 = _anon15;
                            return (() => {
                              const _anon18 = prev_n1;
                              return (() => {
                                const _anon20 = (n => n + 1)(_anon21);
                                return (() => {
                                  (() => {
                                    _anon19 = _anon18;
                                    return true;
                                  })();
                                  return (() => {
                                    (() => {
                                      _anon21 = _anon20;
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
                  }
                  return _anon17;
                })();
              })();
            })();
          })();
        })();
      })();
    })();
  })();
  return (n2 === 0) ? n1 : loop(n1, n2);
})();
const sub_Nat = (n1, n2) => (() => {
  let _anon24;
  return (() => {
    let _anon25;
    return (() => {
      let _anon27;
      return (() => {
        let _anon29;
        return (() => {
          (() => {
            _anon27 = n1;
            return true;
          })();
          return (() => {
            (() => {
              _anon29 = n2;
              return true;
            })();
            return (() => {
              (() => {
                _anon24 = [1];
                return true;
              })();
              return (() => {
                while (_anon24[0] === 1) {
                  (_anon29 === 0) ? (() => {
                      (() => {
                        _anon24 = [0];
                        return true;
                      })();
                      return (() => {
                        (() => {
                          _anon25 = _anon27;
                          return true;
                        })();
                        return [];
                      })();
                    })() : (() => {
                      let _anon23;
                      return (() => {
                        (() => {
                          _anon23 = _anon29 - 1;
                          return true;
                        })();
                        return (() => {
                          const prev_n2 = _anon23;
                          return (_anon27 === 0) ? (() => {
                              (() => {
                                _anon24 = [0];
                                return true;
                              })();
                              return (() => {
                                (() => {
                                  _anon25 = 0;
                                  return true;
                                })();
                                return [];
                              })();
                            })() : (() => {
                              let _anon22;
                              return (() => {
                                (() => {
                                  _anon22 = _anon27 - 1;
                                  return true;
                                })();
                                return (() => {
                                  const prev_n1 = _anon22;
                                  return (() => {
                                    const _anon26 = prev_n1;
                                    return (() => {
                                      const _anon28 = prev_n2;
                                      return (() => {
                                        (() => {
                                          _anon27 = _anon26;
                                          return true;
                                        })();
                                        return (() => {
                                          (() => {
                                            _anon29 = _anon28;
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
                    })();
                }
                return _anon25;
              })();
            })();
          })();
        })();
      })();
    })();
  })();
})();
const mult = (n1, n2) => (() => {
  const loop = (n1_1, n2_1, c) => (() => {
    let _anon32;
    return (() => {
      let _anon33;
      return (() => {
        let _anon35;
        return (() => {
          let _anon37;
          return (() => {
            let _anon39;
            return (() => {
              (() => {
                _anon35 = n1_1;
                return true;
              })();
              return (() => {
                (() => {
                  _anon37 = n2_1;
                  return true;
                })();
                return (() => {
                  (() => {
                    _anon39 = c;
                    return true;
                  })();
                  return (() => {
                    (() => {
                      _anon32 = [1];
                      return true;
                    })();
                    return (() => {
                      while (_anon32[0] === 1) {
                        (_anon35 === 0) ? (() => {
                            (() => {
                              _anon32 = [0];
                              return true;
                            })();
                            return (() => {
                              (() => {
                                _anon33 = _anon39;
                                return true;
                              })();
                              return [];
                            })();
                          })() : (() => {
                            let _anon31;
                            return (() => {
                              (() => {
                                _anon31 = _anon35 - 1;
                                return true;
                              })();
                              return (() => {
                                const n1_2 = _anon31;
                                return (_anon37 === 0) ? (() => {
                                    (() => {
                                      _anon32 = [0];
                                      return true;
                                    })();
                                    return (() => {
                                      (() => {
                                        _anon33 = _anon39;
                                        return true;
                                      })();
                                      return [];
                                    })();
                                  })() : (() => {
                                    let _anon30;
                                    return (() => {
                                      (() => {
                                        _anon30 = _anon37 - 1;
                                        return true;
                                      })();
                                      return (() => {
                                        const n2_2 = _anon30;
                                        return (() => {
                                          const _anon34 = n1_2;
                                          return (() => {
                                            const _anon36 = n2_2;
                                            return (() => {
                                              const _anon38 = (n => n + 1)(Bosatsu_Nat$add(Bosatsu_Nat$add(n1_2, n2_2), _anon39));
                                              return (() => {
                                                (() => {
                                                  _anon35 = _anon34;
                                                  return true;
                                                })();
                                                return (() => {
                                                  (() => {
                                                    _anon37 = _anon36;
                                                    return true;
                                                  })();
                                                  return (() => {
                                                    (() => {
                                                      _anon39 = _anon38;
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
                              })();
                            })();
                          })();
                      }
                      return _anon33;
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
  return (() => {
    const _anon40 = Bosatsu_Nat$cmp_Nat(n1, n2);
    return (_anon40[0] === 2) ? loop(n2, n1, 0) : loop(n1, n2, 0);
  })();
})();
const is_even = n => (() => {
  const loop = (n_1, res) => (() => {
    let _anon42;
    return (() => {
      let _anon43;
      return (() => {
        let _anon45;
        return (() => {
          let _anon47;
          return (() => {
            (() => {
              _anon45 = n_1;
              return true;
            })();
            return (() => {
              (() => {
                _anon47 = res;
                return true;
              })();
              return (() => {
                (() => {
                  _anon42 = [1];
                  return true;
                })();
                return (() => {
                  while (_anon42[0] === 1) {
                    (_anon45 === 0) ? (() => {
                        (() => {
                          _anon42 = [0];
                          return true;
                        })();
                        return (() => {
                          (() => {
                            _anon43 = _anon47;
                            return true;
                          })();
                          return [];
                        })();
                      })() : (() => {
                        let _anon41;
                        return (() => {
                          (() => {
                            _anon41 = _anon45 - 1;
                            return true;
                          })();
                          return (() => {
                            const n_2 = _anon41;
                            return (() => {
                              const _anon44 = n_2;
                              return (() => {
                                const _anon46 = (_anon47[0] === 1) ? [0] : [1];
                                return (() => {
                                  (() => {
                                    _anon45 = _anon44;
                                    return true;
                                  })();
                                  return (() => {
                                    (() => {
                                      _anon47 = _anon46;
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
                  }
                  return _anon43;
                })();
              })();
            })();
          })();
        })();
      })();
    })();
  })();
  return loop(n, [1]);
})();
const div2 = n => (() => {
  const loop = (n_1, acc, is_even) => (() => {
    let _anon49;
    return (() => {
      let _anon50;
      return (() => {
        let _anon52;
        return (() => {
          let _anon54;
          return (() => {
            let _anon56;
            return (() => {
              (() => {
                _anon52 = n_1;
                return true;
              })();
              return (() => {
                (() => {
                  _anon54 = acc;
                  return true;
                })();
                return (() => {
                  (() => {
                    _anon56 = is_even;
                    return true;
                  })();
                  return (() => {
                    (() => {
                      _anon49 = [1];
                      return true;
                    })();
                    return (() => {
                      while (_anon49[0] === 1) {
                        (_anon52 === 0) ? (() => {
                            (() => {
                              _anon49 = [0];
                              return true;
                            })();
                            return (() => {
                              (() => {
                                _anon50 = _anon54;
                                return true;
                              })();
                              return [];
                            })();
                          })() : (() => {
                            let _anon48;
                            return (() => {
                              (() => {
                                _anon48 = _anon52 - 1;
                                return true;
                              })();
                              return (() => {
                                const n_2 = _anon48;
                                return (_anon56[0] === 1) ? (() => {
                                    const _anon51 = n_2;
                                    return (() => {
                                      const _anon53 = (n => n + 1)(_anon54);
                                      return (() => {
                                        const _anon55 = [0];
                                        return (() => {
                                          (() => {
                                            _anon52 = _anon51;
                                            return true;
                                          })();
                                          return (() => {
                                            (() => {
                                              _anon54 = _anon53;
                                              return true;
                                            })();
                                            return (() => {
                                              (() => {
                                                _anon56 = _anon55;
                                                return true;
                                              })();
                                              return [];
                                            })();
                                          })();
                                        })();
                                      })();
                                    })();
                                  })() : (() => {
                                    const _anon51 = n_2;
                                    return (() => {
                                      const _anon55 = [1];
                                      return (() => {
                                        (() => {
                                          _anon52 = _anon51;
                                          return true;
                                        })();
                                        return (() => {
                                          (() => {
                                            _anon56 = _anon55;
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
                      }
                      return _anon50;
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
  return loop(n, 0, Bosatsu_Nat$is_even(n));
})();
const divmod = (numerator, divisor) => (() => {
  const loop = (_slots => (numerator_1, d, m) => (() => {
    let _anon59;
    return (() => {
      let _anon60;
      return (() => {
        let _anon62;
        return (() => {
          let _anon64;
          return (() => {
            let _anon66;
            return (() => {
              (() => {
                _anon62 = numerator_1;
                return true;
              })();
              return (() => {
                (() => {
                  _anon64 = d;
                  return true;
                })();
                return (() => {
                  (() => {
                    _anon66 = m;
                    return true;
                  })();
                  return (() => {
                    (() => {
                      _anon59 = [1];
                      return true;
                    })();
                    return (() => {
                      while (_anon59[0] === 1) {
                        (_anon62 === 0) ? (() => {
                            (() => {
                              _anon59 = [0];
                              return true;
                            })();
                            return (() => {
                              (() => {
                                _anon60 = ((_a0, _a1) => [_a0,
                                  _a1])(_anon64, _anon66);
                                return true;
                              })();
                              return [];
                            })();
                          })() : (() => {
                            let _anon58;
                            return (() => {
                              (() => {
                                _anon58 = _anon62 - 1;
                                return true;
                              })();
                              return (() => {
                                const n = _anon58;
                                return (() => {
                                  const m1 = (n => n + 1)(_anon66);
                                  return (() => {
                                    const _anon57 = Bosatsu_Nat$cmp_Nat(m1, _slots[0]);
                                    return (_anon57[0] === 1) ? (() => {
                                        const _anon61 = n;
                                        return (() => {
                                          const _anon63 = (n => n + 1)(_anon64);
                                          return (() => {
                                            const _anon65 = 0;
                                            return (() => {
                                              (() => {
                                                _anon62 = _anon61;
                                                return true;
                                              })();
                                              return (() => {
                                                (() => {
                                                  _anon64 = _anon63;
                                                  return true;
                                                })();
                                                return (() => {
                                                  (() => {
                                                    _anon66 = _anon65;
                                                    return true;
                                                  })();
                                                  return [];
                                                })();
                                              })();
                                            })();
                                          })();
                                        })();
                                      })() : (() => {
                                        const _anon61 = n;
                                        return (() => {
                                          const _anon65 = m1;
                                          return (() => {
                                            (() => {
                                              _anon62 = _anon61;
                                              return true;
                                            })();
                                            return (() => {
                                              (() => {
                                                _anon66 = _anon65;
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
                      return _anon60;
                    })();
                  })();
                })();
              })();
            })();
          })();
        })();
      })();
    })();
  })())([divisor]);
  return (() => {
    let _anon67;
    return (divisor > 0 && (() => {
      _anon67 = divisor - 1;
      return true;
    })() && (_anon67 === 0)) ? ((_a0, _a1) => [_a0,
        _a1])(numerator, 0) : (divisor > 0) ? loop(numerator, 0, 0) : ((_a0, _a1) => [_a0,
          _a1])(0, numerator);
  })();
})();
const one = (n => n + 1)(0);
const exp = (base, power) => (base === 0) ? (power === 0) ? Bosatsu_Nat$one : 0 : (() => {
    let _anon75;
    return (base > 0 && (() => {
      _anon75 = base - 1;
      return true;
    })() && (_anon75 === 0)) ? Bosatsu_Nat$one : (() => {
        const two_or_more = base;
        return (() => {
          const loop = (_slots => (power_1, acc) => (() => {
            let _anon69;
            return (() => {
              let _anon70;
              return (() => {
                let _anon72;
                return (() => {
                  let _anon74;
                  return (() => {
                    (() => {
                      _anon72 = power_1;
                      return true;
                    })();
                    return (() => {
                      (() => {
                        _anon74 = acc;
                        return true;
                      })();
                      return (() => {
                        (() => {
                          _anon69 = [1];
                          return true;
                        })();
                        return (() => {
                          while (_anon69[0] === 1) {
                            (_anon72 === 0) ? (() => {
                                (() => {
                                  _anon69 = [0];
                                  return true;
                                })();
                                return (() => {
                                  (() => {
                                    _anon70 = _anon74;
                                    return true;
                                  })();
                                  return [];
                                })();
                              })() : (() => {
                                let _anon68;
                                return (() => {
                                  (() => {
                                    _anon68 = _anon72 - 1;
                                    return true;
                                  })();
                                  return (() => {
                                    const prev = _anon68;
                                    return (() => {
                                      const _anon71 = prev;
                                      return (() => {
                                        const _anon73 = Bosatsu_Nat$mult(_anon74, _slots[0]);
                                        return (() => {
                                          (() => {
                                            _anon72 = _anon71;
                                            return true;
                                          })();
                                          return (() => {
                                            (() => {
                                              _anon74 = _anon73;
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
                          }
                          return _anon70;
                        })();
                      })();
                    })();
                  })();
                })();
              })();
            })();
          })())([two_or_more]);
          return loop(power, Bosatsu_Nat$one);
        })();
      })();
  })();
const to_Int = n => (() => {
  const loop = (acc, n_1) => (() => {
    let _anon77;
    return (() => {
      let _anon78;
      return (() => {
        let _anon80;
        return (() => {
          let _anon82;
          return (() => {
            (() => {
              _anon80 = acc;
              return true;
            })();
            return (() => {
              (() => {
                _anon82 = n_1;
                return true;
              })();
              return (() => {
                (() => {
                  _anon77 = [1];
                  return true;
                })();
                return (() => {
                  while (_anon77[0] === 1) {
                    (_anon82 === 0) ? (() => {
                        (() => {
                          _anon77 = [0];
                          return true;
                        })();
                        return (() => {
                          (() => {
                            _anon78 = _anon80;
                            return true;
                          })();
                          return [];
                        })();
                      })() : (() => {
                        let _anon76;
                        return (() => {
                          (() => {
                            _anon76 = _anon82 - 1;
                            return true;
                          })();
                          return (() => {
                            const n_2 = _anon76;
                            return (() => {
                              const _anon79 = 1 + _anon80;
                              return (() => {
                                const _anon81 = n_2;
                                return (() => {
                                  (() => {
                                    _anon80 = _anon79;
                                    return true;
                                  })();
                                  return (() => {
                                    (() => {
                                      _anon82 = _anon81;
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
                  }
                  return _anon78;
                })();
              })();
            })();
          })();
        })();
      })();
    })();
  })();
  return loop(0, n);
})();
const to_Nat = i => _int_loop(i, 0, (i_1, nat) => ((_a0, _a1) => [_a0,
    _a1])((-1) + i_1, (n => n + 1)(nat)));
const n1 = (n => n + 1)(0);
const n2 = (n => n + 1)(Bosatsu_Nat$n1);
const n3 = (n => n + 1)(Bosatsu_Nat$n2);
const n4 = (n => n + 1)(Bosatsu_Nat$n3);
const n5 = (n => n + 1)(Bosatsu_Nat$n4);
const addLaw = (n1, n2, label) => ((_a0, _a1) => [0,
  _a0,
  _a1])((Bosatsu_Nat$to_Int(Bosatsu_Nat$add(n1, n2)) === (Bosatsu_Nat$to_Int(n1) + Bosatsu_Nat$to_Int(n2))) ? [1] : [0], label);
const multLaw = (n1, n2, label) => ((_a0, _a1) => [0,
  _a0,
  _a1])((Bosatsu_Nat$to_Int(Bosatsu_Nat$mult(n1, n2)) === (Bosatsu_Nat$to_Int(n1) * Bosatsu_Nat$to_Int(n2))) ? [1] : [0], label);
const from_to_law = (i, message) => ((_a0, _a1) => [0,
  _a0,
  _a1])((Bosatsu_Nat$to_Int(_int_loop(i, 0, (i1, nat) => ((_a0, _a1) => [_a0,
        _a1])((-1) + i1, (n => n + 1)(nat)))) === i) ? [1] : [0], message);
const from_to_suite = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("to_Nat/to_Int tests"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0,
      _a0,
      _a1])((Bosatsu_Nat$to_Int(_int_loop((-1), 0, (i, nat) => ((_a0, _a1) => [_a0,
            _a1])((-1) + i, (n => n + 1)(nat)))) === 0) ? [1] : [0], _js_to_bosatsu_string("-1 -> 0")), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1) => [0,
        _a0,
        _a1])((Bosatsu_Nat$to_Int(_int_loop((-42), 0, (i_1, nat_1) => ((_a0, _a1) => [_a0,
              _a1])((-1) + i_1, (n => n + 1)(nat_1)))) === 0) ? [1] : [0], _js_to_bosatsu_string("-42 -> 0")), ((_a0, _a1) => [1,
        _a0,
        _a1])(Bosatsu_Nat$from_to_law(0, _js_to_bosatsu_string("0")), ((_a0, _a1) => [1,
          _a0,
          _a1])(Bosatsu_Nat$from_to_law(1, _js_to_bosatsu_string("1")), ((_a0, _a1) => [1,
            _a0,
            _a1])(Bosatsu_Nat$from_to_law(10, _js_to_bosatsu_string("10")), ((_a0, _a1) => [1,
              _a0,
              _a1])(Bosatsu_Nat$from_to_law(42, _js_to_bosatsu_string("42")), [0])))))));
const tests = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("Nat tests"), ((_a0, _a1) => [1,
    _a0,
    _a1])(Bosatsu_Nat$addLaw(0, 0, _js_to_bosatsu_string("0 + 0")), ((_a0, _a1) => [1,
      _a0,
      _a1])(Bosatsu_Nat$addLaw(0, Bosatsu_Nat$n1, _js_to_bosatsu_string("0 + 1")), ((_a0, _a1) => [1,
        _a0,
        _a1])(Bosatsu_Nat$addLaw(Bosatsu_Nat$n1, 0, _js_to_bosatsu_string("1 + 0")), ((_a0, _a1) => [1,
          _a0,
          _a1])(Bosatsu_Nat$addLaw(Bosatsu_Nat$n1, Bosatsu_Nat$n2, _js_to_bosatsu_string("1 + 2")), ((_a0, _a1) => [1,
            _a0,
            _a1])(Bosatsu_Nat$addLaw(Bosatsu_Nat$n2, Bosatsu_Nat$n1, _js_to_bosatsu_string("2 + 1")), ((_a0, _a1) => [1,
              _a0,
              _a1])(Bosatsu_Nat$multLaw(0, 0, _js_to_bosatsu_string("0 * 0")), ((_a0, _a1) => [1,
                _a0,
                _a1])(Bosatsu_Nat$multLaw(0, Bosatsu_Nat$n1, _js_to_bosatsu_string("0 * 1")), ((_a0, _a1) => [1,
                  _a0,
                  _a1])(Bosatsu_Nat$multLaw(Bosatsu_Nat$n1, 0, _js_to_bosatsu_string("1 * 0")), ((_a0, _a1) => [1,
                    _a0,
                    _a1])(Bosatsu_Nat$multLaw(Bosatsu_Nat$n1, Bosatsu_Nat$n2, _js_to_bosatsu_string("1 * 2")), ((_a0, _a1) => [1,
                      _a0,
                      _a1])(Bosatsu_Nat$multLaw(Bosatsu_Nat$n2, Bosatsu_Nat$n1, _js_to_bosatsu_string("2 * 1")), ((_a0, _a1) => [1,
                        _a0,
                        _a1])(Bosatsu_Nat$from_to_suite, ((_a0, _a1) => [1,
                          _a0,
                          _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                              const _anon83 = Bosatsu_Nat$to_Int(Bosatsu_Nat$exp(Bosatsu_Nat$n2, Bosatsu_Nat$n5));
                              return (_anon83 === 32) ? [1] : [0];
                            })(), _js_to_bosatsu_string("exp(2, 5) == 32")), ((_a0, _a1) => [1,
                            _a0,
                            _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                                const _anon84 = Bosatsu_Nat$to_Int(Bosatsu_Nat$div2(Bosatsu_Nat$n1));
                                return (_anon84 === 0) ? [1] : [0];
                              })(), _js_to_bosatsu_string("1 div2 == 0")), ((_a0, _a1) => [1,
                              _a0,
                              _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                                  const _anon85 = Bosatsu_Nat$to_Int(Bosatsu_Nat$div2(Bosatsu_Nat$n2));
                                  return (_anon85 === 1) ? [1] : [0];
                                })(), _js_to_bosatsu_string("2 div2 == 1")), ((_a0, _a1) => [1,
                                _a0,
                                _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                                    const _anon86 = Bosatsu_Nat$to_Int(Bosatsu_Nat$div2(Bosatsu_Nat$n3));
                                    return (_anon86 === 1) ? [1] : [0];
                                  })(), _js_to_bosatsu_string("3 div2 == 1")), ((_a0, _a1) => [1,
                                  _a0,
                                  _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                                      const _anon87 = Bosatsu_Nat$to_Int(Bosatsu_Nat$div2(Bosatsu_Nat$n4));
                                      return (_anon87 === 2) ? [1] : [0];
                                    })(), _js_to_bosatsu_string("4 div2 == 2")), [0])))))))))))))))));
export {cmp_Nat};