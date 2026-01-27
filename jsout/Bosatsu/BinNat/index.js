// Import runtime
// require("./../../_runtime.js") or import from "./../../_runtime.js"

const is_even = b => (b[0] === 0) ? [1] : (b[0] === 2) ? [1] : [0];
const toInt = b => (b[0] === 0) ? 0 : (b[0] === 1) ? (() => {
      const n = b[1];
      return 1 + (toInt(n) + toInt(n));
    })() : (() => {
      const n_1 = b[1];
      return 2 + (toInt(n_1) + toInt(n_1));
    })();
const toNat = b => (b[0] === 0) ? 0 : (b[0] === 1) ? (() => {
      const n = b[1];
      return (n => n + 1)(Bosatsu_Nat$times2(toNat(n)));
    })() : (() => {
      const n_1 = b[1];
      return (n => n + 1)((n => n + 1)(Bosatsu_Nat$times2(toNat(n_1))));
    })();
const toBinNat = n => (() => {
  const fns_1 = _int_loop(n, [0], (n_1, fns) => (() => {
      const is_even = (() => {
        const _anon0 = n_1 & 1;
        return (_anon0 === 0) ? [1] : [0];
      })();
      return (() => {
        const _anon1 = (is_even[0] === 1) ? ((_a0, _a1) => [_a0,
            _a1])(_a0 => [2, _a0], n_2 => (-1) + n_2) : ((_a0, _a1) => [_a0,
            _a1])(_a0 => [1, _a0], n_3 => n_3);
        return (() => {
          const hfn = _anon1[0];
          return (() => {
            const dec = _anon1[1];
            return ((_a0, _a1) => [_a0, _a1])(dec(n_1 >> 1), ((_a0, _a1) => [1,
                _a0,
                _a1])(hfn, fns));
          })();
        })();
      })();
    })());
  return foldl_List(fns_1, [0], (n_4, fn) => fn(n_4));
})();
const cmp_BinNat = (a, b) => (a[0] === 1) ? (() => {
    const a1 = a[1];
    return (b[0] === 1) ? (() => {
        const b1 = b[1];
        return cmp_BinNat(a1, b1);
      })() : (b[0] === 2) ? (() => {
          const b1_1 = b[1];
          return (() => {
            const _anon2 = cmp_BinNat(a1, b1_1);
            return (_anon2[0] === 0) ? [0] : (_anon2[0] === 1) ? [0] : [2];
          })();
        })() : [2];
  })() : (a[0] === 2) ? (() => {
      const a1_1 = a[1];
      return (b[0] === 2) ? (() => {
          const b1_2 = b[1];
          return cmp_BinNat(a1_1, b1_2);
        })() : (b[0] === 1) ? (() => {
            const b1_3 = b[1];
            return (() => {
              const _anon3 = cmp_BinNat(a1_1, b1_3);
              return (_anon3[0] === 2) ? [2] : (_anon3[0] === 1) ? [2] : [0];
            })();
          })() : [2];
    })() : (b[0] === 1) ? [0] : (b[0] === 2) ? [0] : [1];
const eq_BinNat = (a, b) => (() => {
  let _anon4;
  return (() => {
    let _anon5;
    return (() => {
      let _anon7;
      return (() => {
        let _anon9;
        return (() => {
          (() => {
            _anon7 = a;
            return true;
          })();
          return (() => {
            (() => {
              _anon9 = b;
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
                          _anon5 = (_anon9[0] === 0) ? [1] : [0];
                          return true;
                        })();
                        return [];
                      })();
                    })() : (_anon7[0] === 1) ? (() => {
                        const n = _anon7[1];
                        return (_anon9[0] === 1) ? (() => {
                            const m = _anon9[1];
                            return (() => {
                              const _anon6 = n;
                              return (() => {
                                const _anon8 = m;
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
                          })() : (() => {
                            (() => {
                              _anon4 = [0];
                              return true;
                            })();
                            return (() => {
                              (() => {
                                _anon5 = [0];
                                return true;
                              })();
                              return [];
                            })();
                          })();
                      })() : (() => {
                        const n_1 = _anon7[1];
                        return (_anon9[0] === 2) ? (() => {
                            const m_1 = _anon9[1];
                            return (() => {
                              const _anon6 = n_1;
                              return (() => {
                                const _anon8 = m_1;
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
                          })() : (() => {
                            (() => {
                              _anon4 = [0];
                              return true;
                            })();
                            return (() => {
                              (() => {
                                _anon5 = [0];
                                return true;
                              })();
                              return [];
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
const next = b => (b[0] === 1) ? (() => {
    const half = b[1];
    return (_a0 => [2, _a0])(half);
  })() : (b[0] === 2) ? (() => {
      const half1 = b[1];
      return (_a0 => [1, _a0])(next(half1));
    })() : (_a0 => [1, _a0])([0]);
const prev = b => (b[0] === 0) ? [0] : (() => {
    let _anon10;
    return (b[0] === 1 && (() => {
      _anon10 = b[1];
      return true;
    })() && (_anon10[0] === 0)) ? [0] : (b[0] === 1) ? (() => {
          const half = b[1];
          return (_a0 => [2, _a0])(prev(half));
        })() : (() => {
          const half1 = b[1];
          return (_a0 => [1, _a0])(half1);
        })();
  })();
const add_BinNat = (left, right) => (left[0] === 1) ? (() => {
    const odd = left;
    return (() => {
      const left_1 = left[1];
      return (right[0] === 1) ? (() => {
          const right_1 = right[1];
          return (_a0 => [2, _a0])(add_BinNat(left_1, right_1));
        })() : (right[0] === 2) ? (() => {
            const right_2 = right[1];
            return (_a0 => [1,
              _a0])(add_BinNat(left_1, Bosatsu_BinNat$next(right_2)));
          })() : odd;
    })();
  })() : (left[0] === 2) ? (() => {
      const even = left;
      return (() => {
        const left_2 = left[1];
        return (right[0] === 1) ? (() => {
            const right_3 = right[1];
            return (_a0 => [1,
              _a0])(add_BinNat(left_2, Bosatsu_BinNat$next(right_3)));
          })() : (right[0] === 2) ? (() => {
              const right_4 = right[1];
              return (_a0 => [2,
                _a0])(add_BinNat(left_2, Bosatsu_BinNat$next(right_4)));
            })() : even;
      })();
    })() : right;
const times2 = b => (b[0] === 1) ? (() => {
    const n = b[1];
    return (_a0 => [2, _a0])(times2(n));
  })() : (b[0] === 2) ? (() => {
      const n_1 = b[1];
      return (_a0 => [2, _a0])((_a0 => [1, _a0])(n_1));
    })() : [0];
const doub_prev = b => (b[0] === 1) ? (() => {
    const n = b[1];
    return (_a0 => [1, _a0])((_a0 => [1, _a0])(Bosatsu_BinNat$times2(n)));
  })() : (b[0] === 2) ? (() => {
      const n_1 = b[1];
      return (_a0 => [1, _a0])((_a0 => [1, _a0])((_a0 => [1, _a0])(n_1)));
    })() : [0];
const sub_Option = (left, right) => (left[0] === 0) ? (right[0] === 0) ? (_a0 => [1,
      _a0])([0]) : [0] : (left[0] === 1) ? (() => {
      const odd = left;
      return (() => {
        const left_1 = left[1];
        return (right[0] === 0) ? (_a0 => [1,
            _a0])(odd) : (right[0] === 1) ? (() => {
              const right_1 = right[1];
              return (() => {
                const _anon11 = sub_Option(left_1, right_1);
                return (_anon11[0] === 1) ? (() => {
                    const n_m = _anon11[1];
                    return (_a0 => [1, _a0])(Bosatsu_BinNat$times2(n_m));
                  })() : [0];
              })();
            })() : (() => {
              const right_2 = right[1];
              return (() => {
                const _anon12 = sub_Option(left_1, right_2);
                return (_anon12[0] === 1) ? (() => {
                    const n_m_1 = _anon12[1];
                    return Bosatsu_BinNat$doub_prev(n_m_1);
                  })() : [0];
              })();
            })();
      })();
    })() : (() => {
      const even = left;
      return (() => {
        const left_2 = left[1];
        return (right[0] === 0) ? (_a0 => [1,
            _a0])(even) : (right[0] === 1) ? (() => {
              const right_3 = right[1];
              return (() => {
                const _anon13 = sub_Option(left_2, right_3);
                return (_anon13[0] === 1) ? (() => {
                    const n_m_2 = _anon13[1];
                    return (_a0 => [1, _a0])((_a0 => [1, _a0])(n_m_2));
                  })() : [0];
              })();
            })() : (() => {
              const right_4 = right[1];
              return (() => {
                const _anon14 = sub_Option(left_2, right_4);
                return (_anon14[0] === 1) ? (() => {
                    const n_m_3 = _anon14[1];
                    return (_a0 => [1, _a0])(Bosatsu_BinNat$times2(n_m_3));
                  })() : [0];
              })();
            })();
      })();
    })();
const sub_BinNat = (left, right) => (() => {
  const _anon15 = Bosatsu_BinNat$sub_Option(left, right);
  return (_anon15[0] === 1) ? _anon15[1] : [0];
})();
const div2 = b => (b[0] === 0) ? [0] : (b[0] === 1) ? b[1] : (() => {
      const n = b[1];
      return Bosatsu_BinNat$next(n);
    })();
const times_BinNat = (left, right) => (left[0] === 0) ? [0] : (left[0] === 1) ? (() => {
      const left_1 = left[1];
      return (right[0] === 0) ? [0] : (() => {
          const right_1 = right;
          return Bosatsu_BinNat$add_BinNat(Bosatsu_BinNat$times2(times_BinNat(left_1, right_1)), right_1);
        })();
    })() : (() => {
      const left_2 = left[1];
      return (right[0] === 0) ? [0] : (() => {
          const right_2 = right;
          return Bosatsu_BinNat$times2(Bosatsu_BinNat$add_BinNat(times_BinNat(left_2, right_2), right_2));
        })();
    })();
const one = (_a0 => [1, _a0])([0]);
const divmod = (numerator, divisor) => (() => {
  const loop = (_slots => numerator_1 => (numerator_1[0] === 1) ? (() => {
      const n = numerator_1[1];
      return (() => {
        const _anon20 = loop(n);
        return (() => {
          const d1 = _anon20[0];
          return (() => {
            const m1 = _anon20[1];
            return (() => {
              const m = (_a0 => [1, _a0])(m1);
              return (() => {
                const _anon19 = Bosatsu_BinNat$cmp_BinNat(m, _slots[0]);
                return (_anon19[0] === 0) ? ((_a0, _a1) => [_a0,
                    _a1])(Bosatsu_BinNat$times2(d1), m) : (() => {
                    const m2 = (() => {
                      const _anon16 = Bosatsu_BinNat$sub_Option(m, _slots[0]);
                      return (_anon16[0] === 1) ? _anon16[1] : [0];
                    })();
                    return (() => {
                      const _anon18 = Bosatsu_BinNat$cmp_BinNat(m2, _slots[0]);
                      return (_anon18[0] === 0) ? ((_a0, _a1) => [_a0,
                          _a1])((_a0 => [1,
                            _a0])(d1), m2) : (_anon18[0] === 1) ? ((_a0, _a1) => [_a0,
                            _a1])((_a0 => [2,
                              _a0])(d1), [0]) : ((_a0, _a1) => [_a0,
                            _a1])((_a0 => [2, _a0])(d1), (() => {
                              const _anon17 = Bosatsu_BinNat$sub_Option(m2, _slots[0]);
                              return (_anon17[0] === 1) ? _anon17[1] : [0];
                            })());
                    })();
                  })();
              })();
            })();
          })();
        })();
      })();
    })() : (numerator_1[0] === 2) ? (() => {
        const n_1 = numerator_1[1];
        return (() => {
          const _anon25 = loop(n_1);
          return (() => {
            const d1_1 = _anon25[0];
            return (() => {
              const m1_1 = _anon25[1];
              return (() => {
                const m_1 = (_a0 => [2, _a0])(m1_1);
                return (() => {
                  const _anon24 = Bosatsu_BinNat$cmp_BinNat(m_1, _slots[0]);
                  return (_anon24[0] === 0) ? ((_a0, _a1) => [_a0,
                      _a1])(Bosatsu_BinNat$times2(d1_1), m_1) : (() => {
                      const m2_1 = (() => {
                        const _anon21 = Bosatsu_BinNat$sub_Option(m_1, _slots[0]);
                        return (_anon21[0] === 1) ? _anon21[1] : [0];
                      })();
                      return (() => {
                        const _anon23 = Bosatsu_BinNat$cmp_BinNat(m2_1, _slots[0]);
                        return (_anon23[0] === 0) ? ((_a0, _a1) => [_a0,
                            _a1])((_a0 => [1,
                              _a0])(d1_1), m2_1) : (_anon23[0] === 1) ? ((_a0, _a1) => [_a0,
                              _a1])((_a0 => [2,
                                _a0])(d1_1), [0]) : ((_a0, _a1) => [_a0,
                              _a1])((_a0 => [2, _a0])(d1_1), (() => {
                                const _anon22 = Bosatsu_BinNat$sub_Option(m2_1, _slots[0]);
                                return (_anon22[0] === 1) ? _anon22[1] : [0];
                              })());
                      })();
                    })();
                })();
              })();
            })();
          })();
        })();
      })() : ((_a0, _a1) => [_a0, _a1])([0], [0]))([divisor]);
  return (() => {
    let _anon26;
    return (divisor[0] === 1 && (() => {
      _anon26 = divisor[1];
      return true;
    })() && (_anon26[0] === 0)) ? ((_a0, _a1) => [_a0,
        _a1])(numerator, [0]) : (divisor[0] === 1) ? loop(numerator) : (divisor[0] === 2) ? loop(numerator) : ((_a0, _a1) => [_a0,
            _a1])([0], numerator);
  })();
})();
const exp = (base, power) => (power[0] === 0) ? Bosatsu_BinNat$one : (power[0] === 1) ? (() => {
      const n = power[1];
      return (() => {
        const bn = exp(base, n);
        return Bosatsu_BinNat$times_BinNat(Bosatsu_BinNat$times_BinNat(bn, bn), base);
      })();
    })() : (() => {
      const n_1 = power[1];
      return (() => {
        const bn1 = Bosatsu_BinNat$times_BinNat(exp(base, n_1), base);
        return Bosatsu_BinNat$times_BinNat(bn1, bn1);
      })();
    })();
const fold_left_BinNat = (fn, init, cnt) => (() => {
  const loop = (_slots => (init_1, cnt_1, cnt_Nat) => (() => {
    let _anon28;
    return (() => {
      let _anon29;
      return (() => {
        let _anon31;
        return (() => {
          let _anon33;
          return (() => {
            let _anon35;
            return (() => {
              (() => {
                _anon31 = init_1;
                return true;
              })();
              return (() => {
                (() => {
                  _anon33 = cnt_1;
                  return true;
                })();
                return (() => {
                  (() => {
                    _anon35 = cnt_Nat;
                    return true;
                  })();
                  return (() => {
                    (() => {
                      _anon28 = [1];
                      return true;
                    })();
                    return (() => {
                      while (_anon28[0] === 1) {
                        (_anon35 === 0) ? (() => {
                            (() => {
                              _anon28 = [0];
                              return true;
                            })();
                            return (() => {
                              (() => {
                                _anon29 = _anon31;
                                return true;
                              })();
                              return [];
                            })();
                          })() : (() => {
                            let _anon27;
                            return (() => {
                              (() => {
                                _anon27 = _anon35 - 1;
                                return true;
                              })();
                              return (() => {
                                const prevNat = _anon27;
                                return (() => {
                                  const cnt_2 = Bosatsu_BinNat$prev(_anon33);
                                  return (() => {
                                    const _anon30 = _slots[0](_anon31, cnt_2);
                                    return (() => {
                                      const _anon32 = cnt_2;
                                      return (() => {
                                        const _anon34 = prevNat;
                                        return (() => {
                                          (() => {
                                            _anon31 = _anon30;
                                            return true;
                                          })();
                                          return (() => {
                                            (() => {
                                              _anon33 = _anon32;
                                              return true;
                                            })();
                                            return (() => {
                                              (() => {
                                                _anon35 = _anon34;
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
                      return _anon29;
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
  return loop(init, cnt, Bosatsu_BinNat$toNat(cnt));
})();
const fib = b => (() => {
  const loop = (n, cur, next) => (() => {
    let _anon37;
    return (() => {
      let _anon38;
      return (() => {
        let _anon40;
        return (() => {
          let _anon42;
          return (() => {
            let _anon44;
            return (() => {
              (() => {
                _anon40 = n;
                return true;
              })();
              return (() => {
                (() => {
                  _anon42 = cur;
                  return true;
                })();
                return (() => {
                  (() => {
                    _anon44 = next;
                    return true;
                  })();
                  return (() => {
                    (() => {
                      _anon37 = [1];
                      return true;
                    })();
                    return (() => {
                      while (_anon37[0] === 1) {
                        (_anon40 === 0) ? (() => {
                            (() => {
                              _anon37 = [0];
                              return true;
                            })();
                            return (() => {
                              (() => {
                                _anon38 = _anon42;
                                return true;
                              })();
                              return [];
                            })();
                          })() : (() => {
                            let _anon36;
                            return (() => {
                              (() => {
                                _anon36 = _anon40 - 1;
                                return true;
                              })();
                              return (() => {
                                const n_1 = _anon36;
                                return (() => {
                                  const _anon39 = n_1;
                                  return (() => {
                                    const _anon41 = _anon44;
                                    return (() => {
                                      const _anon43 = Bosatsu_BinNat$add_BinNat(_anon42, _anon44);
                                      return (() => {
                                        (() => {
                                          _anon40 = _anon39;
                                          return true;
                                        })();
                                        return (() => {
                                          (() => {
                                            _anon42 = _anon41;
                                            return true;
                                          })();
                                          return (() => {
                                            (() => {
                                              _anon44 = _anon43;
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
                      return _anon38;
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
    const one = (_a0 => [1, _a0])([0]);
    return loop(Bosatsu_BinNat$toNat(b), one, one);
  })();
})();
const next_law = (i, msg) => ((_a0, _a1) => [0,
  _a0,
  _a1])((Bosatsu_BinNat$toInt(Bosatsu_BinNat$next(Bosatsu_BinNat$toBinNat(i))) === (1 + i)) ? [1] : [0], msg);
const times2_law = (i, msg) => ((_a0, _a1) => [0,
  _a0,
  _a1])((Bosatsu_BinNat$toInt(Bosatsu_BinNat$times2(Bosatsu_BinNat$toBinNat(i))) === (i + i)) ? [1] : [0], msg);
const two = Bosatsu_BinNat$next(Bosatsu_BinNat$one);
const three = Bosatsu_BinNat$next(Bosatsu_BinNat$two);
const four = Bosatsu_BinNat$next(Bosatsu_BinNat$three);
const test = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("BinNat tests"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0,
      _a0,
      _a1])((Bosatsu_BinNat$toInt([0]) === 0) ? [1] : [0], _js_to_bosatsu_string("0.toBinNat")), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1) => [0, _a0, _a1])((Bosatsu_BinNat$toInt((_a0 => [1,
            _a0])([0])) === 1) ? [1] : [0], _js_to_bosatsu_string("1.toBinNat")), ((_a0, _a1) => [1,
        _a0,
        _a1])(((_a0, _a1) => [0, _a0, _a1])((Bosatsu_BinNat$toInt((_a0 => [2,
              _a0])([0])) === 2) ? [1] : [0], _js_to_bosatsu_string("2.toBinNat")), ((_a0, _a1) => [1,
          _a0,
          _a1])(((_a0, _a1) => [0, _a0, _a1])((Bosatsu_BinNat$toInt((_a0 => [1,
                _a0])((_a0 => [1,
                  _a0])([0]))) === 3) ? [1] : [0], _js_to_bosatsu_string("3.toBinNat")), ((_a0, _a1) => [1,
            _a0,
            _a1])(((_a0, _a1) => [0,
              _a0,
              _a1])((Bosatsu_BinNat$toInt((_a0 => [2, _a0])((_a0 => [1,
                    _a0])([0]))) === 4) ? [1] : [0], _js_to_bosatsu_string("4.toBinNat")), ((_a0, _a1) => [1,
              _a0,
              _a1])(((_a0, _a1) => [1,
                _a0,
                _a1])(_js_to_bosatsu_string("round trip laws"), Bosatsu_Predef$map_List(((_a0, _a1) => [1,
                    _a0,
                    _a1])(((_a0, _a1) => [_a0,
                      _a1])(0, _js_to_bosatsu_string("roundtrip 0")), ((_a0, _a1) => [1,
                      _a0,
                      _a1])(((_a0, _a1) => [_a0,
                        _a1])(1, _js_to_bosatsu_string("roundtrip 1")), ((_a0, _a1) => [1,
                        _a0,
                        _a1])(((_a0, _a1) => [_a0,
                          _a1])(2, _js_to_bosatsu_string("roundtrip 2")), ((_a0, _a1) => [1,
                          _a0,
                          _a1])(((_a0, _a1) => [_a0,
                            _a1])(3, _js_to_bosatsu_string("roundtrip 3")), ((_a0, _a1) => [1,
                            _a0,
                            _a1])(((_a0, _a1) => [_a0,
                              _a1])(4, _js_to_bosatsu_string("roundtrip 4")), ((_a0, _a1) => [1,
                              _a0,
                              _a1])(((_a0, _a1) => [_a0,
                                _a1])(5, _js_to_bosatsu_string("roundtrip 5")), ((_a0, _a1) => [1,
                                _a0,
                                _a1])(((_a0, _a1) => [_a0,
                                  _a1])(6, _js_to_bosatsu_string("roundtrip 6")), ((_a0, _a1) => [1,
                                  _a0,
                                  _a1])(((_a0, _a1) => [_a0,
                                    _a1])(7, _js_to_bosatsu_string("roundtrip 7")), ((_a0, _a1) => [1,
                                    _a0,
                                    _a1])(((_a0, _a1) => [_a0,
                                      _a1])(50, _js_to_bosatsu_string("roundtrip 50")), ((_a0, _a1) => [1,
                                      _a0,
                                      _a1])(((_a0, _a1) => [_a0,
                                        _a1])(61, _js_to_bosatsu_string("roundtrip 61")), ((_a0, _a1) => [1,
                                        _a0,
                                        _a1])(((_a0, _a1) => [_a0,
                                          _a1])(72, _js_to_bosatsu_string("roundtrip 72")), [0]))))))))))), a => (() => {
                    const i = a[0];
                    return (() => {
                      const m = a[1];
                      return ((_a0, _a1) => [0,
                        _a0,
                        _a1])((Bosatsu_BinNat$toInt(Bosatsu_BinNat$toBinNat(i)) === i) ? [1] : [0], m);
                    })();
                  })())), ((_a0, _a1) => [1, _a0, _a1])(((_a0, _a1) => [1,
                  _a0,
                  _a1])(_js_to_bosatsu_string("next law"), Bosatsu_Predef$map_List(((_a0, _a1) => [1,
                      _a0,
                      _a1])(((_a0, _a1) => [_a0,
                        _a1])(0, _js_to_bosatsu_string("0.next")), ((_a0, _a1) => [1,
                        _a0,
                        _a1])(((_a0, _a1) => [_a0,
                          _a1])(5, _js_to_bosatsu_string("5.next")), ((_a0, _a1) => [1,
                          _a0,
                          _a1])(((_a0, _a1) => [_a0,
                            _a1])(10, _js_to_bosatsu_string("10.next")), ((_a0, _a1) => [1,
                            _a0,
                            _a1])(((_a0, _a1) => [_a0,
                              _a1])(113, _js_to_bosatsu_string("113.next")), [0])))), a_1 => (() => {
                      const i_1 = a_1[0];
                      return (() => {
                        const msg = a_1[1];
                        return Bosatsu_BinNat$next_law(i_1, msg);
                      })();
                    })())), ((_a0, _a1) => [1, _a0, _a1])(((_a0, _a1) => [0,
                    _a0,
                    _a1])((Bosatsu_BinNat$toInt(Bosatsu_BinNat$prev(Bosatsu_BinNat$next(Bosatsu_BinNat$toBinNat(0)))) === 0) ? [1] : [0], _js_to_bosatsu_string("0.next().prev == 0")), ((_a0, _a1) => [1,
                    _a0,
                    _a1])(((_a0, _a1) => [0,
                      _a0,
                      _a1])((Bosatsu_BinNat$toInt(Bosatsu_BinNat$prev(Bosatsu_BinNat$next(Bosatsu_BinNat$toBinNat(5)))) === 5) ? [1] : [0], _js_to_bosatsu_string("5.next().prev == 5")), ((_a0, _a1) => [1,
                      _a0,
                      _a1])(((_a0, _a1) => [0,
                        _a0,
                        _a1])((Bosatsu_BinNat$toInt(Bosatsu_BinNat$prev(Bosatsu_BinNat$next(Bosatsu_BinNat$toBinNat(10)))) === 10) ? [1] : [0], _js_to_bosatsu_string("10.next().prev == 10")), ((_a0, _a1) => [1,
                        _a0,
                        _a1])(((_a0, _a1) => [0,
                          _a0,
                          _a1])((Bosatsu_BinNat$toInt(Bosatsu_BinNat$add_BinNat(Bosatsu_BinNat$toBinNat(10), Bosatsu_BinNat$toBinNat(11))) === 21) ? [1] : [0], _js_to_bosatsu_string("add_BinNat(10, 11) == 21")), ((_a0, _a1) => [1,
                          _a0,
                          _a1])(((_a0, _a1) => [1,
                            _a0,
                            _a1])(_js_to_bosatsu_string("times2 law"), Bosatsu_Predef$map_List(((_a0, _a1) => [1,
                                _a0,
                                _a1])(((_a0, _a1) => [_a0,
                                  _a1])(0, _js_to_bosatsu_string("0 * 2")), ((_a0, _a1) => [1,
                                  _a0,
                                  _a1])(((_a0, _a1) => [_a0,
                                    _a1])(1, _js_to_bosatsu_string("1 * 2")), ((_a0, _a1) => [1,
                                    _a0,
                                    _a1])(((_a0, _a1) => [_a0,
                                      _a1])(2, _js_to_bosatsu_string("2 * 2")), ((_a0, _a1) => [1,
                                      _a0,
                                      _a1])(((_a0, _a1) => [_a0,
                                        _a1])(5, _js_to_bosatsu_string("5 * 2")), ((_a0, _a1) => [1,
                                        _a0,
                                        _a1])(((_a0, _a1) => [_a0,
                                          _a1])(10, _js_to_bosatsu_string("10 * 2")), [0]))))), a_2 => (() => {
                                const i_2 = a_2[0];
                                return (() => {
                                  const msg_1 = a_2[1];
                                  return Bosatsu_BinNat$times2_law(i_2, msg_1);
                                })();
                              })())), ((_a0, _a1) => [1,
                            _a0,
                            _a1])(((_a0, _a1) => [0,
                              _a0,
                              _a1])((Bosatsu_BinNat$toInt(Bosatsu_BinNat$times_BinNat(Bosatsu_BinNat$toBinNat(10), Bosatsu_BinNat$toBinNat(11))) === 110) ? [1] : [0], _js_to_bosatsu_string("10*11 = 110")), ((_a0, _a1) => [1,
                              _a0,
                              _a1])(((_a0, _a1) => [0,
                                _a0,
                                _a1])((Bosatsu_BinNat$toInt(Bosatsu_BinNat$times_BinNat(Bosatsu_BinNat$toBinNat(0), Bosatsu_BinNat$toBinNat(11))) === 0) ? [1] : [0], _js_to_bosatsu_string("0*11 = 0")), ((_a0, _a1) => [1,
                                _a0,
                                _a1])(((_a0, _a1) => [0,
                                  _a0,
                                  _a1])((Bosatsu_BinNat$toInt(Bosatsu_BinNat$fold_left_BinNat((n, a_3) => Bosatsu_BinNat$next(n), [0], Bosatsu_BinNat$toBinNat(10))) === 10) ? [1] : [0], _js_to_bosatsu_string("1 + ... + 1 = 10")), ((_a0, _a1) => [1,
                                  _a0,
                                  _a1])(((_a0, _a1) => [0,
                                    _a0,
                                    _a1])((Bosatsu_BinNat$toInt(Bosatsu_BinNat$fold_left_BinNat(Bosatsu_BinNat$add_BinNat, [0], Bosatsu_BinNat$toBinNat(4))) === 6) ? [1] : [0], _js_to_bosatsu_string("1+2+3=6")), ((_a0, _a1) => [1,
                                    _a0,
                                    _a1])(((_a0, _a1) => [0,
                                      _a0,
                                      _a1])((Bosatsu_BinNat$toInt(Bosatsu_BinNat$fib([0])) === 1) ? [1] : [0], _js_to_bosatsu_string("fib(0) == 1")), ((_a0, _a1) => [1,
                                      _a0,
                                      _a1])(((_a0, _a1) => [0,
                                        _a0,
                                        _a1])((Bosatsu_BinNat$toInt(Bosatsu_BinNat$fib(Bosatsu_BinNat$one)) === 1) ? [1] : [0], _js_to_bosatsu_string("fib(1) == 1")), ((_a0, _a1) => [1,
                                        _a0,
                                        _a1])(((_a0, _a1) => [0,
                                          _a0,
                                          _a1])((Bosatsu_BinNat$toInt(Bosatsu_BinNat$fib(Bosatsu_BinNat$two)) === 2) ? [1] : [0], _js_to_bosatsu_string("fib(2) == 2")), ((_a0, _a1) => [1,
                                          _a0,
                                          _a1])(((_a0, _a1) => [0,
                                            _a0,
                                            _a1])((Bosatsu_BinNat$toInt(Bosatsu_BinNat$fib(Bosatsu_BinNat$three)) === 3) ? [1] : [0], _concat_String(((_a0, _a1) => [1,
                                                _a0,
                                                _a1])(_js_to_bosatsu_string("fib(3) == 3 (got "), ((_a0, _a1) => [1,
                                                  _a0,
                                                  _a1])(_int_to_String(Bosatsu_BinNat$toInt(Bosatsu_BinNat$fib(Bosatsu_BinNat$three))), ((_a0, _a1) => [1,
                                                    _a0,
                                                    _a1])(_js_to_bosatsu_string(")"), [0]))))), ((_a0, _a1) => [1,
                                            _a0,
                                            _a1])(((_a0, _a1) => [0,
                                              _a0,
                                              _a1])((Bosatsu_BinNat$toInt(Bosatsu_BinNat$fib(Bosatsu_BinNat$four)) === 5) ? [1] : [0], _js_to_bosatsu_string("fib(4) == 5")), ((_a0, _a1) => [1,
                                              _a0,
                                              _a1])(((_a0, _a1) => [0,
                                                _a0,
                                                _a1])((() => {
                                                  const _anon45 = Bosatsu_BinNat$cmp_BinNat(Bosatsu_BinNat$toBinNat(54), Bosatsu_BinNat$toBinNat(54));
                                                  return (_anon45[0] === 1) ? [1] : [0];
                                                })(), _js_to_bosatsu_string("54 == 54")), [0])))))))))))))))))))))));
export {is_even};