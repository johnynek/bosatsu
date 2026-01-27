// Import runtime
// require("./../../_runtime.js") or import from "./../../_runtime.js"

const rand_Int = Bosatsu_Rand$from_pair(Bosatsu_Rand$int_range(128), Bosatsu_Rand$geometric_Int);
const rand_Nat = Bosatsu_Rand$map_Rand(Bosatsu_NumberProps$rand_Int, Bosatsu_Nat$to_Nat);
const rand_BinNat = Bosatsu_Rand$map_Rand(Bosatsu_NumberProps$rand_Int, Bosatsu_BinNat$toBinNat);
const rand_BinInt = (() => {
  const pos = Bosatsu_Rand$map_Rand(Bosatsu_NumberProps$rand_Int, Bosatsu_BinInt$int_to_BinInt);
  return Bosatsu_Rand$from_pair(pos, Bosatsu_Rand$map_Rand(pos, Bosatsu_BinInt$not));
})();
const int_props = Bosatsu_Properties$suite_Prop(_js_to_bosatsu_string("Int props"), ((_a0, _a1) => [1,
    _a0,
    _a1])(Bosatsu_Properties$forall_Prop(Bosatsu_Rand$prod_Rand(Bosatsu_NumberProps$rand_Int, Bosatsu_NumberProps$rand_Int), _js_to_bosatsu_string("divmod law"), c => (() => {
        const a = c[0];
        return (() => {
          const b = c[1];
          return ((_a0, _a1) => [0,
            _a0,
            _a1])(((b ? Math.trunc(a / b) : 0) * b + (b ? a % b : a) === a) ? [1] : [0], _js_to_bosatsu_string("check"));
        })();
      })()), [0]));
const cmp_Comparison = (c1, c2) => (c1[0] === 0) ? (c2[0] === 0) ? [1] : [0] : (c1[0] === 1) ? (c2[0] === 0) ? [2] : (c2[0] === 1) ? [1] : [0] : (c2[0] === 2) ? [1] : [2];
const exp_Int = (base, power) => _int_loop(power, 1, (_slots => (p, acc) => ((_a0, _a1) => [_a0,
    _a1])((-1) + p, acc * _slots[0]))([base]));
const small_rand_Nat = Bosatsu_Rand$map_Rand(Bosatsu_Rand$int_range(7), Bosatsu_Nat$to_Nat);
const small_rand_BinNat = Bosatsu_Rand$map_Rand(Bosatsu_Rand$int_range(7), Bosatsu_BinNat$toBinNat);
const nat_props = Bosatsu_Properties$suite_Prop(_js_to_bosatsu_string("Nat props"), ((_a0, _a1) => [1,
    _a0,
    _a1])(Bosatsu_Properties$forall_Prop(Bosatsu_NumberProps$rand_Nat, _js_to_bosatsu_string("if is_even(n) then times2(div2(n)) == n"), n => (() => {
        const _anon2 = Bosatsu_Nat$is_even(n);
        return (_anon2[0] === 1) ? ((_a0, _a1) => [0, _a0, _a1])((() => {
              const _anon0 = Bosatsu_Nat$cmp_Nat(Bosatsu_Nat$times2(Bosatsu_Nat$div2(n)), n);
              return (_anon0[0] === 1) ? [1] : [0];
            })(), _js_to_bosatsu_string("times2/div2")) : ((_a0, _a1) => [0,
            _a0,
            _a1])((() => {
              const _anon1 = Bosatsu_Nat$cmp_Nat((n => n + 1)(Bosatsu_Nat$times2(Bosatsu_Nat$div2(n))), n);
              return (_anon1[0] === 1) ? [1] : [0];
            })(), _js_to_bosatsu_string("times2/div2"));
      })()), ((_a0, _a1) => [1,
      _a0,
      _a1])(Bosatsu_Properties$forall_Prop(Bosatsu_Rand$prod_Rand(Bosatsu_NumberProps$rand_Nat, Bosatsu_NumberProps$rand_Nat), _js_to_bosatsu_string("cmp_Nat matches cmp_Int"), a => (() => {
          const n1 = a[0];
          return (() => {
            const n2 = a[1];
            return ((_a0, _a1) => [0, _a0, _a1])((() => {
                const _anon3 = Bosatsu_NumberProps$cmp_Comparison(Bosatsu_Nat$cmp_Nat(n1, n2), (Bosatsu_Nat$to_Int(n1) < Bosatsu_Nat$to_Int(n2)) ? [0] : (Bosatsu_Nat$to_Int(n1) === Bosatsu_Nat$to_Int(n2)) ? [1] : [2]);
                return (_anon3[0] === 1) ? [1] : [0];
              })(), _js_to_bosatsu_string("cmp_Nat"));
          })();
        })()), ((_a0, _a1) => [1,
        _a0,
        _a1])(Bosatsu_Properties$forall_Prop(Bosatsu_Rand$prod_Rand(Bosatsu_NumberProps$rand_Nat, Bosatsu_NumberProps$rand_Nat), _js_to_bosatsu_string("add homomorphism"), a_1 => (() => {
            const n1_1 = a_1[0];
            return (() => {
              const n2_1 = a_1[1];
              return ((_a0, _a1) => [0,
                _a0,
                _a1])((Bosatsu_Nat$to_Int(Bosatsu_Nat$add(n1_1, n2_1)) === (Bosatsu_Nat$to_Int(n1_1) + Bosatsu_Nat$to_Int(n2_1))) ? [1] : [0], _js_to_bosatsu_string("add homomorphism"));
            })();
          })()), ((_a0, _a1) => [1,
          _a0,
          _a1])(Bosatsu_Properties$forall_Prop(Bosatsu_Rand$prod_Rand(Bosatsu_NumberProps$rand_Nat, Bosatsu_NumberProps$rand_Nat), _js_to_bosatsu_string("sub_Nat homomorphism"), a_2 => (() => {
              const n1_2 = a_2[0];
              return (() => {
                const n2_2 = a_2[1];
                return (() => {
                  const i1 = Bosatsu_Nat$to_Int(n1_2);
                  return (() => {
                    const i2 = Bosatsu_Nat$to_Int(n2_2);
                    return (() => {
                      const _anon5 = (i1 < i2) ? [0] : (i1 === i2) ? [1] : [2];
                      return (_anon5[0] === 1) ? ((_a0, _a1) => [0,
                          _a0,
                          _a1])((Bosatsu_Nat$to_Int(Bosatsu_Nat$sub_Nat(n1_2, n2_2)) === (i1 - i2)) ? [1] : [0], _js_to_bosatsu_string("sub_Nat homomorphism")) : (_anon5[0] === 2) ? ((_a0, _a1) => [0,
                            _a0,
                            _a1])((Bosatsu_Nat$to_Int(Bosatsu_Nat$sub_Nat(n1_2, n2_2)) === (i1 - i2)) ? [1] : [0], _js_to_bosatsu_string("sub_Nat homomorphism")) : ((_a0, _a1) => [0,
                            _a0,
                            _a1])((() => {
                              const _anon4 = Bosatsu_Nat$sub_Nat(n1_2, n2_2);
                              return (_anon4 === 0) ? [1] : [0];
                            })(), _js_to_bosatsu_string("sub to zero"));
                    })();
                  })();
                })();
              })();
            })()), ((_a0, _a1) => [1,
            _a0,
            _a1])(Bosatsu_Properties$forall_Prop(Bosatsu_Rand$prod_Rand(Bosatsu_NumberProps$rand_Nat, Bosatsu_NumberProps$rand_Nat), _js_to_bosatsu_string("mult homomorphism"), a_3 => (() => {
                const n1_3 = a_3[0];
                return (() => {
                  const n2_3 = a_3[1];
                  return ((_a0, _a1) => [0,
                    _a0,
                    _a1])((Bosatsu_Nat$to_Int(Bosatsu_Nat$mult(n1_3, n2_3)) === (Bosatsu_Nat$to_Int(n1_3) * Bosatsu_Nat$to_Int(n2_3))) ? [1] : [0], _js_to_bosatsu_string("mult homomorphism"));
                })();
              })()), ((_a0, _a1) => [1,
              _a0,
              _a1])(Bosatsu_Properties$forall_Prop(Bosatsu_Rand$prod_Rand(Bosatsu_NumberProps$small_rand_Nat, Bosatsu_NumberProps$small_rand_Nat), _js_to_bosatsu_string("exp homomorphism"), a_4 => (() => {
                  const n1_4 = a_4[0];
                  return (() => {
                    const n2_4 = a_4[1];
                    return ((_a0, _a1) => [0,
                      _a0,
                      _a1])((Bosatsu_Nat$to_Int(Bosatsu_Nat$exp(n1_4, n2_4)) === Bosatsu_NumberProps$exp_Int(Bosatsu_Nat$to_Int(n1_4), Bosatsu_Nat$to_Int(n2_4))) ? [1] : [0], _js_to_bosatsu_string("exp homomorphism"));
                  })();
                })()), ((_a0, _a1) => [1,
                _a0,
                _a1])(Bosatsu_Properties$forall_Prop(Bosatsu_NumberProps$rand_Nat, _js_to_bosatsu_string("times2 == x -> mult(2, x)"), n_1 => ((_a0, _a1) => [0,
                    _a0,
                    _a1])((() => {
                      const _anon6 = Bosatsu_Nat$cmp_Nat(Bosatsu_Nat$times2(n_1), Bosatsu_Nat$mult(n_1, (n => n + 1)((n => n + 1)(0))));
                      return (_anon6[0] === 1) ? [1] : [0];
                    })(), _js_to_bosatsu_string("times2 == mult(2, _)"))), ((_a0, _a1) => [1,
                  _a0,
                  _a1])(Bosatsu_Properties$forall_Prop(Bosatsu_Rand$prod_Rand(Bosatsu_NumberProps$rand_Nat, Bosatsu_NumberProps$rand_Nat), _js_to_bosatsu_string("divmod homomorphism"), a_5 => (() => {
                      const n1_5 = a_5[0];
                      return (() => {
                        const n2_5 = a_5[1];
                        return (() => {
                          const _anon10 = Bosatsu_Nat$divmod(n1_5, n2_5);
                          return (() => {
                            const dn = _anon10[0];
                            return (() => {
                              const mn = _anon10[1];
                              return ((_a0, _a1) => [0, _a0, _a1])((() => {
                                  const _anon7 = ((_a0, _a1) => [_a0,
                                    _a1])((Bosatsu_Nat$to_Int(dn) === (Bosatsu_Nat$to_Int(n2_5) ? Math.trunc(Bosatsu_Nat$to_Int(n1_5) / Bosatsu_Nat$to_Int(n2_5)) : 0)) ? [1] : [0], (Bosatsu_Nat$to_Int(mn) === (Bosatsu_Nat$to_Int(n2_5) ? Bosatsu_Nat$to_Int(n1_5) % Bosatsu_Nat$to_Int(n2_5) : Bosatsu_Nat$to_Int(n1_5))) ? [1] : [0]);
                                  return (() => {
                                    let _anon9;
                                    return (() => {
                                      let _anon8;
                                      return ((() => {
                                        _anon9 = _anon7[1];
                                        return true;
                                      })() && ((() => {
                                        _anon8 = _anon7[0];
                                        return true;
                                      })() && (_anon8[0] === 1 && (_anon9[0] === 1)))) ? [1] : [0];
                                    })();
                                  })();
                                })(), _js_to_bosatsu_string("div Nat"));
                            })();
                          })();
                        })();
                      })();
                    })()), [0])))))))));
const binnat_props = Bosatsu_Properties$suite_Prop(_js_to_bosatsu_string("BinNat props"), ((_a0, _a1) => [1,
    _a0,
    _a1])(Bosatsu_Properties$forall_Prop(Bosatsu_NumberProps$rand_BinNat, _js_to_bosatsu_string("if is_even(n) then times2(div2(n)) == n"), n => (() => {
        const _anon13 = Bosatsu_BinNat$is_even(n);
        return (_anon13[0] === 1) ? (() => {
            const n1 = Bosatsu_BinNat$times2(Bosatsu_BinNat$div2(n));
            return ((_a0, _a1) => [0, _a0, _a1])((() => {
                const _anon11 = Bosatsu_BinNat$cmp_BinNat(n1, n);
                return (_anon11[0] === 1) ? [1] : [0];
              })(), _concat_String(((_a0, _a1) => [1,
                  _a0,
                  _a1])(_js_to_bosatsu_string("even, times2/div2: n = "), ((_a0, _a1) => [1,
                    _a0,
                    _a1])(_int_to_String(Bosatsu_BinNat$toInt(n)), ((_a0, _a1) => [1,
                      _a0,
                      _a1])(_js_to_bosatsu_string(", n1 = "), ((_a0, _a1) => [1,
                        _a0,
                        _a1])(_int_to_String(Bosatsu_BinNat$toInt(n1)), [0]))))));
          })() : (() => {
            const n1_1 = Bosatsu_BinNat$times2(Bosatsu_BinNat$div2(n));
            return ((_a0, _a1) => [0, _a0, _a1])((() => {
                const _anon12 = Bosatsu_BinNat$cmp_BinNat(Bosatsu_BinNat$next(n1_1), n);
                return (_anon12[0] === 1) ? [1] : [0];
              })(), _concat_String(((_a0, _a1) => [1,
                  _a0,
                  _a1])(_js_to_bosatsu_string("times2/div2: n = "), ((_a0, _a1) => [1,
                    _a0,
                    _a1])(_int_to_String(Bosatsu_BinNat$toInt(n)), ((_a0, _a1) => [1,
                      _a0,
                      _a1])(_js_to_bosatsu_string(", n1 = "), ((_a0, _a1) => [1,
                        _a0,
                        _a1])(_int_to_String(Bosatsu_BinNat$toInt(n1_1)), [0]))))));
          })();
      })()), ((_a0, _a1) => [1,
      _a0,
      _a1])(Bosatsu_Properties$forall_Prop(Bosatsu_Rand$prod_Rand(Bosatsu_NumberProps$rand_BinNat, Bosatsu_NumberProps$rand_BinNat), _js_to_bosatsu_string("cmp_BinNat matches cmp_Int"), a => (() => {
          const n1_2 = a[0];
          return (() => {
            const n2 = a[1];
            return ((_a0, _a1) => [0, _a0, _a1])((() => {
                const _anon14 = Bosatsu_NumberProps$cmp_Comparison(Bosatsu_BinNat$cmp_BinNat(n1_2, n2), (Bosatsu_BinNat$toInt(n1_2) < Bosatsu_BinNat$toInt(n2)) ? [0] : (Bosatsu_BinNat$toInt(n1_2) === Bosatsu_BinNat$toInt(n2)) ? [1] : [2]);
                return (_anon14[0] === 1) ? [1] : [0];
              })(), _js_to_bosatsu_string("cmp_BinNat"));
          })();
        })()), ((_a0, _a1) => [1,
        _a0,
        _a1])(Bosatsu_Properties$forall_Prop(Bosatsu_Rand$prod_Rand(Bosatsu_NumberProps$rand_BinNat, Bosatsu_NumberProps$rand_BinNat), _js_to_bosatsu_string("cmp_BinNat matches eq_BinNat"), a_1 => (() => {
            const n1_3 = a_1[0];
            return (() => {
              const n2_1 = a_1[1];
              return ((_a0, _a1) => [0, _a0, _a1])((() => {
                  const _anon16 = ((_a0, _a1) => [_a0, _a1])((() => {
                      const _anon15 = Bosatsu_BinNat$cmp_BinNat(n1_3, n2_1);
                      return (_anon15[0] === 1) ? [1] : [0];
                    })(), Bosatsu_BinNat$eq_BinNat(n1_3, n2_1));
                  return (() => {
                    let _anon18;
                    return (() => {
                      let _anon17;
                      return ((() => {
                        _anon18 = _anon16[1];
                        return true;
                      })() && ((() => {
                        _anon17 = _anon16[0];
                        return true;
                      })() && (_anon17[0] === 1 && (_anon18[0] === 1)))) ? [1] : (() => {
                          let _anon20;
                          return (() => {
                            let _anon19;
                            return ((() => {
                              _anon20 = _anon16[1];
                              return true;
                            })() && ((() => {
                              _anon19 = _anon16[0];
                              return true;
                            })() && (_anon19[0] === 0 && (_anon20[0] === 0)))) ? [1] : [0];
                          })();
                        })();
                    })();
                  })();
                })(), _js_to_bosatsu_string("cmp vs eq consistency"));
            })();
          })()), ((_a0, _a1) => [1,
          _a0,
          _a1])(Bosatsu_Properties$forall_Prop(Bosatsu_Rand$prod_Rand(Bosatsu_NumberProps$rand_BinNat, Bosatsu_NumberProps$rand_BinNat), _js_to_bosatsu_string("add homomorphism"), a_2 => (() => {
              const n1_4 = a_2[0];
              return (() => {
                const n2_2 = a_2[1];
                return ((_a0, _a1) => [0,
                  _a0,
                  _a1])((Bosatsu_BinNat$toInt(Bosatsu_BinNat$add_BinNat(n1_4, n2_2)) === (Bosatsu_BinNat$toInt(n1_4) + Bosatsu_BinNat$toInt(n2_2))) ? [1] : [0], _js_to_bosatsu_string("add homomorphism"));
              })();
            })()), ((_a0, _a1) => [1,
            _a0,
            _a1])(Bosatsu_Properties$forall_Prop(Bosatsu_Rand$prod_Rand(Bosatsu_NumberProps$rand_BinNat, Bosatsu_NumberProps$rand_BinNat), _js_to_bosatsu_string("sub_BinNat homomorphism"), a_3 => (() => {
                const n1_5 = a_3[0];
                return (() => {
                  const n2_3 = a_3[1];
                  return (() => {
                    const i1 = Bosatsu_BinNat$toInt(n1_5);
                    return (() => {
                      const i2 = Bosatsu_BinNat$toInt(n2_3);
                      return (() => {
                        const _anon22 = (i1 < i2) ? [0] : (i1 === i2) ? [1] : [2];
                        return (_anon22[0] === 1) ? ((_a0, _a1) => [0,
                            _a0,
                            _a1])((Bosatsu_BinNat$toInt(Bosatsu_BinNat$sub_BinNat(n1_5, n2_3)) === (i1 - i2)) ? [1] : [0], _js_to_bosatsu_string("sub_BinNat homomorphism")) : (_anon22[0] === 2) ? ((_a0, _a1) => [0,
                              _a0,
                              _a1])((Bosatsu_BinNat$toInt(Bosatsu_BinNat$sub_BinNat(n1_5, n2_3)) === (i1 - i2)) ? [1] : [0], _js_to_bosatsu_string("sub_BinNat homomorphism")) : ((_a0, _a1) => [0,
                              _a0,
                              _a1])((() => {
                                const _anon21 = Bosatsu_BinNat$sub_BinNat(n1_5, n2_3);
                                return (_anon21[0] === 0) ? [1] : [0];
                              })(), _js_to_bosatsu_string("sub to zero"));
                      })();
                    })();
                  })();
                })();
              })()), ((_a0, _a1) => [1,
              _a0,
              _a1])(Bosatsu_Properties$forall_Prop(Bosatsu_Rand$prod_Rand(Bosatsu_NumberProps$rand_BinNat, Bosatsu_NumberProps$rand_BinNat), _js_to_bosatsu_string("sub_BinNat_Option is None implies a < b"), a_4 => (() => {
                  const n1_6 = a_4[0];
                  return (() => {
                    const n2_4 = a_4[1];
                    return (() => {
                      const _anon25 = Bosatsu_BinNat$sub_Option(n1_6, n2_4);
                      return (_anon25[0] === 1) ? (() => {
                          const n3 = _anon25[1];
                          return ((_a0, _a1) => [0, _a0, _a1])((() => {
                              const _anon23 = Bosatsu_BinNat$cmp_BinNat(n3, Bosatsu_BinNat$sub_BinNat(n1_6, n2_4));
                              return (_anon23[0] === 1) ? [1] : [0];
                            })(), _js_to_bosatsu_string("sub_BinNat same as sub_BinNat_Option when Some"));
                        })() : ((_a0, _a1) => [0, _a0, _a1])((() => {
                            const _anon24 = Bosatsu_BinNat$cmp_BinNat(n1_6, n2_4);
                            return (_anon24[0] === 0) ? [1] : [0];
                          })(), _js_to_bosatsu_string("otherwise n1 < n2"));
                    })();
                  })();
                })()), ((_a0, _a1) => [1,
                _a0,
                _a1])(Bosatsu_Properties$forall_Prop(Bosatsu_Rand$prod_Rand(Bosatsu_NumberProps$rand_BinNat, Bosatsu_NumberProps$rand_BinNat), _js_to_bosatsu_string("mult homomorphism"), a_5 => (() => {
                    const n1_7 = a_5[0];
                    return (() => {
                      const n2_5 = a_5[1];
                      return ((_a0, _a1) => [0,
                        _a0,
                        _a1])((Bosatsu_BinNat$toInt(Bosatsu_BinNat$times_BinNat(n1_7, n2_5)) === (Bosatsu_BinNat$toInt(n1_7) * Bosatsu_BinNat$toInt(n2_5))) ? [1] : [0], _js_to_bosatsu_string("mult homomorphism"));
                    })();
                  })()), ((_a0, _a1) => [1,
                  _a0,
                  _a1])(Bosatsu_Properties$forall_Prop(Bosatsu_Rand$prod_Rand(Bosatsu_NumberProps$small_rand_BinNat, Bosatsu_NumberProps$small_rand_BinNat), _js_to_bosatsu_string("exp homomorphism"), a_6 => (() => {
                      const n1_8 = a_6[0];
                      return (() => {
                        const n2_6 = a_6[1];
                        return ((_a0, _a1) => [0,
                          _a0,
                          _a1])((Bosatsu_BinNat$toInt(Bosatsu_BinNat$exp(n1_8, n2_6)) === Bosatsu_NumberProps$exp_Int(Bosatsu_BinNat$toInt(n1_8), Bosatsu_BinNat$toInt(n2_6))) ? [1] : [0], _js_to_bosatsu_string("exp homomorphism"));
                      })();
                    })()), ((_a0, _a1) => [1,
                    _a0,
                    _a1])(Bosatsu_Properties$forall_Prop(Bosatsu_NumberProps$rand_BinNat, _js_to_bosatsu_string("times2 == x -> mult(2, x)"), n_1 => ((_a0, _a1) => [0,
                        _a0,
                        _a1])((() => {
                          const _anon26 = Bosatsu_BinNat$cmp_BinNat(Bosatsu_BinNat$times2(n_1), Bosatsu_BinNat$times_BinNat(n_1, (_a0 => [2,
                                _a0])([0])));
                          return (_anon26[0] === 1) ? [1] : [0];
                        })(), _js_to_bosatsu_string("times2 == mult(2, _)"))), ((_a0, _a1) => [1,
                      _a0,
                      _a1])(Bosatsu_Properties$forall_Prop(Bosatsu_Rand$prod_Rand(Bosatsu_NumberProps$rand_BinNat, Bosatsu_NumberProps$rand_BinNat), _js_to_bosatsu_string("divmod homomorphism"), a_7 => (() => {
                          const n1_9 = a_7[0];
                          return (() => {
                            const n2_7 = a_7[1];
                            return (() => {
                              const _anon30 = Bosatsu_BinNat$divmod(n1_9, n2_7);
                              return (() => {
                                const dn = _anon30[0];
                                return (() => {
                                  const mn = _anon30[1];
                                  return ((_a0, _a1) => [0, _a0, _a1])((() => {
                                      const _anon27 = ((_a0, _a1) => [_a0,
                                        _a1])((Bosatsu_BinNat$toInt(dn) === (Bosatsu_BinNat$toInt(n2_7) ? Math.trunc(Bosatsu_BinNat$toInt(n1_9) / Bosatsu_BinNat$toInt(n2_7)) : 0)) ? [1] : [0], (Bosatsu_BinNat$toInt(mn) === (Bosatsu_BinNat$toInt(n2_7) ? Bosatsu_BinNat$toInt(n1_9) % Bosatsu_BinNat$toInt(n2_7) : Bosatsu_BinNat$toInt(n1_9))) ? [1] : [0]);
                                      return (() => {
                                        let _anon29;
                                        return (() => {
                                          let _anon28;
                                          return ((() => {
                                            _anon29 = _anon27[1];
                                            return true;
                                          })() && ((() => {
                                            _anon28 = _anon27[0];
                                            return true;
                                          })() && (_anon28[0] === 1 && (_anon29[0] === 1)))) ? [1] : [0];
                                        })();
                                      })();
                                    })(), _js_to_bosatsu_string("div BinNat"));
                                })();
                              })();
                            })();
                          })();
                        })()), [0])))))))))));
const binint_props = Bosatsu_Properties$suite_Prop(_js_to_bosatsu_string("BinInt props"), ((_a0, _a1) => [1,
    _a0,
    _a1])(Bosatsu_Properties$forall_Prop(Bosatsu_Rand$prod_Rand(Bosatsu_NumberProps$rand_BinInt, Bosatsu_NumberProps$rand_BinInt), _js_to_bosatsu_string("add homomorphism"), a => (() => {
        const n1 = a[0];
        return (() => {
          const n2 = a[1];
          return ((_a0, _a1) => [0,
            _a0,
            _a1])((Bosatsu_BinInt$binInt_to_Int(Bosatsu_BinInt$add(n1, n2)) === (Bosatsu_BinInt$binInt_to_Int(n1) + Bosatsu_BinInt$binInt_to_Int(n2))) ? [1] : [0], _js_to_bosatsu_string("add BinInt"));
        })();
      })()), ((_a0, _a1) => [1,
      _a0,
      _a1])(Bosatsu_Properties$forall_Prop(Bosatsu_Rand$prod_Rand(Bosatsu_NumberProps$rand_BinInt, Bosatsu_NumberProps$rand_BinInt), _js_to_bosatsu_string("sub homomorphism"), a_1 => (() => {
          const n1_1 = a_1[0];
          return (() => {
            const n2_1 = a_1[1];
            return ((_a0, _a1) => [0,
              _a0,
              _a1])((Bosatsu_BinInt$binInt_to_Int(Bosatsu_BinInt$sub(n1_1, n2_1)) === (Bosatsu_BinInt$binInt_to_Int(n1_1) - Bosatsu_BinInt$binInt_to_Int(n2_1))) ? [1] : [0], _js_to_bosatsu_string("sub BinInt"));
          })();
        })()), ((_a0, _a1) => [1,
        _a0,
        _a1])(Bosatsu_Properties$forall_Prop(Bosatsu_NumberProps$rand_BinInt, _js_to_bosatsu_string("x + (-x) == 0"), x => ((_a0, _a1) => [0,
            _a0,
            _a1])((Bosatsu_BinInt$binInt_to_Int(Bosatsu_BinInt$add(x, Bosatsu_BinInt$negate(x))) === 0) ? [1] : [0], _js_to_bosatsu_string("x + (-x) == 0"))), ((_a0, _a1) => [1,
          _a0,
          _a1])(Bosatsu_Properties$forall_Prop(Bosatsu_NumberProps$rand_BinInt, _js_to_bosatsu_string("x + |x| == 0 or 2x"), x_1 => (() => {
              const xi = Bosatsu_BinInt$binInt_to_Int(x_1);
              return (() => {
                const _anon31 = (xi < 0) ? [0] : (xi === 0) ? [1] : [2];
                return (_anon31[0] === 2) ? ((_a0, _a1) => [0,
                    _a0,
                    _a1])((Bosatsu_BinInt$binInt_to_Int(Bosatsu_BinInt$add(x_1, Bosatsu_BinInt$binNat_to_BinInt(Bosatsu_BinInt$abs(x_1)))) === (xi + xi)) ? [1] : [0], _js_to_bosatsu_string("x + |x| == 2|x|")) : ((_a0, _a1) => [0,
                    _a0,
                    _a1])((Bosatsu_BinInt$binInt_to_Int(Bosatsu_BinInt$add(x_1, Bosatsu_BinInt$binNat_to_BinInt(Bosatsu_BinInt$abs(x_1)))) === 0) ? [1] : [0], _js_to_bosatsu_string("x + |x| == 0 if x <= 0"));
              })();
            })()), ((_a0, _a1) => [1,
            _a0,
            _a1])(Bosatsu_Properties$forall_Prop(Bosatsu_NumberProps$rand_BinInt, _js_to_bosatsu_string("x + not(x) == x - x - 1 = -1"), x_2 => (() => {
                const z = Bosatsu_BinInt$add(x_2, Bosatsu_BinInt$not(x_2));
                return (() => {
                  const neg_1 = Bosatsu_BinInt$int_to_BinInt((-1));
                  return ((_a0, _a1) => [0, _a0, _a1])((() => {
                      const _anon32 = ((_a0, _a1) => [_a0,
                        _a1])(Bosatsu_BinInt$cmp(z, neg_1), Bosatsu_BinInt$eq(z, neg_1));
                      return (() => {
                        let _anon34;
                        return (() => {
                          let _anon33;
                          return ((() => {
                            _anon34 = _anon32[1];
                            return true;
                          })() && ((() => {
                            _anon33 = _anon32[0];
                            return true;
                          })() && (_anon33[0] === 1 && (_anon34[0] === 1)))) ? [1] : [0];
                        })();
                      })();
                    })(), _js_to_bosatsu_string("x + not(x) = -1"));
                })();
              })()), [0]))))));
const all_props = ((_a0, _a1) => [1,
  _a0,
  _a1])(Bosatsu_NumberProps$int_props, ((_a0, _a1) => [1,
    _a0,
    _a1])(Bosatsu_NumberProps$nat_props, ((_a0, _a1) => [1,
      _a0,
      _a1])(Bosatsu_NumberProps$binnat_props, ((_a0, _a1) => [1,
        _a0,
        _a1])(Bosatsu_NumberProps$binint_props, [0]))));
const test = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("properties"), Bosatsu_Predef$map_List(Bosatsu_NumberProps$all_props, p => Bosatsu_Properties$run_Prop(p, 100, 123456)));
export {rand_Int};