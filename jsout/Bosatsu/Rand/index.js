// Import runtime
// require("./../../_runtime.js") or import from "./../../_runtime.js"

const bitmask_64 = 1 << 64 - 1;
const rotl = (x, k) => x << k & Bosatsu_Rand$bitmask_64 | (x >> (0 - ((-64) + k)) & Bosatsu_Rand$bitmask_64);
const next = state => (() => {
  const s0 = state[0];
  return (() => {
    const s1 = state[1];
    return (() => {
      const s2 = state[2];
      return (() => {
        const s3 = state[3];
        return (() => {
          const s2_1 = s2 ^ s0;
          return (() => {
            const s3_1 = s3 ^ s1;
            return ((_a0, _a1) => [_a0, _a1])(((_a0, _a1, _a2, _a3) => [_a0,
                _a1,
                _a2,
                _a3])(s0 ^ s3_1, s1 ^ s2_1, s2_1 ^ (s1 << 17 & Bosatsu_Rand$bitmask_64), Bosatsu_Rand$rotl(s3_1, 45) & Bosatsu_Rand$bitmask_64), (_a0 => [_a0])(Bosatsu_Rand$rotl(s1 + (s1 + (s1 + (s1 + s1))) & Bosatsu_Rand$bitmask_64, 7) + (Bosatsu_Rand$rotl(s1 + (s1 + (s1 + (s1 + s1))) & Bosatsu_Rand$bitmask_64, 7) + (Bosatsu_Rand$rotl(s1 + (s1 + (s1 + (s1 + s1))) & Bosatsu_Rand$bitmask_64, 7) + (Bosatsu_Rand$rotl(s1 + (s1 + (s1 + (s1 + s1))) & Bosatsu_Rand$bitmask_64, 7) + (Bosatsu_Rand$rotl(s1 + (s1 + (s1 + (s1 + s1))) & Bosatsu_Rand$bitmask_64, 7) + (Bosatsu_Rand$rotl(s1 + (s1 + (s1 + (s1 + s1))) & Bosatsu_Rand$bitmask_64, 7) + (Bosatsu_Rand$rotl(s1 + (s1 + (s1 + (s1 + s1))) & Bosatsu_Rand$bitmask_64, 7) + (Bosatsu_Rand$rotl(s1 + (s1 + (s1 + (s1 + s1))) & Bosatsu_Rand$bitmask_64, 7) + Bosatsu_Rand$rotl(s1 + (s1 + (s1 + (s1 + s1))) & Bosatsu_Rand$bitmask_64, 7)))))))) & Bosatsu_Rand$bitmask_64));
          })();
        })();
      })();
    })();
  })();
})();
const state_from_Int = i => (() => {
  const not_zero = (() => {
    const _anon0 = i ^ 54564800212389664567110541424720236321503446907971127334425124977866121780221;
    return (_anon0 === 0) ? 54564800212389664567110541424720236321503446907971127334425124977866121780221 : _anon0;
  })();
  return ((_a0, _a1, _a2, _a3) => [_a0,
    _a1,
    _a2,
    _a3])(not_zero & Bosatsu_Rand$bitmask_64, not_zero >> 64 & Bosatsu_Rand$bitmask_64, not_zero >> 128 & Bosatsu_Rand$bitmask_64, not_zero >> 192 & Bosatsu_Rand$bitmask_64);
})();
const map_Rand = (r, fn) => (() => {
  const fna = r[0];
  return (_a0 => [_a0])((_slots => s => (() => {
      const _anon1 = _slots[0](s);
      return (() => {
        const s1 = _anon1[0];
        return (() => {
          const a = _anon1[1];
          return ((_a0, _a1) => [_a0, _a1])(s1, _slots[1](a));
        })();
      })();
    })())([fna, fn]));
})();
const flat_map_Rand = (r, fn) => (() => {
  const fna = r[0];
  return (_a0 => [_a0])((_slots => s => (() => {
      const _anon3 = _slots[0](s);
      return (() => {
        const s1 = _anon3[0];
        return (() => {
          const a = _anon3[1];
          return (() => {
            const _anon2 = _slots[1](a);
            return (() => {
              const fnb = _anon2[0];
              return fnb(s1);
            })();
          })();
        })();
      })();
    })())([fna, fn]));
})();
const prod_Rand = (ra, rb) => (() => {
  const fna = ra[0];
  return (() => {
    const fnb = rb[0];
    return (_a0 => [_a0])((_slots => s0 => (() => {
        const _anon5 = _slots[0](s0);
        return (() => {
          const s1 = _anon5[0];
          return (() => {
            const a = _anon5[1];
            return (() => {
              const _anon4 = _slots[1](s1);
              return (() => {
                const s2 = _anon4[0];
                return (() => {
                  const b = _anon4[1];
                  return ((_a0, _a1) => [_a0, _a1])(s2, ((_a0, _a1) => [_a0,
                      _a1])(a, b));
                })();
              })();
            })();
          })();
        })();
      })())([fna, fnb]));
  })();
})();
const const_Rand = a => (_a0 => [_a0])((_slots => s => ((_a0, _a1) => [_a0,
    _a1])(s, _slots[0]))([a]));
const nat_2 = (n => n + 1)((n => n + 1)(0));
const parity = (n, i) => (n === 0) ? (() => {
    const _anon6 = i & 1;
    return (_anon6 === 1) ? [1] : [0];
  })() : (() => {
    let _anon9;
    return (() => {
      (() => {
        _anon9 = n - 1;
        return true;
      })();
      return (() => {
        const p = _anon9;
        return (() => {
          const _anon8 = parity(p, i >> Bosatsu_Nat$to_Int(Bosatsu_Nat$exp(Bosatsu_Rand$nat_2, n)));
          return (_anon8[0] === 1) ? (() => {
              const _anon7 = parity(p, i);
              return (_anon7[0] === 1) ? [0] : [1];
            })() : parity(p, i);
        })();
      })();
    })();
  })();
const six = (n => n + 1)((n => n + 1)((n => n + 1)((n => n + 1)((n => n + 1)((n => n + 1)(0))))));
const bool_Rand = (_a0 => [_a0])(s => (() => {
    const _anon10 = Bosatsu_Rand$next(s);
    return (() => {
      let _anon11;
      return (() => {
        (() => {
          _anon11 = _anon10[1];
          return true;
        })();
        return (() => {
          const s_1 = _anon10[0];
          return (() => {
            const i = _anon11[0];
            return ((_a0, _a1) => [_a0,
              _a1])(s_1, Bosatsu_Rand$parity(Bosatsu_Rand$six, i));
          })();
        })();
      })();
    })();
  })());
const run_Rand = (rand, seed) => (() => {
  const fn = rand[0];
  return (() => {
    const _anon12 = fn(Bosatsu_Rand$state_from_Int(seed));
    return _anon12[1];
  })();
})();
const sequence_Rand = rands => (() => {
  const sample = (rands_1, s, acc) => (() => {
    let _anon15;
    return (() => {
      let _anon16;
      return (() => {
        let _anon18;
        return (() => {
          let _anon20;
          return (() => {
            let _anon22;
            return (() => {
              (() => {
                _anon18 = rands_1;
                return true;
              })();
              return (() => {
                (() => {
                  _anon20 = s;
                  return true;
                })();
                return (() => {
                  (() => {
                    _anon22 = acc;
                    return true;
                  })();
                  return (() => {
                    (() => {
                      _anon15 = [1];
                      return true;
                    })();
                    return (() => {
                      while (_anon15[0] === 1) {
                        (_anon18[0] === 0) ? (() => {
                            (() => {
                              _anon15 = [0];
                              return true;
                            })();
                            return (() => {
                              (() => {
                                _anon16 = ((_a0, _a1) => [_a0,
                                  _a1])(_anon20, Bosatsu_Predef$reverse(_anon22));
                                return true;
                              })();
                              return [];
                            })();
                          })() : (() => {
                            let _anon14;
                            return (() => {
                              (() => {
                                _anon14 = _anon18[1];
                                return true;
                              })();
                              return (() => {
                                const hfn = _anon14[0];
                                return (() => {
                                  const rt = _anon18[2];
                                  return (() => {
                                    const _anon13 = hfn(_anon20);
                                    return (() => {
                                      const s1 = _anon13[0];
                                      return (() => {
                                        const h = _anon13[1];
                                        return (() => {
                                          const _anon17 = rt;
                                          return (() => {
                                            const _anon19 = s1;
                                            return (() => {
                                              const _anon21 = ((_a0, _a1) => [1,
                                                _a0,
                                                _a1])(h, _anon22);
                                              return (() => {
                                                (() => {
                                                  _anon18 = _anon17;
                                                  return true;
                                                })();
                                                return (() => {
                                                  (() => {
                                                    _anon20 = _anon19;
                                                    return true;
                                                  })();
                                                  return (() => {
                                                    (() => {
                                                      _anon22 = _anon21;
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
                          })();
                      }
                      return _anon16;
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
  return (_a0 => [_a0])((_slots => s_1 => _slots[0](_slots[1], s_1, [0]))([sample,
        rands]));
})();
const uint64_Rand = (_a0 => [_a0])(Bosatsu_Rand$next);
const bit_count = i => _int_loop(i, 0, (i_1, bits) => ((_a0, _a1) => [_a0,
    _a1])(i_1 >> 1, bits + 1));
const to_big_Int = (us, acc) => (() => {
  let _anon24;
  return (() => {
    let _anon25;
    return (() => {
      let _anon27;
      return (() => {
        let _anon29;
        return (() => {
          (() => {
            _anon27 = us;
            return true;
          })();
          return (() => {
            (() => {
              _anon29 = acc;
              return true;
            })();
            return (() => {
              (() => {
                _anon24 = [1];
                return true;
              })();
              return (() => {
                while (_anon24[0] === 1) {
                  (_anon27[0] === 0) ? (() => {
                      (() => {
                        _anon24 = [0];
                        return true;
                      })();
                      return (() => {
                        (() => {
                          _anon25 = _anon29;
                          return true;
                        })();
                        return [];
                      })();
                    })() : (() => {
                      let _anon23;
                      return (() => {
                        (() => {
                          _anon23 = _anon27[1];
                          return true;
                        })();
                        return (() => {
                          const h = _anon23[0];
                          return (() => {
                            const t = _anon27[2];
                            return (() => {
                              const _anon26 = t;
                              return (() => {
                                const _anon28 = _anon29 << 64 | h;
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
const nat30 = Bosatsu_Nat$to_Nat(30);
const resample = (rand_Int, high, uints) => (() => {
  const fn = rand_Int[0];
  return (() => {
    const boundary = (high ? Math.trunc(1 << (64 * uints) / high) : 0) * high;
    return (() => {
      const next = (_slots => (s, fuel) => (() => {
        let _anon33;
        return (() => {
          let _anon34;
          return (() => {
            let _anon36;
            return (() => {
              let _anon38;
              return (() => {
                (() => {
                  _anon36 = s;
                  return true;
                })();
                return (() => {
                  (() => {
                    _anon38 = fuel;
                    return true;
                  })();
                  return (() => {
                    (() => {
                      _anon33 = [1];
                      return true;
                    })();
                    return (() => {
                      while (_anon33[0] === 1) {
                        (_anon38 === 0) ? (() => {
                            (() => {
                              _anon33 = [0];
                              return true;
                            })();
                            return (() => {
                              (() => {
                                _anon34 = ((_a0, _a1) => [_a0,
                                  _a1])(_anon36, (-1) + _slots[0] >> 1);
                                return true;
                              })();
                              return [];
                            })();
                          })() : (() => {
                            let _anon32;
                            return (() => {
                              (() => {
                                _anon32 = _anon38 - 1;
                                return true;
                              })();
                              return (() => {
                                const n = _anon32;
                                return (() => {
                                  const _anon31 = _slots[2](_anon36);
                                  return (() => {
                                    const s1 = _anon31[0];
                                    return (() => {
                                      const i = _anon31[1];
                                      return (() => {
                                        const _anon30 = (i < _slots[1]) ? [0] : (i === _slots[1]) ? [1] : [2];
                                        return (_anon30[0] === 0) ? (() => {
                                            (() => {
                                              _anon33 = [0];
                                              return true;
                                            })();
                                            return (() => {
                                              (() => {
                                                _anon34 = ((_a0, _a1) => [_a0,
                                                  _a1])(s1, _slots[0] ? i % _slots[0] : i);
                                                return true;
                                              })();
                                              return [];
                                            })();
                                          })() : (() => {
                                            const _anon35 = s1;
                                            return (() => {
                                              const _anon37 = n;
                                              return (() => {
                                                (() => {
                                                  _anon36 = _anon35;
                                                  return true;
                                                })();
                                                return (() => {
                                                  (() => {
                                                    _anon38 = _anon37;
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
                      }
                      return _anon34;
                    })();
                  })();
                })();
              })();
            })();
          })();
        })();
      })())([high, boundary, fn]);
      return (_a0 => [_a0])((_slots => s_1 => _slots[0](s_1, Bosatsu_Rand$nat30))([next]));
    })();
  })();
})();
const const0 = (_a0 => [_a0])(s => ((_a0, _a1) => [_a0, _a1])(s, 0));
const int_range = high => (() => {
  const _anon40 = (high < 1) ? [0] : (high === 1) ? [1] : [2];
  return (_anon40[0] === 2) ? (() => {
      const bits = Bosatsu_Rand$bit_count(high);
      return (() => {
        const uint_count = 1 + (bits >> 6);
        return (() => {
          const r = Bosatsu_Rand$sequence_Rand(Bosatsu_Predef$replicate_List(Bosatsu_Rand$uint64_Rand, uint_count));
          return Bosatsu_Rand$resample((() => {
              const fna = r[0];
              return (_a0 => [_a0])((_slots => s => (() => {
                  const _anon39 = _slots[0](s);
                  return (() => {
                    const s1 = _anon39[0];
                    return (() => {
                      const a = _anon39[1];
                      return ((_a0, _a1) => [_a0,
                        _a1])(s1, Bosatsu_Rand$to_big_Int(a, 0));
                    })();
                  })();
                })())([fna]));
            })(), high, uint_count);
        })();
      })();
    })() : Bosatsu_Rand$const0;
})();
const nat_range = high => (() => {
  const r = Bosatsu_Rand$int_range(Bosatsu_Nat$to_Int(high));
  return (() => {
    const fna = r[0];
    return (_a0 => [_a0])((_slots => s => (() => {
        const _anon41 = _slots[0](s);
        return (() => {
          const s1 = _anon41[0];
          return (() => {
            const a = _anon41[1];
            return ((_a0, _a1) => [_a0, _a1])(s1, Bosatsu_Nat$to_Nat(a));
          })();
        })();
      })())([fna]));
  })();
})();
const geometric = (depth, acc) => (depth === 0) ? (_a0 => [_a0])((_slots => s => ((_a0, _a1) => [_a0,
      _a1])(s, _slots[0]))([acc])) : (() => {
    let _anon46;
    return (() => {
      (() => {
        _anon46 = depth - 1;
        return true;
      })();
      return (() => {
        const prev = _anon46;
        return (_a0 => [_a0])((_slots => s_1 => (() => {
            const _anon45 = (() => {
              const _anon42 = Bosatsu_Rand$next(s_1);
              return (() => {
                let _anon43;
                return (() => {
                  (() => {
                    _anon43 = _anon42[1];
                    return true;
                  })();
                  return (() => {
                    const s1 = _anon42[0];
                    return (() => {
                      const i = _anon43[0];
                      return ((_a0, _a1) => [_a0,
                        _a1])(s1, Bosatsu_Rand$parity(Bosatsu_Rand$six, i));
                    })();
                  })();
                })();
              })();
            })();
            return (() => {
              const s1_1 = _anon45[0];
              return (() => {
                const a = _anon45[1];
                return (() => {
                  const _anon44 = (a[0] === 1) ? (_a0 => [_a0])((_slots => s_2 => ((_a0, _a1) => [_a0,
                        _a1])(s_2, _slots[0]))([_slots[0]])) : _slots[1](_slots[2], 1 + _slots[0]);
                  return (() => {
                    const fnb = _anon44[0];
                    return fnb(s1_1);
                  })();
                })();
              })();
            })();
          })())([acc, geometric, prev]));
      })();
    })();
  })();
const geometric_Int = Bosatsu_Rand$geometric(Bosatsu_Rand$nat30, 0);
const split_at = (list, idx) => (list[0] === 0) ? ((_a0, _a1) => [_a0,
    _a1])([0], [0]) : (() => {
    const h = list[1];
    return (() => {
      const t = list[2];
      return (idx[0] === 0) ? ((_a0, _a1) => [_a0, _a1])([0], list) : (() => {
          const _anon47 = split_at(t, Bosatsu_BinNat$prev(idx));
          return (() => {
            const left = _anon47[0];
            return (() => {
              const right = _anon47[1];
              return ((_a0, _a1) => [_a0, _a1])(((_a0, _a1) => [1,
                  _a0,
                  _a1])(h, left), right);
            })();
          })();
        })();
    })();
  })();
const from_pair = (left, right) => (_a0 => [_a0])((_slots => s => (() => {
    const _anon51 = (() => {
      const _anon48 = Bosatsu_Rand$next(s);
      return (() => {
        let _anon49;
        return (() => {
          (() => {
            _anon49 = _anon48[1];
            return true;
          })();
          return (() => {
            const s1 = _anon48[0];
            return (() => {
              const i = _anon49[0];
              return ((_a0, _a1) => [_a0,
                _a1])(s1, Bosatsu_Rand$parity(Bosatsu_Rand$six, i));
            })();
          })();
        })();
      })();
    })();
    return (() => {
      const s1_1 = _anon51[0];
      return (() => {
        const a = _anon51[1];
        return (() => {
          const _anon50 = (a[0] === 1) ? _slots[0] : _slots[1];
          return (() => {
            const fnb = _anon50[0];
            return fnb(s1_1);
          })();
        })();
      })();
    })();
  })())([left, right]));
const one_of = (head, tail) => (() => {
  const loop = (_slots => (items_len, items) => (items_len[0] === 0) ? _slots[0] : (() => {
      let _anon59;
      return (items_len[0] === 1 && (() => {
        _anon59 = items_len[1];
        return true;
      })() && (_anon59[0] === 0)) ? (items[0] === 1) ? items[1] : _slots[0] : (items_len[0] === 1) ? (() => {
            const n = items_len[1];
            return (items[0] === 1) ? (() => {
                const front = items[1];
                return (() => {
                  const tail_1 = items[2];
                  return (() => {
                    const _anon54 = Bosatsu_Rand$split_at(tail_1, n);
                    return (() => {
                      const left = _anon54[0];
                      return (() => {
                        const right = _anon54[1];
                        return (() => {
                          const lrand = loop(n, left);
                          return (() => {
                            const rrand = loop(n, right);
                            return (() => {
                              const back = Bosatsu_Rand$from_pair(lrand, rrand);
                              return (() => {
                                const r = Bosatsu_Rand$int_range(Bosatsu_BinNat$toInt(items_len));
                                return (() => {
                                  const fna = r[0];
                                  return (_a0 => [_a0])((_slots => s => (() => {
                                      const _anon53 = _slots[0](s);
                                      return (() => {
                                        const s1 = _anon53[0];
                                        return (() => {
                                          const a = _anon53[1];
                                          return (() => {
                                            const _anon52 = (a === 0) ? _slots[2] : _slots[1];
                                            return (() => {
                                              const fnb = _anon52[0];
                                              return fnb(s1);
                                            })();
                                          })();
                                        })();
                                      })();
                                    })())([fna, back, front]));
                                })();
                              })();
                            })();
                          })();
                        })();
                      })();
                    })();
                  })();
                })();
              })() : _slots[0];
          })() : (() => {
            let _anon60;
            return (items_len[0] === 2 && (() => {
              _anon60 = items_len[1];
              return true;
            })() && (_anon60[0] === 0)) ? (() => {
                let _anon55;
                return (items[0] === 1 && (() => {
                  _anon55 = items[2];
                  return true;
                })() && (_anon55[0] === 1)) ? (() => {
                    const left_1 = items[1];
                    return (() => {
                      const right_1 = _anon55[1];
                      return Bosatsu_Rand$from_pair(left_1, right_1);
                    })();
                  })() : _slots[0];
              })() : (() => {
                const n_1 = items_len[1];
                return (() => {
                  let _anon58;
                  return (items[0] === 1 && (() => {
                    _anon58 = items[2];
                    return true;
                  })() && (_anon58[0] === 1)) ? (() => {
                      const f1 = items[1];
                      return (() => {
                        const f2 = _anon58[1];
                        return (() => {
                          const tail_2 = _anon58[2];
                          return (() => {
                            const front_1 = Bosatsu_Rand$from_pair(f1, f2);
                            return (() => {
                              const back_1 = loop(n_1, tail_2);
                              return (() => {
                                const r_1 = Bosatsu_Rand$int_range(Bosatsu_BinNat$toInt(items_len) >> 1);
                                return (() => {
                                  const fna_1 = r_1[0];
                                  return (_a0 => [_a0])((_slots => s_1 => (() => {
                                      const _anon57 = _slots[0](s_1);
                                      return (() => {
                                        const s1_1 = _anon57[0];
                                        return (() => {
                                          const a_1 = _anon57[1];
                                          return (() => {
                                            const _anon56 = (a_1 === 0) ? _slots[2] : _slots[1];
                                            return (() => {
                                              const fnb_1 = _anon56[0];
                                              return fnb_1(s1_1);
                                            })();
                                          })();
                                        })();
                                      })();
                                    })())([fna_1, back_1, front_1]));
                                })();
                              })();
                            })();
                          })();
                        })();
                      })();
                    })() : _slots[0];
                })();
              })();
          })();
    })())([head]);
  return (() => {
    const items_1 = ((_a0, _a1) => [1, _a0, _a1])(head, tail);
    return loop((() => {
        const loop_1 = (list, acc) => (() => {
          let _anon61;
          return (() => {
            let _anon62;
            return (() => {
              let _anon64;
              return (() => {
                let _anon66;
                return (() => {
                  (() => {
                    _anon64 = list;
                    return true;
                  })();
                  return (() => {
                    (() => {
                      _anon66 = acc;
                      return true;
                    })();
                    return (() => {
                      (() => {
                        _anon61 = [1];
                        return true;
                      })();
                      return (() => {
                        while (_anon61[0] === 1) {
                          (_anon64[0] === 0) ? (() => {
                              (() => {
                                _anon61 = [0];
                                return true;
                              })();
                              return (() => {
                                (() => {
                                  _anon62 = _anon66;
                                  return true;
                                })();
                                return [];
                              })();
                            })() : (() => {
                              const t = _anon64[2];
                              return (() => {
                                const _anon63 = t;
                                return (() => {
                                  const _anon65 = Bosatsu_BinNat$next(_anon66);
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
                        }
                        return _anon62;
                      })();
                    })();
                  })();
                })();
              })();
            })();
          })();
        })();
        return loop_1(items_1, [0]);
      })(), items_1);
  })();
})();
export {bitmask_64};