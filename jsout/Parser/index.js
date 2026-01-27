// Import runtime
// require("./../_runtime.js") or import from "./../_runtime.js"

const empty = (_a0 => [_a0])(a => [0]);
const parse = (p, str) => (() => {
  const fn = p[0];
  return (() => {
    const _anon0 = fn(str);
    return (() => {
      let _anon1;
      return (_anon0[0] === 1 && (() => {
        _anon1 = _anon0[1];
        return true;
      })()) ? (() => {
          const a = _anon1[1];
          return (_a0 => [1, _a0])(a);
        })() : [0];
    })();
  })();
})();
const expect = str => (_a0 => [_a0])((_slots => s => (() => {
    const _anon2 = _partition_String(s, _slots[0]);
    return (() => {
      let _anon3;
      return (() => {
        let _anon4;
        return (_anon2[0] === 1 && (() => {
          _anon3 = _anon2[1];
          return true;
        })() && ((() => {
          _anon4 = _anon3[0];
          return true;
        })() && (_bosatsu_to_js_string(_anon4) === ""))) ? (() => {
            const rest = _anon3[1];
            return (_a0 => [1, _a0])(((_a0, _a1) => [_a0, _a1])(rest, []));
          })() : [0];
      })();
    })();
  })())([str]));
const map = (p, fn) => (() => {
  const pfn = p[0];
  return (_a0 => [_a0])((_slots => s => (() => {
      const _anon5 = _slots[0](s);
      return (() => {
        let _anon6;
        return (_anon5[0] === 1 && (() => {
          _anon6 = _anon5[1];
          return true;
        })()) ? (() => {
            const rest = _anon6[0];
            return (() => {
              const a = _anon6[1];
              return (_a0 => [1, _a0])(((_a0, _a1) => [_a0,
                  _a1])(rest, _slots[1](a)));
            })();
          })() : [0];
      })();
    })())([pfn, fn]));
})();
const flat_map = (p, fn) => (() => {
  const fa = p[0];
  return (_a0 => [_a0])((_slots => s0 => (() => {
      const _anon8 = _slots[0](s0);
      return (() => {
        let _anon9;
        return (_anon8[0] === 1 && (() => {
          _anon9 = _anon8[1];
          return true;
        })()) ? (() => {
            const s1 = _anon9[0];
            return (() => {
              const a = _anon9[1];
              return (() => {
                const _anon7 = _slots[1](a);
                return (() => {
                  const fb = _anon7[0];
                  return fb(s1);
                })();
              })();
            })();
          })() : [0];
      })();
    })())([fa, fn]));
})();
const one_of = ps => (ps[0] === 0) ? Parser$empty : (() => {
    let _anon12;
    return (ps[0] === 1 && (() => {
      _anon12 = ps[2];
      return true;
    })() && (_anon12[0] === 0)) ? ps[1] : (() => {
        let _anon13;
        return (() => {
          (() => {
            _anon13 = ps[1];
            return true;
          })();
          return (() => {
            const headFn = _anon13[0];
            return (() => {
              const prest = ps[2];
              return (() => {
                const _anon11 = one_of(prest);
                return (() => {
                  const tailFn = _anon11[0];
                  return (_a0 => [_a0])((_slots => s => (() => {
                      const _anon10 = _slots[0](s);
                      return (_anon10[0] === 0) ? _slots[1](s) : _anon10;
                    })())([headFn, tailFn]));
                })();
              })();
            })();
          })();
        })();
      })();
  })();
const then_parse = (pa, pb) => (() => {
  const fa = pa[0];
  return (() => {
    const fb = pb[0];
    return (_a0 => [_a0])((_slots => s0 => (() => {
        const _anon16 = _slots[0](s0);
        return (() => {
          let _anon17;
          return (_anon16[0] === 1 && (() => {
            _anon17 = _anon16[1];
            return true;
          })()) ? (() => {
              const s1 = _anon17[0];
              return (() => {
                const a = _anon17[1];
                return (() => {
                  const _anon14 = _slots[1](s1);
                  return (() => {
                    let _anon15;
                    return (_anon14[0] === 1 && (() => {
                      _anon15 = _anon14[1];
                      return true;
                    })()) ? (() => {
                        const s2 = _anon15[0];
                        return (() => {
                          const b = _anon15[1];
                          return (_a0 => [1, _a0])(((_a0, _a1) => [_a0,
                              _a1])(s2, ((_a0, _a1) => [_a0, _a1])(a, b)));
                        })();
                      })() : [0];
                  })();
                })();
              })();
            })() : [0];
        })();
      })())([fa, fb]));
  })();
})();
const expect_int = i => Parser$map((() => {
    const str = _int_to_String(i);
    return (_a0 => [_a0])((_slots => s => (() => {
        const _anon18 = _partition_String(s, _slots[0]);
        return (() => {
          let _anon19;
          return (() => {
            let _anon20;
            return (_anon18[0] === 1 && (() => {
              _anon19 = _anon18[1];
              return true;
            })() && ((() => {
              _anon20 = _anon19[0];
              return true;
            })() && (_bosatsu_to_js_string(_anon20) === ""))) ? (() => {
                const rest = _anon19[1];
                return (_a0 => [1, _a0])(((_a0, _a1) => [_a0, _a1])(rest, []));
              })() : [0];
          })();
        })();
      })())([str]));
  })(), (_slots => a => _slots[0])([i]));
const digit = Parser$one_of(Bosatsu_Predef$map_List(range(10), Parser$expect_int));
const recur_max = (n, fn, _in) => (() => {
  let _anon23;
  return (() => {
    let _anon24;
    return (() => {
      let _anon26;
      return (() => {
        let _anon28;
        return (() => {
          let _anon30;
          return (() => {
            (() => {
              _anon26 = n;
              return true;
            })();
            return (() => {
              (() => {
                _anon28 = fn;
                return true;
              })();
              return (() => {
                (() => {
                  _anon30 = _in;
                  return true;
                })();
                return (() => {
                  (() => {
                    _anon23 = [1];
                    return true;
                  })();
                  return (() => {
                    while (_anon23[0] === 1) {
                      (_anon26 === 0) ? (() => {
                          (() => {
                            _anon23 = [0];
                            return true;
                          })();
                          return (() => {
                            (() => {
                              _anon24 = [0];
                              return true;
                            })();
                            return [];
                          })();
                        })() : (() => {
                          let _anon22;
                          return (() => {
                            (() => {
                              _anon22 = _anon26 - 1;
                              return true;
                            })();
                            return (() => {
                              const prev = _anon22;
                              return (() => {
                                const _anon21 = _anon28(_anon30);
                                return (_anon21[0] === 0) ? (() => {
                                    const in1 = _anon21[1];
                                    return (() => {
                                      const _anon25 = prev;
                                      return (() => {
                                        const _anon29 = in1;
                                        return (() => {
                                          (() => {
                                            _anon26 = _anon25;
                                            return true;
                                          })();
                                          return (() => {
                                            (() => {
                                              _anon30 = _anon29;
                                              return true;
                                            })();
                                            return [];
                                          })();
                                        })();
                                      })();
                                    })();
                                  })() : (() => {
                                    (() => {
                                      _anon23 = [0];
                                      return true;
                                    })();
                                    return (() => {
                                      (() => {
                                        _anon24 = (() => {
                                          const out = _anon21[1];
                                          return (_a0 => [1, _a0])(out);
                                        })();
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
                    return _anon24;
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
const digits_n = at_most => (() => {
  const fn = Parser$digit[0];
  return (_a0 => [_a0])((_slots => s => (() => {
      const _anon35 = (() => {
        const _in = ((_a0, _a1) => [_a0, _a1])(s, (-1));
        return Parser$recur_max((n => n + 1)(_int_loop(_slots[0], 0, (i, nat) => ((_a0, _a1) => [_a0,
                _a1])((-1) + i, (n => n + 1)(nat)))), (_slots => _in_1 => (() => {
            const s0 = _in_1[0];
            return (() => {
              const acc0 = _in_1[1];
              return (() => {
                const _anon33 = _slots[0](s0);
                return (() => {
                  let _anon34;
                  return (_anon33[0] === 1 && (() => {
                    _anon34 = _anon33[1];
                    return true;
                  })()) ? (() => {
                      const s1 = _anon34[0];
                      return (() => {
                        const d = _anon34[1];
                        return (() => {
                          const acc = (() => {
                            const _anon31 = (acc0 === (-1)) ? [1] : [0];
                            return (_anon31[0] === 1) ? 0 : acc0;
                          })();
                          return (_a0 => [0, _a0])(((_a0, _a1) => [_a0,
                              _a1])(s1, acc + (acc + (acc + (acc + (acc + (acc + (acc + (acc + (acc + (acc + d)))))))))));
                        })();
                      })();
                    })() : (() => {
                      const _anon32 = (acc0 === (-1)) ? [1] : [0];
                      return (_anon32[0] === 1) ? (_a0 => [1,
                          _a0])([0]) : (_a0 => [1, _a0])((_a0 => [1,
                            _a0])(((_a0, _a1) => [_a0, _a1])(s0, acc0)));
                    })();
                })();
              })();
            })();
          })())([_slots[1]]), _in);
      })();
      return (() => {
        let _anon36;
        return (_anon35[0] === 1 && (() => {
          _anon36 = _anon35[1];
          return true;
        })() && (_anon36[0] === 1)) ? (() => {
            const a = _anon36[1];
            return (_a0 => [1, _a0])(a);
          })() : [0];
      })();
    })())([at_most, fn]));
})();
const tests = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("Parser tests"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
        const _anon39 = (() => {
          const _anon37 = (_a0 => [1, _a0])(((_a0, _a1) => [_a0,
              _a1])(_js_to_bosatsu_string("foo"), 1));
          return (() => {
            let _anon38;
            return (_anon37[0] === 1 && (() => {
              _anon38 = _anon37[1];
              return true;
            })()) ? (() => {
                const a = _anon38[1];
                return (_a0 => [1, _a0])(a);
              })() : [0];
          })();
        })();
        return (() => {
          let _anon40;
          return (_anon39[0] === 1 && (() => {
            _anon40 = _anon39[1];
            return true;
          })() && (_anon40 === 1)) ? [1] : [0];
        })();
      })(), _js_to_bosatsu_string("pure")), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
          const _anon46 = (() => {
            const _anon44 = (() => {
              const _anon41 = _partition_String(_js_to_bosatsu_string("foo"), _js_to_bosatsu_string("fo"));
              return (() => {
                let _anon42;
                return (() => {
                  let _anon43;
                  return (_anon41[0] === 1 && (() => {
                    _anon42 = _anon41[1];
                    return true;
                  })() && ((() => {
                    _anon43 = _anon42[0];
                    return true;
                  })() && (_bosatsu_to_js_string(_anon43) === ""))) ? (() => {
                      const rest = _anon42[1];
                      return (_a0 => [1, _a0])(((_a0, _a1) => [_a0,
                          _a1])(rest, []));
                    })() : [0];
                })();
              })();
            })();
            return (() => {
              let _anon45;
              return (_anon44[0] === 1 && (() => {
                _anon45 = _anon44[1];
                return true;
              })()) ? (() => {
                  const a_1 = _anon45[1];
                  return (_a0 => [1, _a0])(a_1);
                })() : [0];
            })();
          })();
          return (_anon46[0] === 1) ? [1] : [0];
        })(), _js_to_bosatsu_string("expect(fo)")), ((_a0, _a1) => [1,
        _a0,
        _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
            const _anon56 = (() => {
              const _anon55 = Parser$then_parse((_a0 => [_a0])(s => (() => {
                    const _anon47 = _partition_String(s, _js_to_bosatsu_string("fo"));
                    return (() => {
                      let _anon48;
                      return (() => {
                        let _anon49;
                        return (_anon47[0] === 1 && (() => {
                          _anon48 = _anon47[1];
                          return true;
                        })() && ((() => {
                          _anon49 = _anon48[0];
                          return true;
                        })() && (_bosatsu_to_js_string(_anon49) === ""))) ? (() => {
                            const rest_1 = _anon48[1];
                            return (_a0 => [1, _a0])(((_a0, _a1) => [_a0,
                                _a1])(rest_1, []));
                          })() : [0];
                      })();
                    })();
                  })()), (_a0 => [_a0])(s_1 => (() => {
                    const _anon50 = _partition_String(s_1, _js_to_bosatsu_string("o"));
                    return (() => {
                      let _anon51;
                      return (() => {
                        let _anon52;
                        return (_anon50[0] === 1 && (() => {
                          _anon51 = _anon50[1];
                          return true;
                        })() && ((() => {
                          _anon52 = _anon51[0];
                          return true;
                        })() && (_bosatsu_to_js_string(_anon52) === ""))) ? (() => {
                            const rest_2 = _anon51[1];
                            return (_a0 => [1, _a0])(((_a0, _a1) => [_a0,
                                _a1])(rest_2, []));
                          })() : [0];
                      })();
                    })();
                  })()));
              return (() => {
                const fn = _anon55[0];
                return (() => {
                  const _anon53 = fn(_js_to_bosatsu_string("foo"));
                  return (() => {
                    let _anon54;
                    return (_anon53[0] === 1 && (() => {
                      _anon54 = _anon53[1];
                      return true;
                    })()) ? (() => {
                        const a_2 = _anon54[1];
                        return (_a0 => [1, _a0])(a_2);
                      })() : [0];
                  })();
                })();
              })();
            })();
            return (_anon56[0] === 1) ? [1] : [0];
          })(), _js_to_bosatsu_string("expect(fo)")), ((_a0, _a1) => [1,
          _a0,
          _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
              const _anon59 = (() => {
                const fn_1 = Parser$digit[0];
                return (() => {
                  const _anon57 = fn_1(_js_to_bosatsu_string("2"));
                  return (() => {
                    let _anon58;
                    return (_anon57[0] === 1 && (() => {
                      _anon58 = _anon57[1];
                      return true;
                    })()) ? (() => {
                        const a_3 = _anon58[1];
                        return (_a0 => [1, _a0])(a_3);
                      })() : [0];
                  })();
                })();
              })();
              return (() => {
                let _anon60;
                return (_anon59[0] === 1 && (() => {
                  _anon60 = _anon59[1];
                  return true;
                })() && (_anon60 === 2)) ? [1] : [0];
              })();
            })(), _js_to_bosatsu_string("digit.parse(2)")), ((_a0, _a1) => [1,
            _a0,
            _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                const _anon63 = (() => {
                  const fn_2 = Parser$digit[0];
                  return (() => {
                    const _anon61 = fn_2(_js_to_bosatsu_string("9"));
                    return (() => {
                      let _anon62;
                      return (_anon61[0] === 1 && (() => {
                        _anon62 = _anon61[1];
                        return true;
                      })()) ? (() => {
                          const a_4 = _anon62[1];
                          return (_a0 => [1, _a0])(a_4);
                        })() : [0];
                    })();
                  })();
                })();
                return (() => {
                  let _anon64;
                  return (_anon63[0] === 1 && (() => {
                    _anon64 = _anon63[1];
                    return true;
                  })() && (_anon64 === 9)) ? [1] : [0];
                })();
              })(), _js_to_bosatsu_string("digit.parse(2)")), ((_a0, _a1) => [1,
              _a0,
              _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                  const _anon68 = (() => {
                    const _anon67 = Parser$digits_n(10);
                    return (() => {
                      const fn_3 = _anon67[0];
                      return (() => {
                        const _anon65 = fn_3(_js_to_bosatsu_string("4242"));
                        return (() => {
                          let _anon66;
                          return (_anon65[0] === 1 && (() => {
                            _anon66 = _anon65[1];
                            return true;
                          })()) ? (() => {
                              const a_5 = _anon66[1];
                              return (_a0 => [1, _a0])(a_5);
                            })() : [0];
                        })();
                      })();
                    })();
                  })();
                  return (() => {
                    let _anon69;
                    return (_anon68[0] === 1 && (() => {
                      _anon69 = _anon68[1];
                      return true;
                    })() && (_anon69 === 4242)) ? [1] : [0];
                  })();
                })(), _js_to_bosatsu_string("digits_n(10).parse(4242)")), ((_a0, _a1) => [1,
                _a0,
                _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                    const _anon73 = (() => {
                      const _anon72 = Parser$digits_n(10);
                      return (() => {
                        const fn_4 = _anon72[0];
                        return (() => {
                          const _anon70 = fn_4(_js_to_bosatsu_string("4242"));
                          return (() => {
                            let _anon71;
                            return (_anon70[0] === 1 && (() => {
                              _anon71 = _anon70[1];
                              return true;
                            })()) ? (() => {
                                const a_6 = _anon71[1];
                                return (_a0 => [1, _a0])(a_6);
                              })() : [0];
                          })();
                        })();
                      })();
                    })();
                    return (() => {
                      let _anon74;
                      return (_anon73[0] === 1 && (() => {
                        _anon74 = _anon73[1];
                        return true;
                      })() && (_anon74 === 4242)) ? [1] : [0];
                    })();
                  })(), _js_to_bosatsu_string("digits_n(10).parse(4242)")), ((_a0, _a1) => [1,
                  _a0,
                  _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                      const _anon78 = (() => {
                        const _anon77 = Parser$digits_n(3);
                        return (() => {
                          const fn_5 = _anon77[0];
                          return (() => {
                            const _anon75 = fn_5(_js_to_bosatsu_string("4242"));
                            return (() => {
                              let _anon76;
                              return (_anon75[0] === 1 && (() => {
                                _anon76 = _anon75[1];
                                return true;
                              })()) ? (() => {
                                  const a_7 = _anon76[1];
                                  return (_a0 => [1, _a0])(a_7);
                                })() : [0];
                            })();
                          })();
                        })();
                      })();
                      return (_anon78[0] === 0) ? [1] : [0];
                    })(), _js_to_bosatsu_string("digits_n(3).parse(4242)")), ((_a0, _a1) => [1,
                    _a0,
                    _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                        const _anon82 = (() => {
                          const _anon81 = Parser$digits_n(4);
                          return (() => {
                            const fn_6 = _anon81[0];
                            return (() => {
                              const _anon79 = fn_6(_js_to_bosatsu_string("4242"));
                              return (() => {
                                let _anon80;
                                return (_anon79[0] === 1 && (() => {
                                  _anon80 = _anon79[1];
                                  return true;
                                })()) ? (() => {
                                    const a_8 = _anon80[1];
                                    return (_a0 => [1, _a0])(a_8);
                                  })() : [0];
                              })();
                            })();
                          })();
                        })();
                        return (() => {
                          let _anon83;
                          return (_anon82[0] === 1 && (() => {
                            _anon83 = _anon82[1];
                            return true;
                          })() && (_anon83 === 4242)) ? [1] : [0];
                        })();
                      })(), _js_to_bosatsu_string("digits_n(4).parse(4242)")), [0]))))))))));
export {empty};