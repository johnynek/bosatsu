// Import runtime
// require("./../../_runtime.js") or import from "./../../_runtime.js"

const any = as => (() => {
  let _anon0;
  return (() => {
    let _anon1;
    return (() => {
      let _anon2;
      return (() => {
        let _anon4;
        return (() => {
          const _anon3 = (() => {
            (() => {
              _anon2 = [0];
              return true;
            })();
            return (() => {
              (() => {
                _anon4 = as;
                return true;
              })();
              return (() => {
                while (_anon4[0] === 1) {
                  (() => {
                    (() => {
                      _anon0 = _anon4;
                      return true;
                    })();
                    return (_anon0[0] === 1 && (() => {
                      _anon1 = _anon0[1];
                      return true;
                    })() && (_anon1[0] === 1)) ? (() => {
                        (() => {
                          _anon4 = [0];
                          return true;
                        })();
                        return (() => {
                          (() => {
                            _anon2 = [1];
                            return true;
                          })();
                          return [];
                        })();
                      })() : (() => {
                        (() => {
                          _anon4 = _anon4[2];
                          return true;
                        })();
                        return [];
                      })();
                  })();
                }
                return _anon2;
              })();
            })();
          })();
          return _anon2[0] === 1;
        })();
      })();
    })() ? [1] : [0];
  })();
})();
const for_all = (xs, fn) => (() => {
  let _anon6;
  return (() => {
    let _anon7;
    return (() => {
      let _anon9;
      return (() => {
        let _anon11;
        return (() => {
          (() => {
            _anon9 = xs;
            return true;
          })();
          return (() => {
            (() => {
              _anon11 = fn;
              return true;
            })();
            return (() => {
              (() => {
                _anon6 = [1];
                return true;
              })();
              return (() => {
                while (_anon6[0] === 1) {
                  (_anon9[0] === 0) ? (() => {
                      (() => {
                        _anon6 = [0];
                        return true;
                      })();
                      return (() => {
                        (() => {
                          _anon7 = [1];
                          return true;
                        })();
                        return [];
                      })();
                    })() : (() => {
                      const head = _anon9[1];
                      return (() => {
                        const tail = _anon9[2];
                        return (() => {
                          const _anon5 = _anon11(head);
                          return (_anon5[0] === 1) ? (() => {
                              const _anon8 = tail;
                              return (() => {
                                (() => {
                                  _anon9 = _anon8;
                                  return true;
                                })();
                                return [];
                              })();
                            })() : (() => {
                              (() => {
                                _anon6 = [0];
                                return true;
                              })();
                              return (() => {
                                (() => {
                                  _anon7 = [0];
                                  return true;
                                })();
                                return [];
                              })();
                            })();
                        })();
                      })();
                    })();
                }
                return _anon7;
              })();
            })();
          })();
        })();
      })();
    })();
  })();
})();
const sum = as => foldl_List(as, 0, (_a0, _a1) => _a0 + _a1);
const exists = (xs, fn) => (() => {
  let _anon13;
  return (() => {
    let _anon14;
    return (() => {
      let _anon16;
      return (() => {
        let _anon18;
        return (() => {
          (() => {
            _anon16 = xs;
            return true;
          })();
          return (() => {
            (() => {
              _anon18 = fn;
              return true;
            })();
            return (() => {
              (() => {
                _anon13 = [1];
                return true;
              })();
              return (() => {
                while (_anon13[0] === 1) {
                  (_anon16[0] === 0) ? (() => {
                      (() => {
                        _anon13 = [0];
                        return true;
                      })();
                      return (() => {
                        (() => {
                          _anon14 = [0];
                          return true;
                        })();
                        return [];
                      })();
                    })() : (() => {
                      const head = _anon16[1];
                      return (() => {
                        const tail = _anon16[2];
                        return (() => {
                          const _anon12 = _anon18(head);
                          return (_anon12[0] === 1) ? (() => {
                              (() => {
                                _anon13 = [0];
                                return true;
                              })();
                              return (() => {
                                (() => {
                                  _anon14 = [1];
                                  return true;
                                })();
                                return [];
                              })();
                            })() : (() => {
                              const _anon15 = tail;
                              return (() => {
                                (() => {
                                  _anon16 = _anon15;
                                  return true;
                                })();
                                return [];
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
})();
const uncons = xs => (xs[0] === 0) ? [0] : (() => {
    const h = xs[1];
    return (() => {
      const t = xs[2];
      return (_a0 => [1, _a0])(((_a0, _a1) => [_a0, _a1])(h, t));
    })();
  })();
const head = xs => (xs[0] === 0) ? [0] : (() => {
    const h = xs[1];
    return (_a0 => [1, _a0])(h);
  })();
const eq_List = fn => (_slots => (a, b) => (a[0] === 0) ? (b[0] === 0) ? [1] : [0] : (() => {
    const ah = a[1];
    return (() => {
      const at = a[2];
      return (b[0] === 0) ? [0] : (() => {
          const bh = b[1];
          return (() => {
            const bt = b[2];
            return (() => {
              const _anon19 = _slots[1](ah, bh);
              return (_anon19[0] === 1) ? _slots[0](_slots[1])(at, bt) : [0];
            })();
          })();
        })();
    })();
  })())([eq_List, fn]);
const zip = (left, right) => (left[0] === 0) ? [0] : (() => {
    const ah = left[1];
    return (() => {
      const at = left[2];
      return (right[0] === 0) ? [0] : (() => {
          const bh = right[1];
          return (() => {
            const bt = right[2];
            return ((_a0, _a1) => [1, _a0, _a1])(((_a0, _a1) => [_a0,
                _a1])(ah, bh), zip(at, bt));
          })();
        })();
    })();
  })();
const size1 = (list, acc) => (() => {
  let _anon20;
  return (() => {
    let _anon21;
    return (() => {
      let _anon23;
      return (() => {
        let _anon25;
        return (() => {
          (() => {
            _anon23 = list;
            return true;
          })();
          return (() => {
            (() => {
              _anon25 = acc;
              return true;
            })();
            return (() => {
              (() => {
                _anon20 = [1];
                return true;
              })();
              return (() => {
                while (_anon20[0] === 1) {
                  (_anon23[0] === 0) ? (() => {
                      (() => {
                        _anon20 = [0];
                        return true;
                      })();
                      return (() => {
                        (() => {
                          _anon21 = _anon25;
                          return true;
                        })();
                        return [];
                      })();
                    })() : (() => {
                      const t = _anon23[2];
                      return (() => {
                        const _anon22 = t;
                        return (() => {
                          const _anon24 = (n => n + 1)(_anon25);
                          return (() => {
                            (() => {
                              _anon23 = _anon22;
                              return true;
                            })();
                            return (() => {
                              (() => {
                                _anon25 = _anon24;
                                return true;
                              })();
                              return [];
                            })();
                          })();
                        })();
                      })();
                    })();
                }
                return _anon21;
              })();
            })();
          })();
        })();
      })();
    })();
  })();
})();
const size = list => Bosatsu_List$size1(list, 0);
const sort = (ord, list) => (() => {
  const to_Fn = ord[0];
  return (() => {
    const loop = (_slots => (list_1, sz) => (sz === 0) ? list_1 : (() => {
        let _anon30;
        return (() => {
          (() => {
            _anon30 = sz - 1;
            return true;
          })();
          return (() => {
            const n = _anon30;
            return (list_1[0] === 0) ? [0] : (() => {
                const h = list_1[1];
                return (() => {
                  const t = list_1[2];
                  return (() => {
                    const lesser = flat_map_List(t, (_slots => ta => (() => {
                        const _anon27 = (() => {
                          const _anon26 = _slots[0](ta, _slots[1]);
                          return (_anon26[0] === 0) ? [1] : [0];
                        })();
                        return (_anon27[0] === 1) ? ((_a0, _a1) => [1,
                            _a0,
                            _a1])(ta, [0]) : [0];
                      })())([_slots[0], h]));
                    return Bosatsu_Predef$concat(loop(lesser, n), ((_a0, _a1) => [1,
                        _a0,
                        _a1])(h, loop(flat_map_List(t, (_slots => ta_1 => (() => {
                              const _anon29 = (() => {
                                const _anon28 = _slots[0](ta_1, _slots[1]);
                                return (_anon28[0] === 2) ? [1] : (_anon28[0] === 1) ? [1] : [0];
                              })();
                              return (_anon29[0] === 1) ? ((_a0, _a1) => [1,
                                  _a0,
                                  _a1])(ta_1, [0]) : [0];
                            })())([_slots[0], h])), n)));
                  })();
                })();
              })();
          })();
        })();
      })())([to_Fn]);
    return loop(list, Bosatsu_List$size1(list, 0));
  })();
})();
const op_59984 = Bosatsu_List$eq_List((_a0, _a1) => (_a0 === _a1) ? [1] : [0]);
const headTest = ((_a0, _a1) => [0, _a0, _a1])((() => {
    const _anon31 = (_a0 => [1, _a0])(1);
    return (() => {
      let _anon32;
      return (_anon31[0] === 1 && (() => {
        _anon32 = _anon31[1];
        return true;
      })() && (_anon32 === 1)) ? [1] : [0];
    })();
  })(), _js_to_bosatsu_string("head test"));
const unconsTest = ((_a0, _a1) => [0, _a0, _a1])((() => {
    const _anon33 = (_a0 => [1, _a0])(((_a0, _a1) => [_a0,
        _a1])(1, ((_a0, _a1) => [1, _a0, _a1])(2, ((_a0, _a1) => [1,
            _a0,
            _a1])(3, [0]))));
    return (() => {
      let _anon34;
      return (() => {
        let _anon36;
        return (() => {
          let _anon35;
          return (() => {
            let _anon38;
            return (() => {
              let _anon37;
              return (() => {
                let _anon40;
                return (() => {
                  let _anon39;
                  return (_anon33[0] === 1 && (() => {
                    _anon34 = _anon33[1];
                    return true;
                  })() && ((() => {
                    _anon36 = _anon34[1];
                    return true;
                  })() && ((() => {
                    _anon35 = _anon34[0];
                    return true;
                  })() && (_anon35 === 1 && (_anon36[0] === 1 && (() => {
                    _anon37 = _anon36[1];
                    return true;
                  })() && (() => {
                    _anon38 = _anon36[2];
                    return true;
                  })() && (_anon37 === 2 && (_anon38[0] === 1 && (() => {
                    _anon39 = _anon38[1];
                    return true;
                  })() && (() => {
                    _anon40 = _anon38[2];
                    return true;
                  })() && (_anon39 === 3 && (_anon40[0] === 0))))))))) ? [1] : [0];
                })();
              })();
            })();
          })();
        })();
      })();
    })();
  })(), _js_to_bosatsu_string("uncons test"));
const zipTest = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("zip tests"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
        const _anon41 = Bosatsu_List$zip(((_a0, _a1) => [1,
            _a0,
            _a1])(1, ((_a0, _a1) => [1, _a0, _a1])(2, [0])), ((_a0, _a1) => [1,
            _a0,
            _a1])(_js_to_bosatsu_string("1"), ((_a0, _a1) => [1,
              _a0,
              _a1])(_js_to_bosatsu_string("2"), [0])));
        return (() => {
          let _anon45;
          return (() => {
            let _anon42;
            return (() => {
              let _anon44;
              return (() => {
                let _anon43;
                return (() => {
                  let _anon49;
                  return (() => {
                    let _anon46;
                    return (() => {
                      let _anon48;
                      return (() => {
                        let _anon47;
                        return (_anon41[0] === 1 && (() => {
                          _anon42 = _anon41[1];
                          return true;
                        })() && (() => {
                          _anon45 = _anon41[2];
                          return true;
                        })() && ((() => {
                          _anon44 = _anon42[1];
                          return true;
                        })() && ((() => {
                          _anon43 = _anon42[0];
                          return true;
                        })() && (_anon43 === 1 && (_bosatsu_to_js_string(_anon44) === "1"))) && (_anon45[0] === 1 && (() => {
                          _anon46 = _anon45[1];
                          return true;
                        })() && (() => {
                          _anon49 = _anon45[2];
                          return true;
                        })() && ((() => {
                          _anon48 = _anon46[1];
                          return true;
                        })() && ((() => {
                          _anon47 = _anon46[0];
                          return true;
                        })() && (_anon47 === 2 && (_bosatsu_to_js_string(_anon48) === "2"))) && (_anon49[0] === 0))))) ? [1] : [0];
                      })();
                    })();
                  })();
                })();
              })();
            })();
          })();
        })();
      })(), _js_to_bosatsu_string("same size")), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
          const _anon50 = Bosatsu_List$zip(((_a0, _a1) => [1,
              _a0,
              _a1])(1, [0]), ((_a0, _a1) => [1,
              _a0,
              _a1])(_js_to_bosatsu_string("1"), ((_a0, _a1) => [1,
                _a0,
                _a1])(_js_to_bosatsu_string("2"), [0])));
          return (() => {
            let _anon54;
            return (() => {
              let _anon51;
              return (() => {
                let _anon53;
                return (() => {
                  let _anon52;
                  return (_anon50[0] === 1 && (() => {
                    _anon51 = _anon50[1];
                    return true;
                  })() && (() => {
                    _anon54 = _anon50[2];
                    return true;
                  })() && ((() => {
                    _anon53 = _anon51[1];
                    return true;
                  })() && ((() => {
                    _anon52 = _anon51[0];
                    return true;
                  })() && (_anon52 === 1 && (_bosatsu_to_js_string(_anon53) === "1"))) && (_anon54[0] === 0))) ? [1] : [0];
                })();
              })();
            })();
          })();
        })(), _js_to_bosatsu_string("left smaller")), ((_a0, _a1) => [1,
        _a0,
        _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
            const _anon55 = Bosatsu_List$zip(((_a0, _a1) => [1,
                _a0,
                _a1])(1, ((_a0, _a1) => [1,
                  _a0,
                  _a1])(2, [0])), ((_a0, _a1) => [1,
                _a0,
                _a1])(_js_to_bosatsu_string("1"), [0]));
            return (() => {
              let _anon59;
              return (() => {
                let _anon56;
                return (() => {
                  let _anon58;
                  return (() => {
                    let _anon57;
                    return (_anon55[0] === 1 && (() => {
                      _anon56 = _anon55[1];
                      return true;
                    })() && (() => {
                      _anon59 = _anon55[2];
                      return true;
                    })() && ((() => {
                      _anon58 = _anon56[1];
                      return true;
                    })() && ((() => {
                      _anon57 = _anon56[0];
                      return true;
                    })() && (_anon57 === 1 && (_bosatsu_to_js_string(_anon58) === "1"))) && (_anon59[0] === 0))) ? [1] : [0];
                  })();
                })();
              })();
            })();
          })(), _js_to_bosatsu_string("right smaller")), [0]))));
const sortTest = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("sort tests"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
        const _anon60 = Bosatsu_List$sort((_a0 => [_a0])((_a0, _a1) => (_a0 < _a1) ? [0] : (_a0 === _a1) ? [1] : [2]), ((_a0, _a1) => [1,
            _a0,
            _a1])(3, ((_a0, _a1) => [1, _a0, _a1])(1, ((_a0, _a1) => [1,
                _a0,
                _a1])(2, [0]))));
        return (() => {
          let _anon62;
          return (() => {
            let _anon61;
            return (() => {
              let _anon64;
              return (() => {
                let _anon63;
                return (() => {
                  let _anon66;
                  return (() => {
                    let _anon65;
                    return (_anon60[0] === 1 && (() => {
                      _anon61 = _anon60[1];
                      return true;
                    })() && (() => {
                      _anon62 = _anon60[2];
                      return true;
                    })() && (_anon61 === 1 && (_anon62[0] === 1 && (() => {
                      _anon63 = _anon62[1];
                      return true;
                    })() && (() => {
                      _anon64 = _anon62[2];
                      return true;
                    })() && (_anon63 === 2 && (_anon64[0] === 1 && (() => {
                      _anon65 = _anon64[1];
                      return true;
                    })() && (() => {
                      _anon66 = _anon64[2];
                      return true;
                    })() && (_anon65 === 3 && (_anon66[0] === 0))))))) ? [1] : [0];
                  })();
                })();
              })();
            })();
          })();
        })();
      })(), _js_to_bosatsu_string("3, 1, 2")), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
          const _anon67 = Bosatsu_List$sort((_a0 => [_a0])((_a0, _a1) => (_a0 < _a1) ? [0] : (_a0 === _a1) ? [1] : [2]), ((_a0, _a1) => [1,
              _a0,
              _a1])(1, ((_a0, _a1) => [1, _a0, _a1])(2, ((_a0, _a1) => [1,
                  _a0,
                  _a1])(3, [0]))));
          return (() => {
            let _anon69;
            return (() => {
              let _anon68;
              return (() => {
                let _anon71;
                return (() => {
                  let _anon70;
                  return (() => {
                    let _anon73;
                    return (() => {
                      let _anon72;
                      return (_anon67[0] === 1 && (() => {
                        _anon68 = _anon67[1];
                        return true;
                      })() && (() => {
                        _anon69 = _anon67[2];
                        return true;
                      })() && (_anon68 === 1 && (_anon69[0] === 1 && (() => {
                        _anon70 = _anon69[1];
                        return true;
                      })() && (() => {
                        _anon71 = _anon69[2];
                        return true;
                      })() && (_anon70 === 2 && (_anon71[0] === 1 && (() => {
                        _anon72 = _anon71[1];
                        return true;
                      })() && (() => {
                        _anon73 = _anon71[2];
                        return true;
                      })() && (_anon72 === 3 && (_anon73[0] === 0))))))) ? [1] : [0];
                    })();
                  })();
                })();
              })();
            })();
          })();
        })(), _js_to_bosatsu_string("1, 2, 3")), ((_a0, _a1) => [1,
        _a0,
        _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
            const _anon74 = Bosatsu_List$sort((_a0 => [_a0])((_a0, _a1) => (_a0 < _a1) ? [0] : (_a0 === _a1) ? [1] : [2]), ((_a0, _a1) => [1,
                _a0,
                _a1])(2, ((_a0, _a1) => [1, _a0, _a1])(3, ((_a0, _a1) => [1,
                    _a0,
                    _a1])(1, [0]))));
            return (() => {
              let _anon76;
              return (() => {
                let _anon75;
                return (() => {
                  let _anon78;
                  return (() => {
                    let _anon77;
                    return (() => {
                      let _anon80;
                      return (() => {
                        let _anon79;
                        return (_anon74[0] === 1 && (() => {
                          _anon75 = _anon74[1];
                          return true;
                        })() && (() => {
                          _anon76 = _anon74[2];
                          return true;
                        })() && (_anon75 === 1 && (_anon76[0] === 1 && (() => {
                          _anon77 = _anon76[1];
                          return true;
                        })() && (() => {
                          _anon78 = _anon76[2];
                          return true;
                        })() && (_anon77 === 2 && (_anon78[0] === 1 && (() => {
                          _anon79 = _anon78[1];
                          return true;
                        })() && (() => {
                          _anon80 = _anon78[2];
                          return true;
                        })() && (_anon79 === 3 && (_anon80[0] === 0))))))) ? [1] : [0];
                      })();
                    })();
                  })();
                })();
              })();
            })();
          })(), _js_to_bosatsu_string("2, 3, 1")), ((_a0, _a1) => [1,
          _a0,
          _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
              const _anon81 = Bosatsu_List$sort((_a0 => [_a0])((_a0, _a1) => (_a0 < _a1) ? [0] : (_a0 === _a1) ? [1] : [2]), ((_a0, _a1) => [1,
                  _a0,
                  _a1])(1, ((_a0, _a1) => [1, _a0, _a1])(2, ((_a0, _a1) => [1,
                      _a0,
                      _a1])(1, [0]))));
              return (() => {
                let _anon83;
                return (() => {
                  let _anon82;
                  return (() => {
                    let _anon85;
                    return (() => {
                      let _anon84;
                      return (() => {
                        let _anon87;
                        return (() => {
                          let _anon86;
                          return (_anon81[0] === 1 && (() => {
                            _anon82 = _anon81[1];
                            return true;
                          })() && (() => {
                            _anon83 = _anon81[2];
                            return true;
                          })() && (_anon82 === 1 && (_anon83[0] === 1 && (() => {
                            _anon84 = _anon83[1];
                            return true;
                          })() && (() => {
                            _anon85 = _anon83[2];
                            return true;
                          })() && (_anon84 === 1 && (_anon85[0] === 1 && (() => {
                            _anon86 = _anon85[1];
                            return true;
                          })() && (() => {
                            _anon87 = _anon85[2];
                            return true;
                          })() && (_anon86 === 2 && (_anon87[0] === 0))))))) ? [1] : [0];
                        })();
                      })();
                    })();
                  })();
                })();
              })();
            })(), _js_to_bosatsu_string("1, 2, 1")), [0])))));
const stringTests = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("string tests"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
        const _anon88 = _int_to_String(0);
        return (_bosatsu_to_js_string(_anon88) === "0") ? [1] : [0];
      })(), _concat_String(((_a0, _a1) => [1,
          _a0,
          _a1])(_js_to_bosatsu_string("int_to_String(0) == "), ((_a0, _a1) => [1,
            _a0,
            _a1])(_int_to_String(0), [0])))), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
          const _anon89 = _int_to_String(1);
          return (_bosatsu_to_js_string(_anon89) === "1") ? [1] : [0];
        })(), _concat_String(((_a0, _a1) => [1,
            _a0,
            _a1])(_js_to_bosatsu_string("int_to_String(1) == "), ((_a0, _a1) => [1,
              _a0,
              _a1])(_int_to_String(1), [0])))), ((_a0, _a1) => [1,
        _a0,
        _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
            const _anon90 = _int_to_String(2);
            return (_bosatsu_to_js_string(_anon90) === "2") ? [1] : [0];
          })(), _concat_String(((_a0, _a1) => [1,
              _a0,
              _a1])(_js_to_bosatsu_string("int_to_String(2) == "), ((_a0, _a1) => [1,
                _a0,
                _a1])(_int_to_String(2), [0])))), [0]))));
const il = _int_loop(5, 0, (i, a) => ((_a0, _a1) => [_a0,
    _a1])((-1) + i, i + a));
const tests = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("List tests"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0, _a0, _a1])([1], _concat_String(((_a0, _a1) => [1,
          _a0,
          _a1])(_js_to_bosatsu_string("2 - 3 = "), ((_a0, _a1) => [1,
            _a0,
            _a1])(_int_to_String((-1)), [0])))), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1) => [0,
        _a0,
        _a1])((Bosatsu_List$il === 15) ? [1] : [0], _concat_String(((_a0, _a1) => [1,
            _a0,
            _a1])(_js_to_bosatsu_string("int_loop test "), ((_a0, _a1) => [1,
              _a0,
              _a1])(_int_to_String(Bosatsu_List$il), [0])))), ((_a0, _a1) => [1,
        _a0,
        _a1])(((_a0, _a1) => [0,
          _a0,
          _a1])(Bosatsu_List$op_59984(((_a0, _a1) => [1,
              _a0,
              _a1])(1, ((_a0, _a1) => [1, _a0, _a1])(2, ((_a0, _a1) => [1,
                  _a0,
                  _a1])(3, [0]))), ((_a0, _a1) => [1,
              _a0,
              _a1])(1, ((_a0, _a1) => [1, _a0, _a1])(2, ((_a0, _a1) => [1,
                  _a0,
                  _a1])(3, [0])))), _js_to_bosatsu_string("list [1, 2, 3]")), ((_a0, _a1) => [1,
          _a0,
          _a1])(((_a0, _a1) => [0,
            _a0,
            _a1])(Bosatsu_Bool$not(Bosatsu_List$op_59984(((_a0, _a1) => [1,
                  _a0,
                  _a1])(1, ((_a0, _a1) => [1, _a0, _a1])(2, ((_a0, _a1) => [1,
                      _a0,
                      _a1])(3, [0]))), ((_a0, _a1) => [1,
                  _a0,
                  _a1])(1, ((_a0, _a1) => [1,
                    _a0,
                    _a1])(2, [0])))), _js_to_bosatsu_string("list [1, 2, 3] != [1, 2]")), ((_a0, _a1) => [1,
            _a0,
            _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                const _anon91 = range(0);
                return (_anon91[0] === 0) ? [1] : [0];
              })(), _js_to_bosatsu_string("range(0) matches []")), ((_a0, _a1) => [1,
              _a0,
              _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                  const _anon92 = range(1);
                  return (() => {
                    let _anon94;
                    return (() => {
                      let _anon93;
                      return (_anon92[0] === 1 && (() => {
                        _anon93 = _anon92[1];
                        return true;
                      })() && (() => {
                        _anon94 = _anon92[2];
                        return true;
                      })() && (_anon93 === 0 && (_anon94[0] === 0))) ? [1] : [0];
                    })();
                  })();
                })(), _js_to_bosatsu_string("range(1) matches [0]")), ((_a0, _a1) => [1,
                _a0,
                _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                    const _anon95 = range(2);
                    return (() => {
                      let _anon97;
                      return (() => {
                        let _anon96;
                        return (() => {
                          let _anon99;
                          return (() => {
                            let _anon98;
                            return (_anon95[0] === 1 && (() => {
                              _anon96 = _anon95[1];
                              return true;
                            })() && (() => {
                              _anon97 = _anon95[2];
                              return true;
                            })() && (_anon96 === 0 && (_anon97[0] === 1 && (() => {
                              _anon98 = _anon97[1];
                              return true;
                            })() && (() => {
                              _anon99 = _anon97[2];
                              return true;
                            })() && (_anon98 === 1 && (_anon99[0] === 0))))) ? [1] : [0];
                          })();
                        })();
                      })();
                    })();
                  })(), _js_to_bosatsu_string("range(2) matches [0, 1]")), ((_a0, _a1) => [1,
                  _a0,
                  _a1])(((_a0, _a1) => [0,
                    _a0,
                    _a1])(Bosatsu_List$op_59984(range(6), ((_a0, _a1) => [1,
                        _a0,
                        _a1])(0, ((_a0, _a1) => [1,
                          _a0,
                          _a1])(1, ((_a0, _a1) => [1,
                            _a0,
                            _a1])(2, ((_a0, _a1) => [1,
                              _a0,
                              _a1])(3, ((_a0, _a1) => [1,
                                _a0,
                                _a1])(4, ((_a0, _a1) => [1,
                                  _a0,
                                  _a1])(5, [0]))))))), _concat_String(((_a0, _a1) => [1,
                        _a0,
                        _a1])(_js_to_bosatsu_string("range(6) == "), ((_a0, _a1) => [1,
                          _a0,
                          _a1])(_concat_String(Bosatsu_Predef$map_List(range(6), i => _concat_String(((_a0, _a1) => [1,
                                  _a0,
                                  _a1])(_int_to_String(i), ((_a0, _a1) => [1,
                                    _a0,
                                    _a1])(_js_to_bosatsu_string(", "), [0]))))), [0])))), ((_a0, _a1) => [1,
                    _a0,
                    _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                        const _anon100 = foldl_List([0], 0, (_a0, _a1) => _a0 + _a1);
                        return (_anon100 === 0) ? [1] : [0];
                      })(), _js_to_bosatsu_string("sum([])")), ((_a0, _a1) => [1,
                      _a0,
                      _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                          const _anon101 = foldl_List(((_a0, _a1) => [1,
                              _a0,
                              _a1])(5, [0]), 0, (_a0, _a1) => _a0 + _a1);
                          return (_anon101 === 5) ? [1] : [0];
                        })(), _js_to_bosatsu_string("sum([5])")), ((_a0, _a1) => [1,
                        _a0,
                        _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                            const _anon102 = foldl_List(((_a0, _a1) => [1,
                                _a0,
                                _a1])(5, ((_a0, _a1) => [1,
                                  _a0,
                                  _a1])(11, [0])), 0, (_a0, _a1) => _a0 + _a1);
                            return (_anon102 === 16) ? [1] : [0];
                          })(), _js_to_bosatsu_string("sum([5, 11])")), ((_a0, _a1) => [1,
                          _a0,
                          _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                              const _anon103 = foldl_List(((_a0, _a1) => [1,
                                  _a0,
                                  _a1])(0, ((_a0, _a1) => [1,
                                    _a0,
                                    _a1])(1, ((_a0, _a1) => [1,
                                      _a0,
                                      _a1])(2, ((_a0, _a1) => [1,
                                        _a0,
                                        _a1])(3, ((_a0, _a1) => [1,
                                          _a0,
                                          _a1])(4, ((_a0, _a1) => [1,
                                            _a0,
                                            _a1])(5, [0])))))), 0, (_a0, _a1) => _a0 + _a1);
                              return (_anon103 === 15) ? [1] : [0];
                            })(), _js_to_bosatsu_string("sum([0, 1, 2, 3, 4, 5])")), ((_a0, _a1) => [1,
                            _a0,
                            _a1])(((_a0, _a1) => [0,
                              _a0,
                              _a1])(Bosatsu_List$exists(range(6), v => (v === 5) ? [1] : [0]), _js_to_bosatsu_string("range(6) does have 5")), ((_a0, _a1) => [1,
                              _a0,
                              _a1])(((_a0, _a1) => [0,
                                _a0,
                                _a1])(Bosatsu_Bool$not(Bosatsu_List$exists(range(6), v_1 => (v_1 === 6) ? [1] : [0])), _js_to_bosatsu_string("range(6) does not have 6")), ((_a0, _a1) => [1,
                                _a0,
                                _a1])(Bosatsu_List$stringTests, ((_a0, _a1) => [1,
                                  _a0,
                                  _a1])(Bosatsu_List$headTest, ((_a0, _a1) => [1,
                                    _a0,
                                    _a1])(Bosatsu_List$unconsTest, ((_a0, _a1) => [1,
                                      _a0,
                                      _a1])(Bosatsu_List$zipTest, ((_a0, _a1) => [1,
                                        _a0,
                                        _a1])(Bosatsu_List$sortTest, ((_a0, _a1) => [1,
                                          _a0,
                                          _a1])(((_a0, _a1) => [0,
                                            _a0,
                                            _a1])((() => {
                                              const _anon110 = (() => {
                                                const _anon104 = [0];
                                                return (() => {
                                                  let _anon105;
                                                  return (() => {
                                                    let _anon106;
                                                    return (() => {
                                                      let _anon107;
                                                      return (() => {
                                                        let _anon109;
                                                        return (() => {
                                                          const _anon108 = (() => {
                                                            (() => {
                                                              _anon107 = [0];
                                                              return true;
                                                            })();
                                                            return (() => {
                                                              (() => {
                                                                _anon109 = _anon104;
                                                                return true;
                                                              })();
                                                              return (() => {
                                                                while (_anon109[0] === 1) {
                                                                  (() => {
                                                                    (() => {
                                                                      _anon105 = _anon109;
                                                                      return true;
                                                                    })();
                                                                    return (_anon105[0] === 1 && (() => {
                                                                      _anon106 = _anon105[1];
                                                                      return true;
                                                                    })() && (_anon106[0] === 1)) ? (() => {
                                                                        (() => {
                                                                          _anon109 = [0];
                                                                          return true;
                                                                        })();
                                                                        return (() => {
                                                                          (() => {
                                                                            _anon107 = [1];
                                                                            return true;
                                                                          })();
                                                                          return [];
                                                                        })();
                                                                      })() : (() => {
                                                                        (() => {
                                                                          _anon109 = _anon109[2];
                                                                          return true;
                                                                        })();
                                                                        return [];
                                                                      })();
                                                                  })();
                                                                }
                                                                return _anon107;
                                                              })();
                                                            })();
                                                          })();
                                                          return _anon107[0] === 1;
                                                        })();
                                                      })();
                                                    })() ? [1] : [0];
                                                  })();
                                                })();
                                              })();
                                              return (_anon110[0] === 0) ? [1] : [0];
                                            })(), _js_to_bosatsu_string("any([])")), ((_a0, _a1) => [1,
                                            _a0,
                                            _a1])(((_a0, _a1) => [0,
                                              _a0,
                                              _a1])((() => {
                                                const _anon117 = (() => {
                                                  const _anon111 = ((_a0, _a1) => [1,
                                                    _a0,
                                                    _a1])([1], [0]);
                                                  return (() => {
                                                    let _anon112;
                                                    return (() => {
                                                      let _anon113;
                                                      return (() => {
                                                        let _anon114;
                                                        return (() => {
                                                          let _anon116;
                                                          return (() => {
                                                            const _anon115 = (() => {
                                                              (() => {
                                                                _anon114 = [0];
                                                                return true;
                                                              })();
                                                              return (() => {
                                                                (() => {
                                                                  _anon116 = _anon111;
                                                                  return true;
                                                                })();
                                                                return (() => {
                                                                  while (_anon116[0] === 1) {
                                                                    (() => {
                                                                      (() => {
                                                                        _anon112 = _anon116;
                                                                        return true;
                                                                      })();
                                                                      return (_anon112[0] === 1 && (() => {
                                                                        _anon113 = _anon112[1];
                                                                        return true;
                                                                      })() && (_anon113[0] === 1)) ? (() => {
                                                                          (() => {
                                                                            _anon116 = [0];
                                                                            return true;
                                                                          })();
                                                                          return (() => {
                                                                            (() => {
                                                                              _anon114 = [1];
                                                                              return true;
                                                                            })();
                                                                            return [];
                                                                          })();
                                                                        })() : (() => {
                                                                          (() => {
                                                                            _anon116 = _anon116[2];
                                                                            return true;
                                                                          })();
                                                                          return [];
                                                                        })();
                                                                    })();
                                                                  }
                                                                  return _anon114;
                                                                })();
                                                              })();
                                                            })();
                                                            return _anon114[0] === 1;
                                                          })();
                                                        })();
                                                      })() ? [1] : [0];
                                                    })();
                                                  })();
                                                })();
                                                return (_anon117[0] === 1) ? [1] : [0];
                                              })(), _js_to_bosatsu_string("any([True])")), ((_a0, _a1) => [1,
                                              _a0,
                                              _a1])(((_a0, _a1) => [0,
                                                _a0,
                                                _a1])((() => {
                                                  const _anon124 = (() => {
                                                    const _anon118 = ((_a0, _a1) => [1,
                                                      _a0,
                                                      _a1])([0], [0]);
                                                    return (() => {
                                                      let _anon119;
                                                      return (() => {
                                                        let _anon120;
                                                        return (() => {
                                                          let _anon121;
                                                          return (() => {
                                                            let _anon123;
                                                            return (() => {
                                                              const _anon122 = (() => {
                                                                (() => {
                                                                  _anon121 = [0];
                                                                  return true;
                                                                })();
                                                                return (() => {
                                                                  (() => {
                                                                    _anon123 = _anon118;
                                                                    return true;
                                                                  })();
                                                                  return (() => {
                                                                    while (_anon123[0] === 1) {
                                                                      (() => {
                                                                        (() => {
                                                                          _anon119 = _anon123;
                                                                          return true;
                                                                        })();
                                                                        return (_anon119[0] === 1 && (() => {
                                                                          _anon120 = _anon119[1];
                                                                          return true;
                                                                        })() && (_anon120[0] === 1)) ? (() => {
                                                                            (() => {
                                                                              _anon123 = [0];
                                                                              return true;
                                                                            })();
                                                                            return (() => {
                                                                              (() => {
                                                                                _anon121 = [1];
                                                                                return true;
                                                                              })();
                                                                              return [];
                                                                            })();
                                                                          })() : (() => {
                                                                            (() => {
                                                                              _anon123 = _anon123[2];
                                                                              return true;
                                                                            })();
                                                                            return [];
                                                                          })();
                                                                      })();
                                                                    }
                                                                    return _anon121;
                                                                  })();
                                                                })();
                                                              })();
                                                              return _anon121[0] === 1;
                                                            })();
                                                          })();
                                                        })() ? [1] : [0];
                                                      })();
                                                    })();
                                                  })();
                                                  return (_anon124[0] === 0) ? [1] : [0];
                                                })(), _js_to_bosatsu_string("any([False])")), ((_a0, _a1) => [1,
                                                _a0,
                                                _a1])(((_a0, _a1) => [0,
                                                  _a0,
                                                  _a1])((() => {
                                                    const _anon131 = (() => {
                                                      const _anon125 = ((_a0, _a1) => [1,
                                                        _a0,
                                                        _a1])([0], ((_a0, _a1) => [1,
                                                          _a0,
                                                          _a1])([0], [0]));
                                                      return (() => {
                                                        let _anon126;
                                                        return (() => {
                                                          let _anon127;
                                                          return (() => {
                                                            let _anon128;
                                                            return (() => {
                                                              let _anon130;
                                                              return (() => {
                                                                const _anon129 = (() => {
                                                                  (() => {
                                                                    _anon128 = [0];
                                                                    return true;
                                                                  })();
                                                                  return (() => {
                                                                    (() => {
                                                                      _anon130 = _anon125;
                                                                      return true;
                                                                    })();
                                                                    return (() => {
                                                                      while (_anon130[0] === 1) {
                                                                        (() => {
                                                                          (() => {
                                                                            _anon126 = _anon130;
                                                                            return true;
                                                                          })();
                                                                          return (_anon126[0] === 1 && (() => {
                                                                            _anon127 = _anon126[1];
                                                                            return true;
                                                                          })() && (_anon127[0] === 1)) ? (() => {
                                                                              (() => {
                                                                                _anon130 = [0];
                                                                                return true;
                                                                              })();
                                                                              return (() => {
                                                                                (() => {
                                                                                  _anon128 = [1];
                                                                                  return true;
                                                                                })();
                                                                                return [];
                                                                              })();
                                                                            })() : (() => {
                                                                              (() => {
                                                                                _anon130 = _anon130[2];
                                                                                return true;
                                                                              })();
                                                                              return [];
                                                                            })();
                                                                        })();
                                                                      }
                                                                      return _anon128;
                                                                    })();
                                                                  })();
                                                                })();
                                                                return _anon128[0] === 1;
                                                              })();
                                                            })();
                                                          })() ? [1] : [0];
                                                        })();
                                                      })();
                                                    })();
                                                    return (_anon131[0] === 0) ? [1] : [0];
                                                  })(), _js_to_bosatsu_string("any([False, False])")), ((_a0, _a1) => [1,
                                                  _a0,
                                                  _a1])(((_a0, _a1) => [0,
                                                    _a0,
                                                    _a1])((() => {
                                                      const _anon138 = (() => {
                                                        const _anon132 = ((_a0, _a1) => [1,
                                                          _a0,
                                                          _a1])([0], ((_a0, _a1) => [1,
                                                            _a0,
                                                            _a1])([1], [0]));
                                                        return (() => {
                                                          let _anon133;
                                                          return (() => {
                                                            let _anon134;
                                                            return (() => {
                                                              let _anon135;
                                                              return (() => {
                                                                let _anon137;
                                                                return (() => {
                                                                  const _anon136 = (() => {
                                                                    (() => {
                                                                      _anon135 = [0];
                                                                      return true;
                                                                    })();
                                                                    return (() => {
                                                                      (() => {
                                                                        _anon137 = _anon132;
                                                                        return true;
                                                                      })();
                                                                      return (() => {
                                                                        while (_anon137[0] === 1) {
                                                                          (() => {
                                                                            (() => {
                                                                              _anon133 = _anon137;
                                                                              return true;
                                                                            })();
                                                                            return (_anon133[0] === 1 && (() => {
                                                                              _anon134 = _anon133[1];
                                                                              return true;
                                                                            })() && (_anon134[0] === 1)) ? (() => {
                                                                                (() => {
                                                                                  _anon137 = [0];
                                                                                  return true;
                                                                                })();
                                                                                return (() => {
                                                                                  (() => {
                                                                                    _anon135 = [1];
                                                                                    return true;
                                                                                  })();
                                                                                  return [];
                                                                                })();
                                                                              })() : (() => {
                                                                                (() => {
                                                                                  _anon137 = _anon137[2];
                                                                                  return true;
                                                                                })();
                                                                                return [];
                                                                              })();
                                                                          })();
                                                                        }
                                                                        return _anon135;
                                                                      })();
                                                                    })();
                                                                  })();
                                                                  return _anon135[0] === 1;
                                                                })();
                                                              })();
                                                            })() ? [1] : [0];
                                                          })();
                                                        })();
                                                      })();
                                                      return (_anon138[0] === 1) ? [1] : [0];
                                                    })(), _js_to_bosatsu_string("any([False, True])")), ((_a0, _a1) => [1,
                                                    _a0,
                                                    _a1])(((_a0, _a1) => [0,
                                                      _a0,
                                                      _a1])((() => {
                                                        const _anon145 = (() => {
                                                          const _anon139 = ((_a0, _a1) => [1,
                                                            _a0,
                                                            _a1])([1], ((_a0, _a1) => [1,
                                                              _a0,
                                                              _a1])([0], [0]));
                                                          return (() => {
                                                            let _anon140;
                                                            return (() => {
                                                              let _anon141;
                                                              return (() => {
                                                                let _anon142;
                                                                return (() => {
                                                                  let _anon144;
                                                                  return (() => {
                                                                    const _anon143 = (() => {
                                                                      (() => {
                                                                        _anon142 = [0];
                                                                        return true;
                                                                      })();
                                                                      return (() => {
                                                                        (() => {
                                                                          _anon144 = _anon139;
                                                                          return true;
                                                                        })();
                                                                        return (() => {
                                                                          while (_anon144[0] === 1) {
                                                                            (() => {
                                                                              (() => {
                                                                                _anon140 = _anon144;
                                                                                return true;
                                                                              })();
                                                                              return (_anon140[0] === 1 && (() => {
                                                                                _anon141 = _anon140[1];
                                                                                return true;
                                                                              })() && (_anon141[0] === 1)) ? (() => {
                                                                                  (() => {
                                                                                    _anon144 = [0];
                                                                                    return true;
                                                                                  })();
                                                                                  return (() => {
                                                                                    (() => {
                                                                                      _anon142 = [1];
                                                                                      return true;
                                                                                    })();
                                                                                    return [];
                                                                                  })();
                                                                                })() : (() => {
                                                                                  (() => {
                                                                                    _anon144 = _anon144[2];
                                                                                    return true;
                                                                                  })();
                                                                                  return [];
                                                                                })();
                                                                            })();
                                                                          }
                                                                          return _anon142;
                                                                        })();
                                                                      })();
                                                                    })();
                                                                    return _anon142[0] === 1;
                                                                  })();
                                                                })();
                                                              })() ? [1] : [0];
                                                            })();
                                                          })();
                                                        })();
                                                        return (_anon145[0] === 1) ? [1] : [0];
                                                      })(), _js_to_bosatsu_string("any([True, False])")), ((_a0, _a1) => [1,
                                                      _a0,
                                                      _a1])(((_a0, _a1) => [0,
                                                        _a0,
                                                        _a1])((() => {
                                                          const _anon152 = (() => {
                                                            const _anon146 = ((_a0, _a1) => [1,
                                                              _a0,
                                                              _a1])([1], ((_a0, _a1) => [1,
                                                                _a0,
                                                                _a1])([1], [0]));
                                                            return (() => {
                                                              let _anon147;
                                                              return (() => {
                                                                let _anon148;
                                                                return (() => {
                                                                  let _anon149;
                                                                  return (() => {
                                                                    let _anon151;
                                                                    return (() => {
                                                                      const _anon150 = (() => {
                                                                        (() => {
                                                                          _anon149 = [0];
                                                                          return true;
                                                                        })();
                                                                        return (() => {
                                                                          (() => {
                                                                            _anon151 = _anon146;
                                                                            return true;
                                                                          })();
                                                                          return (() => {
                                                                            while (_anon151[0] === 1) {
                                                                              (() => {
                                                                                (() => {
                                                                                  _anon147 = _anon151;
                                                                                  return true;
                                                                                })();
                                                                                return (_anon147[0] === 1 && (() => {
                                                                                  _anon148 = _anon147[1];
                                                                                  return true;
                                                                                })() && (_anon148[0] === 1)) ? (() => {
                                                                                    (() => {
                                                                                      _anon151 = [0];
                                                                                      return true;
                                                                                    })();
                                                                                    return (() => {
                                                                                      (() => {
                                                                                        _anon149 = [1];
                                                                                        return true;
                                                                                      })();
                                                                                      return [];
                                                                                    })();
                                                                                  })() : (() => {
                                                                                    (() => {
                                                                                      _anon151 = _anon151[2];
                                                                                      return true;
                                                                                    })();
                                                                                    return [];
                                                                                  })();
                                                                              })();
                                                                            }
                                                                            return _anon149;
                                                                          })();
                                                                        })();
                                                                      })();
                                                                      return _anon149[0] === 1;
                                                                    })();
                                                                  })();
                                                                })() ? [1] : [0];
                                                              })();
                                                            })();
                                                          })();
                                                          return (_anon152[0] === 1) ? [1] : [0];
                                                        })(), _js_to_bosatsu_string("any([True, True])")), [0])))))))))))))))))))))))))));
export {any};