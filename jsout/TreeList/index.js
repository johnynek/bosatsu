// Import runtime
// require("./../_runtime.js") or import from "./../_runtime.js"

const foldTree = (t, init, fn) => (() => {
  const loop = (_slots => (t_1, init_1) => (t_1[0] === 0) ? (() => {
      const a = t_1[1];
      return _slots[0](init_1, a);
    })() : (() => {
      const h = t_1[2];
      return (() => {
        const l = t_1[3];
        return (() => {
          const r = t_1[4];
          return loop(r, loop(l, _slots[0](init_1, h)));
        })();
      })();
    })())([fn]);
  return loop(t, init);
})();
const empty = (_a0 => [_a0])([0]);
const cons = (head, a) => (() => {
  const tail = a[0];
  return (tail[0] === 0) ? (_a0 => [_a0])(((_a0, _a1) => [1,
        _a0,
        _a1])((_a0 => [0, _a0])(head), [0])) : (() => {
      let _anon2;
      return (() => {
        let _anon1;
        return (tail[0] === 1 && (() => {
          _anon1 = tail[1];
          return true;
        })() && (() => {
          _anon2 = tail[2];
          return true;
        })() && (_anon1[0] === 0 && (_anon2[0] === 0))) ? (() => {
            const s1 = _anon1;
            return (_a0 => [_a0])(((_a0, _a1) => [1, _a0, _a1])((_a0 => [0,
                  _a0])(head), ((_a0, _a1) => [1, _a0, _a1])(s1, [0])));
          })() : (() => {
            let _anon4;
            return (() => {
              let _anon3;
              return (() => {
                let _anon5;
                return (tail[0] === 1 && (() => {
                  _anon3 = tail[1];
                  return true;
                })() && (() => {
                  _anon4 = tail[2];
                  return true;
                })() && (_anon3[0] === 0 && (_anon4[0] === 1 && (() => {
                  _anon5 = _anon4[1];
                  return true;
                })() && (_anon5[0] === 1)))) ? (() => {
                    const b1 = _anon5;
                    return (() => {
                      const t = _anon4[2];
                      return (_a0 => [_a0])(((_a0, _a1) => [1,
                          _a0,
                          _a1])((_a0 => [0, _a0])(head), ((_a0, _a1) => [1,
                            _a0,
                            _a1])(b1, t)));
                    })();
                  })() : (() => {
                    let _anon7;
                    return (() => {
                      let _anon6;
                      return (() => {
                        let _anon8;
                        return (tail[0] === 1 && (() => {
                          _anon6 = tail[1];
                          return true;
                        })() && (() => {
                          _anon7 = tail[2];
                          return true;
                        })() && (_anon6[0] === 0 && (_anon7[0] === 1 && (() => {
                          _anon8 = _anon7[1];
                          return true;
                        })() && (_anon8[0] === 0)))) ? (() => {
                            const s1_1 = _anon6;
                            return (() => {
                              const s2 = _anon8;
                              return (() => {
                                const t_1 = _anon7[2];
                                return (_a0 => [_a0])(((_a0, _a1) => [1,
                                    _a0,
                                    _a1])(((_a0, _a1, _a2, _a3) => [1,
                                      _a0,
                                      _a1,
                                      _a2,
                                      _a3])(3, head, s1_1, s2), t_1));
                              })();
                            })();
                          })() : (() => {
                            let _anon10;
                            return (() => {
                              let _anon9;
                              return (() => {
                                let _anon11;
                                return (tail[0] === 1 && (() => {
                                  _anon9 = tail[1];
                                  return true;
                                })() && (() => {
                                  _anon10 = tail[2];
                                  return true;
                                })() && (_anon9[0] === 1 && (_anon10[0] === 1 && (() => {
                                  _anon11 = _anon10[1];
                                  return true;
                                })() && (_anon11[0] === 0)))) ? TreeList$empty : (() => {
                                    let _anon13;
                                    return (() => {
                                      let _anon12;
                                      return (tail[0] === 1 && (() => {
                                        _anon12 = tail[1];
                                        return true;
                                      })() && (() => {
                                        _anon13 = tail[2];
                                        return true;
                                      })() && (_anon12[0] === 1 && (_anon13[0] === 0))) ? (() => {
                                          const branch = _anon12;
                                          return (_a0 => [_a0])(((_a0, _a1) => [1,
                                              _a0,
                                              _a1])((_a0 => [0,
                                                _a0])(head), ((_a0, _a1) => [1,
                                                _a0,
                                                _a1])(branch, [0])));
                                        })() : (() => {
                                          let _anon15;
                                          return (() => {
                                            let _anon14;
                                            return (() => {
                                              let _anon16;
                                              return (() => {
                                                (() => {
                                                  _anon14 = tail[1];
                                                  return true;
                                                })() && (() => {
                                                  _anon15 = tail[2];
                                                  return true;
                                                })() && (() => {
                                                  _anon16 = _anon15[1];
                                                  return true;
                                                })();
                                                return (() => {
                                                  const b1_1 = _anon14;
                                                  return (() => {
                                                    const s1_2 = _anon14[1];
                                                    return (() => {
                                                      const b2 = _anon16;
                                                      return (() => {
                                                        const s2_1 = _anon16[1];
                                                        return (() => {
                                                          const t_2 = _anon15[2];
                                                          return (() => {
                                                            const _anon0 = (s1_2 === s2_1) ? [1] : [0];
                                                            return (_anon0[0] === 1) ? (_a0 => [_a0])(((_a0, _a1) => [1,
                                                                  _a0,
                                                                  _a1])(((_a0, _a1, _a2, _a3) => [1,
                                                                    _a0,
                                                                    _a1,
                                                                    _a2,
                                                                    _a3])(1 + (s1_2 + s2_1), head, b1_1, b2), t_2)) : (_a0 => [_a0])(((_a0, _a1) => [1,
                                                                  _a0,
                                                                  _a1])((_a0 => [0,
                                                                    _a0])(head), tail));
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
const decons = a => (() => {
  const trees = a[0];
  return (trees[0] === 0) ? [0] : (() => {
      let _anon17;
      return (trees[0] === 1 && (() => {
        _anon17 = trees[1];
        return true;
      })() && (_anon17[0] === 0)) ? (() => {
          const h = _anon17[1];
          return (() => {
            const t = trees[2];
            return (_a0 => [1, _a0])(((_a0, _a1) => [_a0,
                _a1])(h, (_a0 => [_a0])(t)));
          })();
        })() : (() => {
          let _anon18;
          return (() => {
            (() => {
              _anon18 = trees[1];
              return true;
            })();
            return (() => {
              const h_1 = _anon18[2];
              return (() => {
                const b1 = _anon18[3];
                return (() => {
                  const b2 = _anon18[4];
                  return (() => {
                    const t_1 = trees[2];
                    return (_a0 => [1, _a0])(((_a0, _a1) => [_a0,
                        _a1])(h_1, (_a0 => [_a0])(((_a0, _a1) => [1,
                            _a0,
                            _a1])(b1, ((_a0, _a1) => [1,
                              _a0,
                              _a1])(b2, t_1)))));
                  })();
                })();
              })();
            })();
          })();
        })();
    })();
})();
const head = tl => (() => {
  const _anon19 = TreeList$decons(tl);
  return (() => {
    let _anon20;
    return (_anon19[0] === 1 && (() => {
      _anon20 = _anon19[1];
      return true;
    })()) ? (() => {
        const h = _anon20[0];
        return (_a0 => [1, _a0])(h);
      })() : [0];
  })();
})();
const get = (b, idx) => (() => {
  const trees = b[0];
  return (() => {
    const _anon28 = _int_loop(1 + idx, ((_a0, _a1) => [_a0,
        _a1])(trees, [0]), (idx_1, a) => (() => {
        const trees_1 = a[0];
        return (() => {
          const _anon27 = (idx_1 === 1) ? [1] : [0];
          return (_anon27[0] === 1) ? ((_a0, _a1) => [_a0,
              _a1])(0, ((_a0, _a1) => [_a0, _a1])([0], (() => {
                  const _anon21 = TreeList$decons((_a0 => [_a0])(trees_1));
                  return (() => {
                    let _anon22;
                    return (_anon21[0] === 1 && (() => {
                      _anon22 = _anon21[1];
                      return true;
                    })()) ? (() => {
                        const h = _anon22[0];
                        return (_a0 => [1, _a0])(h);
                      })() : [0];
                  })();
                })())) : (trees_1[0] === 0) ? ((_a0, _a1) => [_a0,
                _a1])(0, ((_a0, _a1) => [_a0, _a1])([0], [0])) : (() => {
                let _anon25;
                return (trees_1[0] === 1 && (() => {
                  _anon25 = trees_1[1];
                  return true;
                })() && (_anon25[0] === 0)) ? (() => {
                    const h_1 = _anon25[1];
                    return (() => {
                      const rest = trees_1[2];
                      return (() => {
                        const _anon23 = (idx_1 === 1) ? [1] : [0];
                        return (_anon23[0] === 1) ? ((_a0, _a1) => [_a0,
                            _a1])(0, ((_a0, _a1) => [_a0, _a1])([0], (_a0 => [1,
                                _a0])(h_1))) : ((_a0, _a1) => [_a0,
                            _a1])((-1) + idx_1, ((_a0, _a1) => [_a0,
                              _a1])(rest, [0]));
                      })();
                    })();
                  })() : (() => {
                    let _anon26;
                    return (() => {
                      (() => {
                        _anon26 = trees_1[1];
                        return true;
                      })();
                      return (() => {
                        const s = _anon26[1];
                        return (() => {
                          const t1 = _anon26[3];
                          return (() => {
                            const t2 = _anon26[4];
                            return (() => {
                              const rest_1 = trees_1[2];
                              return (() => {
                                const _anon24 = (idx_1 < s) ? [0] : (idx_1 === s) ? [1] : [2];
                                return (_anon24[0] === 0) ? ((_a0, _a1) => [_a0,
                                    _a1])((-1) + idx_1, ((_a0, _a1) => [_a0,
                                      _a1])(((_a0, _a1) => [1,
                                        _a0,
                                        _a1])(t1, ((_a0, _a1) => [1,
                                          _a0,
                                          _a1])(t2, rest_1)), [0])) : (_anon24[0] === 1) ? ((_a0, _a1) => [_a0,
                                      _a1])((-1) + idx_1, ((_a0, _a1) => [_a0,
                                        _a1])(((_a0, _a1) => [1,
                                          _a0,
                                          _a1])(t1, ((_a0, _a1) => [1,
                                            _a0,
                                            _a1])(t2, rest_1)), [0])) : ((_a0, _a1) => [_a0,
                                      _a1])(idx_1 - s, ((_a0, _a1) => [_a0,
                                        _a1])(rest_1, [0]));
                              })();
                            })();
                          })();
                        })();
                      })();
                    })();
                  })();
              })();
        })();
      })());
    return _anon28[1];
  })();
})();
const fold = (a, init, fn) => (() => {
  const trees = a[0];
  return (() => {
    const loop = (_slots => (trees_1, init_1) => (() => {
      let _anon29;
      return (() => {
        let _anon30;
        return (() => {
          let _anon32;
          return (() => {
            let _anon34;
            return (() => {
              (() => {
                _anon32 = trees_1;
                return true;
              })();
              return (() => {
                (() => {
                  _anon34 = init_1;
                  return true;
                })();
                return (() => {
                  (() => {
                    _anon29 = [1];
                    return true;
                  })();
                  return (() => {
                    while (_anon29[0] === 1) {
                      (_anon32[0] === 0) ? (() => {
                          (() => {
                            _anon29 = [0];
                            return true;
                          })();
                          return (() => {
                            (() => {
                              _anon30 = _anon34;
                              return true;
                            })();
                            return [];
                          })();
                        })() : (() => {
                          const h = _anon32[1];
                          return (() => {
                            const t = _anon32[2];
                            return (() => {
                              const _anon31 = t;
                              return (() => {
                                const _anon33 = TreeList$foldTree(h, _anon34, _slots[0]);
                                return (() => {
                                  (() => {
                                    _anon32 = _anon31;
                                    return true;
                                  })();
                                  return (() => {
                                    (() => {
                                      _anon34 = _anon33;
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
                    return _anon30;
                  })();
                })();
              })();
            })();
          })();
        })();
      })();
    })())([fn]);
    return loop(trees, init);
  })();
})();
const to_List = list => Bosatsu_Predef$reverse(TreeList$fold(list, [0], (l, h) => ((_a0, _a1) => [1,
      _a0,
      _a1])(h, l)));
const eq_TreeList = fn => (_slots => (a, b) => (() => {
  const _anon38 = TreeList$fold(a, ((_a0, _a1) => [_a0,
      _a1])([1], b), (_slots => (a_1, h) => (() => {
      const current = a_1[0];
      return (() => {
        const b_1 = a_1[1];
        return (current[0] === 1) ? (() => {
            const _anon36 = TreeList$decons(b_1);
            return (_anon36[0] === 0) ? ((_a0, _a1) => [_a0,
                _a1])([0], TreeList$empty) : (() => {
                let _anon37;
                return (() => {
                  (() => {
                    _anon37 = _anon36[1];
                    return true;
                  })();
                  return (() => {
                    const hb = _anon37[0];
                    return (() => {
                      const tb = _anon37[1];
                      return (() => {
                        const _anon35 = _slots[0](h, hb);
                        return (_anon35[0] === 1) ? ((_a0, _a1) => [_a0,
                            _a1])([1], tb) : ((_a0, _a1) => [_a0,
                            _a1])([0], TreeList$empty);
                      })();
                    })();
                  })();
                })();
              })();
          })() : ((_a0, _a1) => [_a0, _a1])([0], TreeList$empty);
      })();
    })())([_slots[0]]));
  return _anon38[0];
})())([fn]);
const eq_ti = TreeList$eq_TreeList((_a0, _a1) => (_a0 === _a1) ? [1] : [0]);
const tl12 = TreeList$cons(2, TreeList$cons(1, TreeList$empty));
const list14 = (() => {
  const list = ((_a0, _a1) => [1, _a0, _a1])(1, ((_a0, _a1) => [1,
      _a0,
      _a1])(2, ((_a0, _a1) => [1, _a0, _a1])(3, ((_a0, _a1) => [1,
          _a0,
          _a1])(4, [0]))));
  return foldl_List(Bosatsu_Predef$reverse(list), TreeList$empty, (lst, h) => TreeList$cons(h, lst));
})();
const cons14 = TreeList$cons(1, TreeList$cons(2, TreeList$cons(3, TreeList$cons(4, TreeList$empty))));
const tests = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("TreeList tests"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
        const _anon39 = ((_a0, _a1) => [_a0,
          _a1])(TreeList$get(TreeList$tl12, 0), (_a0 => [1, _a0])(2));
        return (() => {
          let _anon41;
          return (() => {
            let _anon40;
            return ((() => {
              _anon41 = _anon39[1];
              return true;
            })() && ((() => {
              _anon40 = _anon39[0];
              return true;
            })() && (_anon40[0] === 1 && (_anon41[0] === 1)))) ? (() => {
                const a = _anon40[1];
                return (() => {
                  const b = _anon41[1];
                  return (a === b) ? [1] : [0];
                })();
              })() : (() => {
                let _anon43;
                return (() => {
                  let _anon42;
                  return ((() => {
                    _anon43 = _anon39[1];
                    return true;
                  })() && ((() => {
                    _anon42 = _anon39[0];
                    return true;
                  })() && (_anon42[0] === 0 && (_anon43[0] === 0)))) ? [1] : [0];
                })();
              })();
          })();
        })();
      })(), _js_to_bosatsu_string("get 0 == 2")), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
          const _anon44 = ((_a0, _a1) => [_a0,
            _a1])(TreeList$get(TreeList$tl12, 1), (_a0 => [1, _a0])(1));
          return (() => {
            let _anon46;
            return (() => {
              let _anon45;
              return ((() => {
                _anon46 = _anon44[1];
                return true;
              })() && ((() => {
                _anon45 = _anon44[0];
                return true;
              })() && (_anon45[0] === 1 && (_anon46[0] === 1)))) ? (() => {
                  const a_1 = _anon45[1];
                  return (() => {
                    const b_1 = _anon46[1];
                    return (a_1 === b_1) ? [1] : [0];
                  })();
                })() : (() => {
                  let _anon48;
                  return (() => {
                    let _anon47;
                    return ((() => {
                      _anon48 = _anon44[1];
                      return true;
                    })() && ((() => {
                      _anon47 = _anon44[0];
                      return true;
                    })() && (_anon47[0] === 0 && (_anon48[0] === 0)))) ? [1] : [0];
                  })();
                })();
            })();
          })();
        })(), _js_to_bosatsu_string("get 1 == 1")), ((_a0, _a1) => [1,
        _a0,
        _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
            const _anon49 = ((_a0, _a1) => [_a0,
              _a1])(TreeList$get(TreeList$tl12, 2), [0]);
            return (() => {
              let _anon51;
              return (() => {
                let _anon50;
                return ((() => {
                  _anon51 = _anon49[1];
                  return true;
                })() && ((() => {
                  _anon50 = _anon49[0];
                  return true;
                })() && (_anon50[0] === 1 && (_anon51[0] === 1)))) ? (() => {
                    const a_2 = _anon50[1];
                    return (() => {
                      const b_2 = _anon51[1];
                      return (a_2 === b_2) ? [1] : [0];
                    })();
                  })() : (() => {
                    let _anon53;
                    return (() => {
                      let _anon52;
                      return ((() => {
                        _anon53 = _anon49[1];
                        return true;
                      })() && ((() => {
                        _anon52 = _anon49[0];
                        return true;
                      })() && (_anon52[0] === 0 && (_anon53[0] === 0)))) ? [1] : [0];
                    })();
                  })();
              })();
            })();
          })(), _js_to_bosatsu_string("get 2 == None")), ((_a0, _a1) => [1,
          _a0,
          _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
              const _anon54 = ((_a0, _a1) => [_a0,
                _a1])(TreeList$get(TreeList$list14, 0), (_a0 => [1, _a0])(1));
              return (() => {
                let _anon56;
                return (() => {
                  let _anon55;
                  return ((() => {
                    _anon56 = _anon54[1];
                    return true;
                  })() && ((() => {
                    _anon55 = _anon54[0];
                    return true;
                  })() && (_anon55[0] === 1 && (_anon56[0] === 1)))) ? (() => {
                      const a_3 = _anon55[1];
                      return (() => {
                        const b_3 = _anon56[1];
                        return (a_3 === b_3) ? [1] : [0];
                      })();
                    })() : (() => {
                      let _anon58;
                      return (() => {
                        let _anon57;
                        return ((() => {
                          _anon58 = _anon54[1];
                          return true;
                        })() && ((() => {
                          _anon57 = _anon54[0];
                          return true;
                        })() && (_anon57[0] === 0 && (_anon58[0] === 0)))) ? [1] : [0];
                      })();
                    })();
                })();
              })();
            })(), _js_to_bosatsu_string("[1, 2, 3, 4] get 0")), ((_a0, _a1) => [1,
            _a0,
            _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                const _anon59 = ((_a0, _a1) => [_a0,
                  _a1])(TreeList$get(TreeList$list14, 1), (_a0 => [1, _a0])(2));
                return (() => {
                  let _anon61;
                  return (() => {
                    let _anon60;
                    return ((() => {
                      _anon61 = _anon59[1];
                      return true;
                    })() && ((() => {
                      _anon60 = _anon59[0];
                      return true;
                    })() && (_anon60[0] === 1 && (_anon61[0] === 1)))) ? (() => {
                        const a_4 = _anon60[1];
                        return (() => {
                          const b_4 = _anon61[1];
                          return (a_4 === b_4) ? [1] : [0];
                        })();
                      })() : (() => {
                        let _anon63;
                        return (() => {
                          let _anon62;
                          return ((() => {
                            _anon63 = _anon59[1];
                            return true;
                          })() && ((() => {
                            _anon62 = _anon59[0];
                            return true;
                          })() && (_anon62[0] === 0 && (_anon63[0] === 0)))) ? [1] : [0];
                        })();
                      })();
                  })();
                })();
              })(), _js_to_bosatsu_string("[1, 2, 3, 4] get 1")), ((_a0, _a1) => [1,
              _a0,
              _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                  const _anon64 = ((_a0, _a1) => [_a0,
                    _a1])(TreeList$get(TreeList$list14, 2), (_a0 => [1,
                      _a0])(3));
                  return (() => {
                    let _anon66;
                    return (() => {
                      let _anon65;
                      return ((() => {
                        _anon66 = _anon64[1];
                        return true;
                      })() && ((() => {
                        _anon65 = _anon64[0];
                        return true;
                      })() && (_anon65[0] === 1 && (_anon66[0] === 1)))) ? (() => {
                          const a_5 = _anon65[1];
                          return (() => {
                            const b_5 = _anon66[1];
                            return (a_5 === b_5) ? [1] : [0];
                          })();
                        })() : (() => {
                          let _anon68;
                          return (() => {
                            let _anon67;
                            return ((() => {
                              _anon68 = _anon64[1];
                              return true;
                            })() && ((() => {
                              _anon67 = _anon64[0];
                              return true;
                            })() && (_anon67[0] === 0 && (_anon68[0] === 0)))) ? [1] : [0];
                          })();
                        })();
                    })();
                  })();
                })(), _js_to_bosatsu_string("[1, 2, 3, 4] get 2")), ((_a0, _a1) => [1,
                _a0,
                _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                    const _anon69 = ((_a0, _a1) => [_a0,
                      _a1])(TreeList$get(TreeList$list14, 3), (_a0 => [1,
                        _a0])(4));
                    return (() => {
                      let _anon71;
                      return (() => {
                        let _anon70;
                        return ((() => {
                          _anon71 = _anon69[1];
                          return true;
                        })() && ((() => {
                          _anon70 = _anon69[0];
                          return true;
                        })() && (_anon70[0] === 1 && (_anon71[0] === 1)))) ? (() => {
                            const a_6 = _anon70[1];
                            return (() => {
                              const b_6 = _anon71[1];
                              return (a_6 === b_6) ? [1] : [0];
                            })();
                          })() : (() => {
                            let _anon73;
                            return (() => {
                              let _anon72;
                              return ((() => {
                                _anon73 = _anon69[1];
                                return true;
                              })() && ((() => {
                                _anon72 = _anon69[0];
                                return true;
                              })() && (_anon72[0] === 0 && (_anon73[0] === 0)))) ? [1] : [0];
                            })();
                          })();
                      })();
                    })();
                  })(), _js_to_bosatsu_string("[1, 2, 3, 4] get 3")), ((_a0, _a1) => [1,
                  _a0,
                  _a1])(((_a0, _a1) => [0,
                    _a0,
                    _a1])((TreeList$fold(TreeList$list14, 0, (_a0, _a1) => _a0 + _a1) === 10) ? [1] : [0], _js_to_bosatsu_string("fold to 10")), ((_a0, _a1) => [1,
                    _a0,
                    _a1])(((_a0, _a1) => [0,
                      _a0,
                      _a1])(TreeList$eq_ti(TreeList$list14, TreeList$cons14), _js_to_bosatsu_string("fromList matches building by cons")), [0]))))))))));
export {foldTree};