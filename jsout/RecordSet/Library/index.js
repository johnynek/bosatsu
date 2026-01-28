// Import runtime
// require("./../../_runtime.js") or import from "./../../_runtime.js"

const list_of_rows = b => (() => {
  const fields = b[0];
  return (() => {
    const rows = b[1];
    return (() => {
      const getters = b[2];
      return (() => {
        const traverse = b[3];
        return (() => {
          const record_to_list = b[4];
          return Bosatsu_Predef$map_List(rows, (_slots => row => _slots[0](_slots[1](_slots[2])((_slots => a => (() => {
                  const get_field = a[0];
                  return (() => {
                    const get_value = a[1];
                    return (() => {
                      const _anon0 = get_field(_slots[0]);
                      return (() => {
                        const to_entry = _anon0[1];
                        return (_a0 => [_a0])(to_entry(get_value(_slots[1])));
                      })();
                    })();
                  })();
                })())([_slots[3], row]))))([record_to_list,
                traverse,
                getters,
                fields]));
        })();
      })();
    })();
  })();
})();
const restructure = (a, f) => (() => {
  const fields = a[0];
  return (() => {
    const rows = a[1];
    return (() => {
      const getters = a[2];
      return (() => {
        const _anon1 = f(getters);
        return (() => {
          const reshaperF = _anon1[0];
          return (() => {
            const reshaperV = _anon1[1];
            return (() => {
              const new_getters = _anon1[2];
              return (() => {
                const traverse = _anon1[3];
                return (() => {
                  const record_to_list = _anon1[4];
                  return ((_a0, _a1, _a2, _a3, _a4) => [_a0,
                    _a1,
                    _a2,
                    _a3,
                    _a4])(reshaperF(fields), Bosatsu_Predef$map_List(rows, reshaperV), new_getters, traverse, record_to_list);
                })();
              })();
            })();
          })();
        })();
      })();
    })();
  })();
})();
const new_record_set = ((_a0, _a1, _a2, _a3, _a4) => [_a0,
  _a1,
  _a2,
  _a3,
  _a4])([], [0], [], a => a_1 => [], a_2 => [0]);
const ps_end = ((_a0, _a1, _a2, _a3, _a4) => [_a0,
  _a1,
  _a2,
  _a3,
  _a4])(a => [], a_1 => [], [], a_2 => a_3 => [], a_4 => [0]);
const ps = (c, d) => (() => {
  const fF = c[0];
  return (() => {
    const fV = c[1];
    return (() => {
      const reshaper1F = d[0];
      return (() => {
        const reshaper1V = d[1];
        return (() => {
          const getters1 = d[2];
          return (() => {
            const traverse1 = d[3];
            return (() => {
              const record_to_list1 = d[4];
              return (() => {
                const getters2 = traverse1(getters1)(b => (() => {
                    const f1 = b[0];
                    return (() => {
                      const v1 = b[1];
                      return ((_a0, _a1) => [_a0, _a1])((_slots => a => (() => {
                          const sh2 = a[1];
                          return _slots[0](sh2);
                        })())([f1]), (_slots => a_1 => (() => {
                          const sh2_1 = a_1[1];
                          return _slots[0](sh2_1);
                        })())([v1]));
                    })();
                  })());
                return ((_a0, _a1, _a2, _a3, _a4) => [_a0,
                  _a1,
                  _a2,
                  _a3,
                  _a4])((_slots => sh1 => ((_a0, _a1) => [_a0,
                    _a1])(_slots[0](sh1), _slots[1](sh1)))([fF,
                      reshaper1F]), (_slots => sh1_1 => ((_a0, _a1) => [_a0,
                    _a1])(_slots[0](sh1_1), _slots[1](sh1_1)))([fV,
                      reshaper1V]), ((_a0, _a1) => [_a0,
                    _a1])(((_a0, _a1) => [_a0,
                      _a1])(a_2 => a_2[0], a_3 => a_3[0]), getters2), (_slots => a_4 => (() => {
                    const x = a_4[0];
                    return (() => {
                      const sh2_2 = a_4[1];
                      return (_slots => g => ((_a0, _a1) => [_a0,
                        _a1])(g(_slots[0]), _slots[1](_slots[2])(g)))([x,
                          _slots[0],
                          sh2_2]);
                    })();
                  })())([traverse1]), (_slots => a_5 => (() => {
                    let _anon2;
                    return (() => {
                      (() => {
                        _anon2 = a_5[0];
                        return true;
                      })();
                      return (() => {
                        const row_entry = _anon2[0];
                        return (() => {
                          const sh2_3 = a_5[1];
                          return Bosatsu_Predef$concat(((_a0, _a1) => [1,
                              _a0,
                              _a1])(row_entry, [0]), _slots[0](sh2_3));
                        })();
                      })();
                    })();
                  })())([record_to_list1]));
              })();
            })();
          })();
        })();
      })();
    })();
  })();
})();
const equal_List = (is_equal, l1, l2) => (() => {
  let _anon4;
  return (() => {
    let _anon5;
    return (() => {
      let _anon7;
      return (() => {
        let _anon9;
        return (() => {
          let _anon11;
          return (() => {
            (() => {
              _anon7 = is_equal;
              return true;
            })();
            return (() => {
              (() => {
                _anon9 = l1;
                return true;
              })();
              return (() => {
                (() => {
                  _anon11 = l2;
                  return true;
                })();
                return (() => {
                  (() => {
                    _anon4 = [1];
                    return true;
                  })();
                  return (() => {
                    while (_anon4[0] === 1) {
                      (_anon9[0] === 0) ? (() => {
                          (() => {
                            _anon4 = [0];
                            return true;
                          })();
                          return (() => {
                            (() => {
                              _anon5 = (_anon11[0] === 0) ? [1] : [0];
                              return true;
                            })();
                            return [];
                          })();
                        })() : (() => {
                          const h1 = _anon9[1];
                          return (() => {
                            const r1 = _anon9[2];
                            return (_anon11[0] === 0) ? (() => {
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
                              })() : (() => {
                                const h2 = _anon11[1];
                                return (() => {
                                  const r2 = _anon11[2];
                                  return (() => {
                                    const _anon3 = _anon7(h1, h2);
                                    return (_anon3[0] === 1) ? (() => {
                                        const _anon8 = r1;
                                        return (() => {
                                          const _anon10 = r2;
                                          return (() => {
                                            (() => {
                                              _anon9 = _anon8;
                                              return true;
                                            })();
                                            return (() => {
                                              (() => {
                                                _anon11 = _anon10;
                                                return true;
                                              })();
                                              return [];
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
  })();
})();
const equal_RowEntry = (re1, re2) => (() => {
  const _anon20 = ((_a0, _a1) => [_a0, _a1])(re1, re2);
  return (() => {
    let _anon23;
    return (() => {
      let _anon21;
      return (() => {
        let _anon22;
        return (() => {
          let _anon24;
          return ((() => {
            _anon23 = _anon20[1];
            return true;
          })() && ((() => {
            _anon21 = _anon20[0];
            return true;
          })() && (_anon21[0] === 0 && (() => {
            _anon22 = _anon21[1];
            return true;
          })() && (_anon23[0] === 0 && (() => {
            _anon24 = _anon23[1];
            return true;
          })())))) ? (() => {
              const x1 = _anon22[0];
              return (() => {
                const x2 = _anon24[0];
                return (() => {
                  const _anon17 = (() => {
                    const _anon12 = ((_a0, _a1) => [_a0, _a1])(x1, x2);
                    return (() => {
                      let _anon14;
                      return (() => {
                        let _anon13;
                        return ((() => {
                          _anon14 = _anon12[1];
                          return true;
                        })() && ((() => {
                          _anon13 = _anon12[0];
                          return true;
                        })() && (_anon13[0] === 1 && (_anon14[0] === 0)))) ? [2] : (() => {
                            let _anon16;
                            return (() => {
                              let _anon15;
                              return ((() => {
                                _anon16 = _anon12[1];
                                return true;
                              })() && ((() => {
                                _anon15 = _anon12[0];
                                return true;
                              })() && (_anon15[0] === 0 && (_anon16[0] === 1)))) ? [0] : [1];
                            })();
                          })();
                      })();
                    })();
                  })();
                  return (_anon17[0] === 1) ? [1] : [0];
                })();
              })();
            })() : (() => {
              let _anon27;
              return (() => {
                let _anon25;
                return (() => {
                  let _anon26;
                  return (() => {
                    let _anon28;
                    return ((() => {
                      _anon27 = _anon20[1];
                      return true;
                    })() && ((() => {
                      _anon25 = _anon20[0];
                      return true;
                    })() && (_anon25[0] === 1 && (() => {
                      _anon26 = _anon25[1];
                      return true;
                    })() && (_anon27[0] === 1 && (() => {
                      _anon28 = _anon27[1];
                      return true;
                    })())))) ? (() => {
                        const x1_1 = _anon26[0];
                        return (() => {
                          const x2_1 = _anon28[0];
                          return (() => {
                            const _anon18 = (x1_1 < x2_1) ? [0] : (x1_1 === x2_1) ? [1] : [2];
                            return (_anon18[0] === 1) ? [1] : [0];
                          })();
                        })();
                      })() : (() => {
                        let _anon31;
                        return (() => {
                          let _anon29;
                          return (() => {
                            let _anon30;
                            return (() => {
                              let _anon32;
                              return ((() => {
                                _anon31 = _anon20[1];
                                return true;
                              })() && ((() => {
                                _anon29 = _anon20[0];
                                return true;
                              })() && (_anon29[0] === 2 && (() => {
                                _anon30 = _anon29[1];
                                return true;
                              })() && (_anon31[0] === 2 && (() => {
                                _anon32 = _anon31[1];
                                return true;
                              })())))) ? (() => {
                                  const x1_2 = _anon30[0];
                                  return (() => {
                                    const x2_2 = _anon32[0];
                                    return (() => {
                                      const _anon19 = _cmp_String(x1_2, x2_2);
                                      return (_anon19[0] === 1) ? [1] : [0];
                                    })();
                                  })();
                                })() : [0];
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
const equal_rows = (a, b) => RecordSet_Library$equal_List(RecordSet_Library$equal_RowEntry, a, b);
const rs_empty = RecordSet_Library$restructure(RecordSet_Library$new_record_set, a => RecordSet_Library$ps((() => {
      const rf = ((_a0, _a1) => [_a0,
        _a1])(_js_to_bosatsu_string("String field"), _a0 => [2, _a0]);
      return ((_a0, _a1) => [_a0,
        _a1])((_slots => a_1 => _slots[0])([rf]), a_2 => (_a0 => [_a0])(_js_to_bosatsu_string("")));
    })(), RecordSet_Library$ps((() => {
        const rf_1 = ((_a0, _a1) => [_a0,
          _a1])(_js_to_bosatsu_string("Int field"), _a0 => [1, _a0]);
        return ((_a0, _a1) => [_a0,
          _a1])((_slots => a_3 => _slots[0])([rf_1]), a_4 => (_a0 => [_a0])(0));
      })(), RecordSet_Library$ps((() => {
          const rf_2 = ((_a0, _a1) => [_a0,
            _a1])(_js_to_bosatsu_string("Bool field"), _a0 => [0, _a0]);
          return ((_a0, _a1) => [_a0,
            _a1])((_slots => a_5 => _slots[0])([rf_2]), a_6 => (_a0 => [_a0])([1]));
        })(), RecordSet_Library$ps_end))));
const rs = (() => {
  const fields = RecordSet_Library$rs_empty[0];
  return (() => {
    const rows = RecordSet_Library$rs_empty[1];
    return (() => {
      const getters = RecordSet_Library$rs_empty[2];
      return (() => {
        const traverse = RecordSet_Library$rs_empty[3];
        return (() => {
          const record_to_list = RecordSet_Library$rs_empty[4];
          return ((_a0, _a1, _a2, _a3, _a4) => [_a0,
            _a1,
            _a2,
            _a3,
            _a4])(fields, Bosatsu_Predef$concat(rows, ((_a0, _a1) => [1,
                _a0,
                _a1])(((_a0, _a1) => [_a0,
                  _a1])((_a0 => [_a0])(_js_to_bosatsu_string("a")), ((_a0, _a1) => [_a0,
                    _a1])((_a0 => [_a0])(1), ((_a0, _a1) => [_a0,
                      _a1])((_a0 => [_a0])([0]), []))), [0])), getters, traverse, record_to_list);
        })();
      })();
    })();
  })();
})();
const rs0 = RecordSet_Library$restructure(RecordSet_Library$rs, d => (() => {
    let _anon34;
    return (() => {
      let _anon35;
      return (() => {
        (() => {
          _anon34 = d[1];
          return true;
        })() && (() => {
          _anon35 = _anon34[1];
          return true;
        })();
        return (() => {
          const a = d[0];
          return (() => {
            const b = _anon34[0];
            return (() => {
              const c = _anon35[0];
              return RecordSet_Library$ps(c, RecordSet_Library$ps(b, RecordSet_Library$ps(a, RecordSet_Library$ps((() => {
                        const rf = ((_a0, _a1) => [_a0,
                          _a1])(_js_to_bosatsu_string("Plus 2"), _a0 => [1,
                            _a0]);
                        return ((_a0, _a1) => [_a0,
                          _a1])((_slots => a_1 => _slots[0])([rf]), (_slots => sh => (_a0 => [_a0])(2 + (() => {
                              const getter = _slots[0][1];
                              return (() => {
                                const _anon33 = getter(sh);
                                return _anon33[0];
                              })();
                            })()))([b]));
                      })(), RecordSet_Library$ps_end))));
            })();
          })();
        })();
      })();
    })();
  })());
const tests = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("reordering"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0,
      _a0,
      _a1])(RecordSet_Library$equal_List(RecordSet_Library$equal_rows, RecordSet_Library$list_of_rows(RecordSet_Library$rs0), ((_a0, _a1) => [1,
          _a0,
          _a1])(((_a0, _a1) => [1, _a0, _a1])((_a0 => [0,
              _a0])((_a0 => [_a0])([0])), ((_a0, _a1) => [1,
              _a0,
              _a1])((_a0 => [1, _a0])((_a0 => [_a0])(1)), ((_a0, _a1) => [1,
                _a0,
                _a1])((_a0 => [2,
                  _a0])((_a0 => [_a0])(_js_to_bosatsu_string("a"))), ((_a0, _a1) => [1,
                  _a0,
                  _a1])((_a0 => [1,
                    _a0])((_a0 => [_a0])(3)), [0])))), [0])), _js_to_bosatsu_string("swap")), [0]));
export {list_of_rows};