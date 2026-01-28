// Import runtime
// require("./../_runtime.js") or import from "./../_runtime.js"

const rotation = (left, right, max_diff) => (() => {
  const _anon3 = (() => {
    const _anon0 = (left - right < max_diff) ? [0] : (left - right === max_diff) ? [1] : [2];
    return (_anon0[0] === 2) ? [1] : [0];
  })();
  return (_anon3[0] === 1) ? [2] : (() => {
      const _anon2 = (() => {
        const _anon1 = (right - left < max_diff) ? [0] : (right - left === max_diff) ? [1] : [2];
        return (_anon1[0] === 2) ? [1] : [0];
      })();
      return (_anon2[0] === 1) ? [0] : [1];
    })();
})();
const max = (i, j) => (() => {
  const _anon4 = (i < j) ? [0] : (i === j) ? [1] : [2];
  return (_anon4[0] === 0) ? j : i;
})();
const branch = (sz, item, left, right) => (() => {
  const i = (left[0] === 0) ? 0 : left[2];
  return (() => {
    const j = (right[0] === 0) ? 0 : right[2];
    return ((_a0, _a1, _a2, _a3, _a4) => [1,
      _a0,
      _a1,
      _a2,
      _a3,
      _a4])(sz, 1 + (() => {
        const _anon5 = (i < j) ? [0] : (i === j) ? [1] : [2];
        return (_anon5[0] === 0) ? j : i;
      })(), item, left, right);
  })();
})();
const branch_s = (item, left, right) => AvlTree$branch(1 + (((left[0] === 0) ? 0 : left[1]) + ((right[0] === 0) ? 0 : right[1])), item, left, right);
const balance = t => (t[0] === 0) ? [0] : (() => {
    const top_item = t[3];
    return (() => {
      const left = t[4];
      return (() => {
        const right = t[5];
        return (() => {
          const _anon8 = AvlTree$rotation((left[0] === 0) ? 0 : left[2], (right[0] === 0) ? 0 : right[2], 1);
          return (_anon8[0] === 1) ? t : (_anon8[0] === 2) ? (left[0] === 0) ? t : (() => {
                  const inner_item = left[3];
                  return (() => {
                    const left_1 = left[4];
                    return (() => {
                      const left_right = left_1[5];
                      return (() => {
                        const _anon6 = AvlTree$rotation((left_1[0] === 0) ? 0 : left_1[2], (left_right[0] === 0) ? 0 : left_right[2], 0);
                        return (_anon6[0] === 2) ? AvlTree$branch_s(inner_item, left_1, AvlTree$branch_s(top_item, left_right, right)) : (_anon6[0] === 1) ? AvlTree$branch_s(inner_item, left_1, AvlTree$branch_s(top_item, left_right, right)) : (left_right[0] === 0) ? _trace(_js_to_bosatsu_string("unreachable"), t) : (() => {
                                const lrv = left_right[3];
                                return (() => {
                                  const left_right_left = left_right[4];
                                  return (() => {
                                    const left_right_right = left_right[5];
                                    return AvlTree$branch_s(lrv, AvlTree$branch_s(inner_item, left_1, left_right_left), AvlTree$branch_s(top_item, left_right_right, right));
                                  })();
                                })();
                              })();
                      })();
                    })();
                  })();
                })() : (right[0] === 0) ? t : (() => {
                  const inner_item_1 = right[3];
                  return (() => {
                    const right_left = right[4];
                    return (() => {
                      const right_right = right[5];
                      return (() => {
                        const _anon7 = AvlTree$rotation((right_left[0] === 0) ? 0 : right_left[2], (right_right[0] === 0) ? 0 : right_right[2], 0);
                        return (_anon7[0] === 0) ? AvlTree$branch_s(inner_item_1, AvlTree$branch_s(top_item, left, right_left), right_right) : (_anon7[0] === 1) ? AvlTree$branch_s(inner_item_1, AvlTree$branch_s(top_item, left, right_left), right_right) : (right_left[0] === 0) ? _trace(_js_to_bosatsu_string("unreachable"), t) : (() => {
                                const right_left_key = right_left[3];
                                return (() => {
                                  const right_left_right = right_left[4];
                                  return (() => {
                                    const right_left_left = right_left[5];
                                    return AvlTree$branch_s(right_left_key, AvlTree$branch_s(top_item, left, right_left_left), AvlTree$branch_s(inner_item_1, right_left_right, right_right));
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
const add_item = (ord, tree, item) => (() => {
  const fn = ord[0];
  return (() => {
    const loop = (_slots => tree_1 => (tree_1[0] === 0) ? ((_a0, _a1, _a2, _a3, _a4) => [1,
        _a0,
        _a1,
        _a2,
        _a3,
        _a4])(1, 1, _slots[1], [0], [0]) : (() => {
        const s = tree_1[1];
        return (() => {
          const h = tree_1[2];
          return (() => {
            const item0 = tree_1[3];
            return (() => {
              const left = tree_1[4];
              return (() => {
                const right = tree_1[5];
                return (() => {
                  const _anon9 = _slots[0](_slots[1], item0);
                  return (_anon9[0] === 1) ? ((_a0, _a1, _a2, _a3, _a4) => [1,
                      _a0,
                      _a1,
                      _a2,
                      _a3,
                      _a4])(s, h, _slots[1], left, right) : (_anon9[0] === 0) ? AvlTree$balance(AvlTree$branch(1 + s, item0, loop(left), right)) : AvlTree$balance(AvlTree$branch(1 + s, item0, left, loop(right)));
                })();
              })();
            })();
          })();
        })();
      })())([fn, item]);
    return loop(tree);
  })();
})();
const contains = (ord, tree, item) => (() => {
  const fn = ord[0];
  return (() => {
    const loop = (_slots => tree_1 => (() => {
      let _anon11;
      return (() => {
        let _anon12;
        return (() => {
          let _anon14;
          return (() => {
            (() => {
              _anon14 = tree_1;
              return true;
            })();
            return (() => {
              (() => {
                _anon11 = [1];
                return true;
              })();
              return (() => {
                while (_anon11[0] === 1) {
                  (_anon14[0] === 0) ? (() => {
                      (() => {
                        _anon11 = [0];
                        return true;
                      })();
                      return (() => {
                        (() => {
                          _anon12 = [0];
                          return true;
                        })();
                        return [];
                      })();
                    })() : (() => {
                      const key = _anon14[3];
                      return (() => {
                        const left = _anon14[4];
                        return (() => {
                          const right = _anon14[5];
                          return (() => {
                            const _anon10 = _slots[0](_slots[1], key);
                            return (_anon10[0] === 1) ? (() => {
                                (() => {
                                  _anon11 = [0];
                                  return true;
                                })();
                                return (() => {
                                  (() => {
                                    _anon12 = (_a0 => [1, _a0])(key);
                                    return true;
                                  })();
                                  return [];
                                })();
                              })() : (_anon10[0] === 0) ? (() => {
                                  const _anon13 = left;
                                  return (() => {
                                    (() => {
                                      _anon14 = _anon13;
                                      return true;
                                    })();
                                    return [];
                                  })();
                                })() : (() => {
                                  const _anon13 = right;
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
                }
                return _anon12;
              })();
            })();
          })();
        })();
      })();
    })())([fn, item]);
    return loop(tree);
  })();
})();
const min = tree => (() => {
  let _anon16;
  return (() => {
    let _anon17;
    return (() => {
      let _anon19;
      return (() => {
        (() => {
          _anon19 = tree;
          return true;
        })();
        return (() => {
          (() => {
            _anon16 = [1];
            return true;
          })();
          return (() => {
            while (_anon16[0] === 1) {
              (_anon19[0] === 0) ? (() => {
                  (() => {
                    _anon16 = [0];
                    return true;
                  })();
                  return (() => {
                    (() => {
                      _anon17 = [0];
                      return true;
                    })();
                    return [];
                  })();
                })() : (() => {
                  let _anon15;
                  return (_anon19[0] === 1 && (() => {
                    _anon15 = _anon19[4];
                    return true;
                  })() && (_anon15[0] === 0)) ? (() => {
                      (() => {
                        _anon16 = [0];
                        return true;
                      })();
                      return (() => {
                        (() => {
                          _anon17 = (() => {
                            const key = _anon19[3];
                            return (_a0 => [1, _a0])(key);
                          })();
                          return true;
                        })();
                        return [];
                      })();
                    })() : (() => {
                      const left = _anon19[4];
                      return (() => {
                        const _anon18 = left;
                        return (() => {
                          (() => {
                            _anon19 = _anon18;
                            return true;
                          })();
                          return [];
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
const remove_item = (ord, tree, item) => (() => {
  const fn = ord[0];
  return (() => {
    const loop = (_slots => tree_1 => (tree_1[0] === 0) ? [0] : (() => {
        const size = tree_1[1];
        return (() => {
          const key = tree_1[3];
          return (() => {
            const left = tree_1[4];
            return (() => {
              const right = tree_1[5];
              return (() => {
                const _anon20 = _slots[0](_slots[1], key);
                return (_anon20[0] === 1) ? (right[0] === 0) ? left : AvlTree$balance(AvlTree$branch((-1) + size, key, left, loop(right))) : (_anon20[0] === 0) ? AvlTree$balance(AvlTree$branch((-1) + size, key, loop(left), right)) : AvlTree$balance(AvlTree$branch((-1) + size, key, left, loop(right)));
              })();
            })();
          })();
        })();
      })())([fn, item]);
    return loop(tree);
  })();
})();
const fold_left_Tree = (t, left_v, fn) => (t[0] === 0) ? left_v : (() => {
    const key = t[3];
    return (() => {
      const left = t[4];
      return (() => {
        const right = t[5];
        return fold_left_Tree(right, fn(fold_left_Tree(left, left_v, fn), key), fn);
      })();
    })();
  })();
const fold_right_Tree = (t, right_v, fn) => (t[0] === 0) ? right_v : (() => {
    const key = t[3];
    return (() => {
      const left = t[4];
      return (() => {
        const right = t[5];
        return fold_right_Tree(left, fn(key, fold_right_Tree(right, right_v, fn)), fn);
      })();
    })();
  })();
const _module = ord => ((_a0, _a1, _a2, _a3, _a4, _a5, _a6, _a7) => [_a0,
  _a1,
  _a2,
  _a3,
  _a4,
  _a5,
  _a6,
  _a7])(ord, [0], (_slots => (t, a) => AvlTree$add_item(_slots[0], t, a))([ord]), (_slots => a_1 => AvlTree$add_item(_slots[0], [0], a_1))([ord]), (_slots => (t_1, a_2) => AvlTree$contains(_slots[0], t_1, a_2))([ord]), (_slots => (t_2, a_3) => AvlTree$remove_item(_slots[0], t_2, a_3))([ord]), AvlTree$fold_left_Tree, AvlTree$fold_right_Tree);
const _a = AvlTree$_module((_a0 => [_a0])((_a0, _a1) => (_a0 < _a1) ? [0] : (_a0 === _a1) ? [1] : [2]));
const contains_test = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("contains tests"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
        const _anon21 = AvlTree$_a[4](AvlTree$_a[2]([0], 2), 2);
        return (_anon21[0] === 1) ? [1] : [0];
      })(), _js_to_bosatsu_string("Empty.add_law 2")), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
          const _anon22 = AvlTree$_a[4](AvlTree$_a[2](AvlTree$_a[3](2), 2), 2);
          return (_anon22[0] === 1) ? [1] : [0];
        })(), _js_to_bosatsu_string("single(2) + 2 add_law")), ((_a0, _a1) => [1,
        _a0,
        _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
            const _anon23 = AvlTree$_a[4](AvlTree$_a[2](AvlTree$_a[3](3), 42), 42);
            return (_anon23[0] === 1) ? [1] : [0];
          })(), _js_to_bosatsu_string("single(3) add_law 42")), ((_a0, _a1) => [1,
          _a0,
          _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
              const _anon25 = (() => {
                const _anon24 = AvlTree$_a[4](AvlTree$_a[3](2), 3);
                return (_anon24[0] === 1) ? [1] : [0];
              })();
              return (_anon25[0] === 1) ? [0] : [1];
            })(), _js_to_bosatsu_string("single(2) ! contains 3")), ((_a0, _a1) => [1,
            _a0,
            _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                const _anon27 = (() => {
                  const _anon26 = AvlTree$_a[4](AvlTree$_a[5](AvlTree$_a[3](2), 2), 2);
                  return (_anon26[0] === 1) ? [1] : [0];
                })();
                return (_anon27[0] === 1) ? [0] : [1];
              })(), _js_to_bosatsu_string("Empty + 2 - 2, !contains(2)")), ((_a0, _a1) => [1,
              _a0,
              _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                  const _anon29 = (() => {
                    const _anon28 = AvlTree$_a[4](AvlTree$_a[5](AvlTree$_a[5](AvlTree$_a[3](2), 2), 2), 2);
                    return (_anon28[0] === 1) ? [1] : [0];
                  })();
                  return (_anon29[0] === 1) ? [0] : [1];
                })(), _js_to_bosatsu_string("Empty + 2 - 2, !contains(2)")), [0])))))));
const add_increases_size = (t, i, msg) => ((_a0, _a1) => [0,
  _a0,
  _a1])(((() => {
    const _anon30 = AvlTree$_a[2](t, i);
    return (_anon30[0] === 0) ? 0 : _anon30[1];
  })() - ((t[0] === 0) ? 0 : t[1]) === 1) ? [1] : [0], msg);
const rem_decreases_size = (t, i, msg) => ((_a0, _a1) => [0,
  _a0,
  _a1])((((t[0] === 0) ? 0 : t[1]) - (() => {
    const _anon31 = AvlTree$_a[5](t, i);
    return (_anon31[0] === 0) ? 0 : _anon31[1];
  })() === 1) ? [1] : [0], msg);
const size_tests = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("size tests"), ((_a0, _a1) => [1,
    _a0,
    _a1])(AvlTree$add_increases_size([0], 1, _js_to_bosatsu_string("Empty.add(1)")), ((_a0, _a1) => [1,
      _a0,
      _a1])(AvlTree$add_increases_size(AvlTree$_a[3](1), 2, _js_to_bosatsu_string("single(1).add(2)")), ((_a0, _a1) => [1,
        _a0,
        _a1])(((_a0, _a1) => [0, _a0, _a1])(((() => {
            const _anon32 = AvlTree$_a[3](1);
            return (_anon32[0] === 0) ? 0 : _anon32[1];
          })() === (() => {
            const _anon33 = AvlTree$_a[2](AvlTree$_a[3](1), 1);
            return (_anon33[0] === 0) ? 0 : _anon33[1];
          })()) ? [1] : [0], _js_to_bosatsu_string("single(1) + 1 has same size")), ((_a0, _a1) => [1,
          _a0,
          _a1])(AvlTree$rem_decreases_size(AvlTree$_a[3](1), 1, _js_to_bosatsu_string("single(1) - 1")), ((_a0, _a1) => [1,
            _a0,
            _a1])(AvlTree$rem_decreases_size(AvlTree$_a[2](AvlTree$_a[3](2), 3), 2, _js_to_bosatsu_string("single(2) + 3 - 2")), [0]))))));
const log2 = i => _int_loop(i, 0, (n, cnt) => ((_a0, _a1) => [_a0,
    _a1])(2 ? Math.trunc(n / 2) : 0, 1 + cnt));
const height_tests = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("height_tests"), Bosatsu_Predef$map_List(range(30), n => (() => {
      const t = foldl_List(range(n), [0], AvlTree$_a[2]);
      return (() => {
        const h = (t[0] === 0) ? 0 : t[2];
        return (() => {
          const n1 = (t[0] === 0) ? 0 : t[1];
          return ((_a0, _a1) => [0, _a0, _a1])((() => {
              const _anon34 = (h + h < (AvlTree$log2(2 + n1) + (AvlTree$log2(2 + n1) + AvlTree$log2(2 + n1)))) ? [0] : (h + h === (AvlTree$log2(2 + n1) + (AvlTree$log2(2 + n1) + AvlTree$log2(2 + n1)))) ? [1] : [2];
              return (_anon34[0] === 0) ? [1] : [0];
            })(), _concat_String(((_a0, _a1) => [1,
                _a0,
                _a1])(_js_to_bosatsu_string("size_law for range("), ((_a0, _a1) => [1,
                  _a0,
                  _a1])(_int_to_String(n1), ((_a0, _a1) => [1,
                    _a0,
                    _a1])(_js_to_bosatsu_string(")"), [0])))));
        })();
      })();
    })()));
const fold_left_tests = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("fold_left_tests"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0,
      _a0,
      _a1])((AvlTree$fold_left_Tree(foldl_List(range(100), [0], AvlTree$_a[2]), 0, (i, a) => 1 + i) === 100) ? [1] : [0], _js_to_bosatsu_string("sum 100")), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1) => [0,
        _a0,
        _a1])((AvlTree$fold_left_Tree(foldl_List(range(100), [0], AvlTree$_a[2]), 0, AvlTree$max) === 99) ? [1] : [0], _js_to_bosatsu_string("max 100")), ((_a0, _a1) => [1,
        _a0,
        _a1])(((_a0, _a1) => [0,
          _a0,
          _a1])((AvlTree$fold_left_Tree(foldl_List(range(100), [0], AvlTree$_a[2]), (-1), (acc, i_1) => (acc === (-1)) ? i_1 : acc) === 0) ? [1] : [0], _js_to_bosatsu_string("first is 0")), [0]))));
const fold_right_tests = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("fold_right_tests"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0,
      _a0,
      _a1])((AvlTree$fold_right_Tree(foldl_List(range(100), [0], AvlTree$_a[2]), 0, (a, i) => 1 + i) === 100) ? [1] : [0], _js_to_bosatsu_string("sum 100")), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1) => [0,
        _a0,
        _a1])((AvlTree$fold_right_Tree(foldl_List(range(100), [0], AvlTree$_a[2]), 0, AvlTree$max) === 99) ? [1] : [0], _js_to_bosatsu_string("max 100")), ((_a0, _a1) => [1,
        _a0,
        _a1])(((_a0, _a1) => [0,
          _a0,
          _a1])((AvlTree$fold_right_Tree(foldl_List(range(100), [0], AvlTree$_a[2]), (-1), (i_1, acc) => (acc === (-1)) ? i_1 : acc) === 99) ? [1] : [0], _js_to_bosatsu_string("last is 99")), [0]))));
const tests = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("AvlTree tests"), ((_a0, _a1) => [1,
    _a0,
    _a1])(AvlTree$contains_test, ((_a0, _a1) => [1,
      _a0,
      _a1])(AvlTree$size_tests, ((_a0, _a1) => [1,
        _a0,
        _a1])(AvlTree$height_tests, ((_a0, _a1) => [1,
          _a0,
          _a1])(AvlTree$fold_left_tests, ((_a0, _a1) => [1,
            _a0,
            _a1])(AvlTree$fold_right_tests, [0]))))));
export {rotation};