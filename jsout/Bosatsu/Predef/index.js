// Import runtime
// require("./../../_runtime.js") or import from "./../../_runtime.js"

const build_List = fn => fn((_a0, _a1) => [1, _a0, _a1], [0]);
const foldr_List = (list, fn, acc) => (() => {
  const loop = (_slots => list_1 => (list_1[0] === 0) ? _slots[0] : (() => {
      const h = list_1[1];
      return (() => {
        const t = list_1[2];
        return _slots[1](h, loop(t));
      })();
    })())([acc, fn]);
  return loop(list);
})();
const foldl_List = (lst, item, fn) => (() => {
  const loop = (_slots => (lst_1, item_1) => (() => {
    let _anon0;
    return (() => {
      let _anon1;
      return (() => {
        let _anon3;
        return (() => {
          let _anon5;
          return (() => {
            (() => {
              _anon3 = lst_1;
              return true;
            })();
            return (() => {
              (() => {
                _anon5 = item_1;
                return true;
              })();
              return (() => {
                (() => {
                  _anon0 = [1];
                  return true;
                })();
                return (() => {
                  while (_anon0[0] === 1) {
                    (_anon3[0] === 0) ? (() => {
                        (() => {
                          _anon0 = [0];
                          return true;
                        })();
                        return (() => {
                          (() => {
                            _anon1 = _anon5;
                            return true;
                          })();
                          return [];
                        })();
                      })() : (() => {
                        const head = _anon3[1];
                        return (() => {
                          const tail = _anon3[2];
                          return (() => {
                            const _anon2 = tail;
                            return (() => {
                              const _anon4 = _slots[0](_anon5, head);
                              return (() => {
                                (() => {
                                  _anon3 = _anon2;
                                  return true;
                                })();
                                return (() => {
                                  (() => {
                                    _anon5 = _anon4;
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
                  return _anon1;
                })();
              })();
            })();
          })();
        })();
      })();
    })();
  })())([fn]);
  return loop(lst, item);
})();
const reverse_concat = (front, back) => foldl_List(front, back, (tail, h) => ((_a0, _a1) => [1,
    _a0,
    _a1])(h, tail));
const reverse = as => Bosatsu_Predef$reverse_concat(as, [0]);
const concat = (front, back) => (back[0] === 0) ? front : Bosatsu_Predef$reverse_concat(Bosatsu_Predef$reverse_concat(front, [0]), back);
const map_List = (lst, fn) => Bosatsu_Predef$reverse(foldl_List(lst, [0], (_slots => (t, a) => ((_a0, _a1) => [1,
      _a0,
      _a1])(_slots[0](a), t))([fn])));
const flat_map_List = (lst, fn) => Bosatsu_Predef$reverse(foldl_List(lst, [0], (_slots => (t, a) => (() => {
      const front = _slots[0](a);
      return foldl_List(front, t, (tail, h) => ((_a0, _a1) => [1,
          _a0,
          _a1])(h, tail));
    })())([fn])));
const replicate_List = (item, cnt) => _int_loop(cnt, [0], (_slots => (i, acc) => ((_a0, _a1) => [_a0,
    _a1])((-1) + i, ((_a0, _a1) => [1, _a0, _a1])(_slots[0], acc)))([item]));
const uncurry2 = f => (_slots => (x1, x2) => _slots[0](x1)(x2))([f]);
const uncurry3 = f => (_slots => (x1, x2, x3) => _slots[0](x1)(x2)(x3))([f]);
const range = exclusiveUpper => _int_loop(exclusiveUpper, [0], (i, tail) => (() => {
    const inext = (-1) + i;
    return ((_a0, _a1) => [_a0, _a1])(inext, ((_a0, _a1) => [1,
        _a0,
        _a1])(inext, tail));
  })());
const range_fold = (inclusiveLower, exclusiveUpper, init, fn) => (() => {
  const diff = exclusiveUpper - inclusiveLower;
  return _int_loop(diff, init, (_slots => (diff0, a) => ((_a0, _a1) => [_a0,
      _a1])((-1) + diff0, _slots[0](a, _slots[1] - diff0)))([fn,
        exclusiveUpper]));
})();
const string_Order = (_a0 => [_a0])((_a0, _a1) => _cmp_String(_a0, _a1));
const rotation = (left, right, max_diff) => (() => {
  const _anon9 = (() => {
    const _anon6 = (left - right < max_diff) ? [0] : (left - right === max_diff) ? [1] : [2];
    return (_anon6[0] === 2) ? [1] : [0];
  })();
  return (_anon9[0] === 1) ? [2] : (() => {
      const _anon8 = (() => {
        const _anon7 = (right - left < max_diff) ? [0] : (right - left === max_diff) ? [1] : [2];
        return (_anon7[0] === 2) ? [1] : [0];
      })();
      return (_anon8[0] === 1) ? [0] : [1];
    })();
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
        const _anon10 = (i < j) ? [0] : (i === j) ? [1] : [2];
        return (_anon10[0] === 0) ? j : i;
      })(), item, left, right);
  })();
})();
const branch_s = (item, left, right) => Bosatsu_Predef$branch(1 + (((left[0] === 0) ? 0 : left[1]) + ((right[0] === 0) ? 0 : right[1])), item, left, right);
const balance = t => (t[0] === 0) ? [0] : (() => {
    const top_item = t[3];
    return (() => {
      const left = t[4];
      return (() => {
        const right = t[5];
        return (() => {
          const _anon13 = Bosatsu_Predef$rotation((left[0] === 0) ? 0 : left[2], (right[0] === 0) ? 0 : right[2], 1);
          return (_anon13[0] === 1) ? t : (_anon13[0] === 2) ? (left[0] === 0) ? t : (() => {
                  const inner_item = left[3];
                  return (() => {
                    const left_1 = left[4];
                    return (() => {
                      const left_right = left_1[5];
                      return (() => {
                        const _anon11 = Bosatsu_Predef$rotation((left_1[0] === 0) ? 0 : left_1[2], (left_right[0] === 0) ? 0 : left_right[2], 0);
                        return (_anon11[0] === 2) ? Bosatsu_Predef$branch_s(inner_item, left_1, Bosatsu_Predef$branch_s(top_item, left_right, right)) : (_anon11[0] === 1) ? Bosatsu_Predef$branch_s(inner_item, left_1, Bosatsu_Predef$branch_s(top_item, left_right, right)) : (left_right[0] === 0) ? _trace(_js_to_bosatsu_string("unreachable"), t) : (() => {
                                const lrv = left_right[3];
                                return (() => {
                                  const left_right_left = left_right[4];
                                  return (() => {
                                    const left_right_right = left_right[5];
                                    return Bosatsu_Predef$branch_s(lrv, Bosatsu_Predef$branch_s(inner_item, left_1, left_right_left), Bosatsu_Predef$branch_s(top_item, left_right_right, right));
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
                        const _anon12 = Bosatsu_Predef$rotation((right_left[0] === 0) ? 0 : right_left[2], (right_right[0] === 0) ? 0 : right_right[2], 0);
                        return (_anon12[0] === 0) ? Bosatsu_Predef$branch_s(inner_item_1, Bosatsu_Predef$branch_s(top_item, left, right_left), right_right) : (_anon12[0] === 1) ? Bosatsu_Predef$branch_s(inner_item_1, Bosatsu_Predef$branch_s(top_item, left, right_left), right_right) : (right_left[0] === 0) ? _trace(_js_to_bosatsu_string("unreachable"), t) : (() => {
                                const right_left_key = right_left[3];
                                return (() => {
                                  const right_left_right = right_left[4];
                                  return (() => {
                                    const right_left_left = right_left[5];
                                    return Bosatsu_Predef$branch_s(right_left_key, Bosatsu_Predef$branch_s(top_item, left, right_left_left), Bosatsu_Predef$branch_s(inner_item_1, right_left_right, right_right));
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
                  const _anon14 = _slots[0](_slots[1], item0);
                  return (_anon14[0] === 1) ? ((_a0, _a1, _a2, _a3, _a4) => [1,
                      _a0,
                      _a1,
                      _a2,
                      _a3,
                      _a4])(s, h, _slots[1], left, right) : (_anon14[0] === 0) ? Bosatsu_Predef$balance(Bosatsu_Predef$branch(1 + s, item0, loop(left), right)) : Bosatsu_Predef$balance(Bosatsu_Predef$branch(1 + s, item0, left, loop(right)));
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
      let _anon16;
      return (() => {
        let _anon17;
        return (() => {
          let _anon19;
          return (() => {
            (() => {
              _anon19 = tree_1;
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
                      const key = _anon19[3];
                      return (() => {
                        const left = _anon19[4];
                        return (() => {
                          const right = _anon19[5];
                          return (() => {
                            const _anon15 = _slots[0](_slots[1], key);
                            return (_anon15[0] === 1) ? (() => {
                                (() => {
                                  _anon16 = [0];
                                  return true;
                                })();
                                return (() => {
                                  (() => {
                                    _anon17 = (_a0 => [1, _a0])(key);
                                    return true;
                                  })();
                                  return [];
                                })();
                              })() : (_anon15[0] === 0) ? (() => {
                                  const _anon18 = left;
                                  return (() => {
                                    (() => {
                                      _anon19 = _anon18;
                                      return true;
                                    })();
                                    return [];
                                  })();
                                })() : (() => {
                                  const _anon18 = right;
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
                      })();
                    })();
                }
                return _anon17;
              })();
            })();
          })();
        })();
      })();
    })())([fn, item]);
    return loop(tree);
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
                return (_anon20[0] === 1) ? (right[0] === 0) ? left : Bosatsu_Predef$balance(Bosatsu_Predef$branch((-1) + size, key, left, loop(right))) : (_anon20[0] === 0) ? Bosatsu_Predef$balance(Bosatsu_Predef$branch((-1) + size, key, loop(left), right)) : Bosatsu_Predef$balance(Bosatsu_Predef$branch((-1) + size, key, left, loop(right)));
              })();
            })();
          })();
        })();
      })())([fn, item]);
    return loop(tree);
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
const empty_Dict = comp => (() => {
  const fn = comp[0];
  return ((_a0, _a1) => [_a0, _a1])((_a0 => [_a0])((_slots => (a, b) => (() => {
        const k1 = a[0];
        return (() => {
          const k2 = b[0];
          return _slots[0](k1, k2);
        })();
      })())([fn])), [0]);
})();
const add_key = (dict, key, value) => (() => {
  const ord = dict[0];
  return (() => {
    const tree = dict[1];
    return ((_a0, _a1) => [_a0,
      _a1])(ord, Bosatsu_Predef$add_item(ord, tree, ((_a0, _a1) => [_a0,
          _a1])(key, value)));
  })();
})();
const get_key = (dict, key) => (() => {
  const ord = dict[0];
  return (() => {
    const tree = dict[1];
    return (() => {
      let _anon23;
      return (tree[0] === 1 && (() => {
        _anon23 = tree[3];
        return true;
      })()) ? (() => {
          const v = _anon23[1];
          return (() => {
            const _anon21 = Bosatsu_Predef$contains(ord, tree, ((_a0, _a1) => [_a0,
                _a1])(key, v));
            return (() => {
              let _anon22;
              return (_anon21[0] === 1 && (() => {
                _anon22 = _anon21[1];
                return true;
              })()) ? (() => {
                  const v_1 = _anon22[1];
                  return (_a0 => [1, _a0])(v_1);
                })() : [0];
            })();
          })();
        })() : [0];
    })();
  })();
})();
const remove_key = (dict, key) => (() => {
  const ord = dict[0];
  return (() => {
    const tree = dict[1];
    return (() => {
      let _anon24;
      return (tree[0] === 1 && (() => {
        _anon24 = tree[3];
        return true;
      })()) ? (() => {
          const v = _anon24[1];
          return ((_a0, _a1) => [_a0,
            _a1])(ord, Bosatsu_Predef$remove_item(ord, tree, ((_a0, _a1) => [_a0,
                _a1])(key, v)));
        })() : dict;
    })();
  })();
})();
const items = dict => (() => {
  const tree = dict[1];
  return Bosatsu_Predef$fold_right_Tree(tree, [0], (_a0, _a1) => [1, _a0, _a1]);
})();
const clear_Dict = dict => (() => {
  const ord = dict[0];
  return ((_a0, _a1) => [_a0, _a1])(ord, [0]);
})();
export {build_List};