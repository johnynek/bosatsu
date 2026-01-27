// Bosatsu JS Runtime
// Note: Using 'var' for all declarations to create true globals accessible from generated code
// GCD using Euclidean algorithm
var _gcd = (a, b) => {
  a = Math.abs(a);
  b = Math.abs(b);
  while (b !== 0) {
    const t = b;
    b = a % b;
    a = t;
  }
  return a;
};

// int_loop(i, state, fn) - countdown loop with accumulator
// fn(i, state) returns [newI, newState]
// continues while newI > 0 AND newI < i (ensures progress)
var _int_loop = (i, state, fn) => {
  let _i = i;
  let _state = state;
  while (_i > 0) {
    const result = fn(_i, _state);
    const newI = result[0];
    const newState = result[1];
    // Update state regardless
    _state = newState;
    // Check if we should continue
    if (newI <= 0 || newI >= _i) {
      // Reached 0 or no progress, return new state
      return _state;
    }
    _i = newI;
  }
  return _state;
};

// Convert Bosatsu string list to JS string
var _bosatsu_to_js_string = (bstr) => {
  let result = '';
  let current = bstr;
  while (current[0] === 1) {
    result += current[1];
    current = current[2];
  }
  return result;
};

// Convert JS string to Bosatsu string list
var _js_to_bosatsu_string = (str) => {
  let result = [0]; // Empty list
  for (let i = str.length - 1; i >= 0; i--) {
    result = [1, str[i], result];
  }
  return result;
};

// concat_String - takes a Bosatsu list of strings and concatenates
var _concat_String = (strList) => {
  let result = '';
  let current = strList;
  while (current[0] === 1) {
    result += _bosatsu_to_js_string(current[1]);
    current = current[2];
  }
  return _js_to_bosatsu_string(result);
};

// int_to_String
var _int_to_String = (n) => _js_to_bosatsu_string(String(n));

// string_to_Int - returns Option: [0] for None, [1, value] for Some
var _string_to_Int = (bstr) => {
  const str = _bosatsu_to_js_string(bstr);
  const n = parseInt(str, 10);
  return isNaN(n) ? [0] : [1, n];
};

// char_to_String - char is already a single-char string
var _char_to_String = (c) => [1, c, [0]];

// trace - log message and return value
var _trace = (msg, value) => {
  console.log(_bosatsu_to_js_string(msg));
  return value;
};

// cmp_String - compare two Bosatsu strings, return 0 (LT), 1 (EQ), or 2 (GT)
var _cmp_String = (a, b) => {
  const sa = _bosatsu_to_js_string(a);
  const sb = _bosatsu_to_js_string(b);
  return sa < sb ? 0 : (sa === sb ? 1 : 2);
};

// partition_String - split string on first occurrence of separator
// Returns tuple of (before, sep, after) or (original, empty, empty) if not found
// partition_String - returns Option[(String, String)]
// None if sep is empty or not found, Some((before, after)) otherwise
var _partition_String = (str, sep) => {
  const s = _bosatsu_to_js_string(str);
  const sp = _bosatsu_to_js_string(sep);
  // Empty separator returns None
  if (sp.length === 0) return [0];
  const idx = s.indexOf(sp);
  if (idx === -1) return [0]; // Not found: None
  // Found: Some((before, after))
  return [1, [
    _js_to_bosatsu_string(s.substring(0, idx)),
    _js_to_bosatsu_string(s.substring(idx + sp.length))
  ]];
};

// rpartition_String - returns Option[(String, String)]
// None if sep is empty or not found, Some((before, after)) otherwise
var _rpartition_String = (str, sep) => {
  const s = _bosatsu_to_js_string(str);
  const sp = _bosatsu_to_js_string(sep);
  // Empty separator returns None
  if (sp.length === 0) return [0];
  const idx = s.lastIndexOf(sp);
  if (idx === -1) return [0]; // Not found: None
  // Found: Some((before, after))
  return [1, [
    _js_to_bosatsu_string(s.substring(0, idx)),
    _js_to_bosatsu_string(s.substring(idx + sp.length))
  ]];
};

// range(n) - generate list [0, 1, 2, ..., n-1]
var range = (n) => {
  let result = [0];
  for (let i = n - 1; i >= 0; i--) {
    result = [1, i, result];
  }
  return result;
};

// foldl_List(list, init, fn) - left fold over a list
var foldl_List = (list, init, fn) => {
  let acc = init;
  let current = list;
  while (current[0] === 1) {
    acc = fn(acc, current[1]);
    current = current[2];
  }
  return acc;
};

// flat_map_List(list, fn) - flatMap over a list
var flat_map_List = (list, fn) => {
  let result = [0];
  let current = list;
  // First collect all results in reverse order
  let reversed = [0];
  while (current[0] === 1) {
    const mapped = fn(current[1]);
    // Prepend mapped items to reversed
    let m = mapped;
    while (m[0] === 1) {
      reversed = [1, m[1], reversed];
      m = m[2];
    }
    current = current[2];
  }
  // Now reverse to get correct order
  current = reversed;
  while (current[0] === 1) {
    result = [1, current[1], result];
    current = current[2];
  }
  return result;
};

// Bosatsu/Prog external functions
// Prog is represented as a thunk that takes an environment and returns [result_type, value_or_error]
// result_type: 0 = success, 1 = error
var Bosatsu_Prog$pure = a => _env => [0, a];
var Bosatsu_Prog$raise_error = e => _env => [1, e];
var Bosatsu_Prog$read_env = env => [0, env];
var Bosatsu_Prog$flat_map = (prog, fn) => env => {
  const result = prog(env);
  if (result[0] === 0) {
    return fn(result[1])(env);
  }
  return result; // propagate error
};
var Bosatsu_Prog$recover = (prog, fn) => env => {
  const result = prog(env);
  if (result[0] === 1) {
    return fn(result[1])(env);
  }
  return result;
};
var Bosatsu_Prog$apply_fix = (a, fn) => {
  const fixed = x => fn(fixed)(x);
  return fixed(a);
};
var Bosatsu_Prog$remap_env = (p, f) => env => p(f(env));
var Bosatsu_Prog$println = str => _env => { console.log(_bosatsu_to_js_string(str)); return [0, []]; };
var Bosatsu_Prog$print = str => _env => { process.stdout.write(_bosatsu_to_js_string(str)); return [0, []]; };
var Bosatsu_Prog$read_stdin_utf8_bytes = n => _env => [0, [0]]; // Return empty string for now


// Test registry
var _tests = {};

// Generated code
var Bosatsu_Predef$build_List = fn => fn((_a0, _a1) => [1, _a0, _a1], [0]);
var Bosatsu_Predef$foldr_List = (list, fn, acc) => (() => {
  const loop = (_slots => list_1 => (list_1[0] === 0) ? _slots[0] : (() => {
      const h = list_1[1];
      return (() => {
        const t = list_1[2];
        return _slots[1](h, loop(t));
      })();
    })())([acc, fn]);
  return loop(list);
})();
var Bosatsu_Predef$reverse_concat = (front, back) => foldl_List(front, back, (tail, h) => ((_a0, _a1) => [1,
    _a0,
    _a1])(h, tail));
var Bosatsu_Predef$reverse = as => Bosatsu_Predef$reverse_concat(as, [0]);
var Bosatsu_Predef$concat = (front, back) => (back[0] === 0) ? front : Bosatsu_Predef$reverse_concat(Bosatsu_Predef$reverse_concat(front, [0]), back);
var Bosatsu_Predef$map_List = (lst, fn) => Bosatsu_Predef$reverse(foldl_List(lst, [0], (_slots => (t, a) => ((_a0, _a1) => [1,
      _a0,
      _a1])(_slots[0](a), t))([fn])));
var Bosatsu_Predef$replicate_List = (item, cnt) => _int_loop(cnt, [0], (_slots => (i, acc) => ((_a0, _a1) => [_a0,
    _a1])((-1) + i, ((_a0, _a1) => [1, _a0, _a1])(_slots[0], acc)))([item]));
var Bosatsu_Predef$uncurry2 = f => (_slots => (x1, x2) => _slots[0](x1)(x2))([f]);
var Bosatsu_Predef$uncurry3 = f => (_slots => (x1, x2, x3) => _slots[0](x1)(x2)(x3))([f]);
var Bosatsu_Predef$range_fold = (inclusiveLower, exclusiveUpper, init, fn) => (() => {
  const diff = exclusiveUpper - inclusiveLower;
  return _int_loop(diff, init, (_slots => (diff0, a) => ((_a0, _a1) => [_a0,
      _a1])((-1) + diff0, _slots[0](a, _slots[1] - diff0)))([fn,
        exclusiveUpper]));
})();
var Bosatsu_Predef$string_Order = (_a0 => [_a0])((_a0, _a1) => _cmp_String(_a0, _a1));
var Bosatsu_Predef$rotation = (left, right, max_diff) => (() => {
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
var Bosatsu_Predef$branch = (sz, item, left, right) => (() => {
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
var Bosatsu_Predef$branch_s = (item, left, right) => Bosatsu_Predef$branch(1 + (((left[0] === 0) ? 0 : left[1]) + ((right[0] === 0) ? 0 : right[1])), item, left, right);
var Bosatsu_Predef$balance = t => (t[0] === 0) ? [0] : (() => {
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
var Bosatsu_Predef$add_item = (ord, tree, item) => (() => {
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
var Bosatsu_Predef$contains = (ord, tree, item) => (() => {
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
var Bosatsu_Predef$remove_item = (ord, tree, item) => (() => {
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
var Bosatsu_Predef$fold_right_Tree = (t, right_v, fn) => (t[0] === 0) ? right_v : (() => {
    const key = t[3];
    return (() => {
      const left = t[4];
      return (() => {
        const right = t[5];
        return Bosatsu_Predef$fold_right_Tree(left, fn(key, Bosatsu_Predef$fold_right_Tree(right, right_v, fn)), fn);
      })();
    })();
  })();
var Bosatsu_Predef$empty_Dict = comp => (() => {
  const fn = comp[0];
  return ((_a0, _a1) => [_a0, _a1])((_a0 => [_a0])((_slots => (a, b) => (() => {
        const k1 = a[0];
        return (() => {
          const k2 = b[0];
          return _slots[0](k1, k2);
        })();
      })())([fn])), [0]);
})();
var Bosatsu_Predef$add_key = (dict, key, value) => (() => {
  const ord = dict[0];
  return (() => {
    const tree = dict[1];
    return ((_a0, _a1) => [_a0,
      _a1])(ord, Bosatsu_Predef$add_item(ord, tree, ((_a0, _a1) => [_a0,
          _a1])(key, value)));
  })();
})();
var Bosatsu_Predef$get_key = (dict, key) => (() => {
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
var Bosatsu_Predef$remove_key = (dict, key) => (() => {
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
var Bosatsu_Predef$items = dict => (() => {
  const tree = dict[1];
  return Bosatsu_Predef$fold_right_Tree(tree, [0], (_a0, _a1) => [1, _a0, _a1]);
})();
var Bosatsu_Predef$clear_Dict = dict => (() => {
  const ord = dict[0];
  return ((_a0, _a1) => [_a0, _a1])(ord, [0]);
})();
var Ackermann$ack1 = n => (n === 0) ? n => n + 1 : (() => {
    let _anon1;
    return (() => {
      (() => {
        _anon1 = n - 1;
        return true;
      })();
      return (() => {
        const n_prev = _anon1;
        return (() => {
          const ack_p = Ackermann$ack1(n_prev);
          return (_slots => m => _slots[0]((m === 0) ? (n => n + 1)(0) : (() => {
                let _anon0;
                return (() => {
                  (() => {
                    _anon0 = m - 1;
                    return true;
                  })();
                  return (() => {
                    const m_prev = _anon0;
                    return inner(m_prev);
                  })();
                })();
              })()))([ack_p]);
        })();
      })();
    })();
  })();
var Ackermann$ack = (n, m) => Ackermann$ack1(n)(m);
var AvlTree$rotation = (left, right, max_diff) => (() => {
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
var AvlTree$max = (i, j) => (() => {
  const _anon4 = (i < j) ? [0] : (i === j) ? [1] : [2];
  return (_anon4[0] === 0) ? j : i;
})();
var AvlTree$branch = (sz, item, left, right) => (() => {
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
var AvlTree$branch_s = (item, left, right) => AvlTree$branch(1 + (((left[0] === 0) ? 0 : left[1]) + ((right[0] === 0) ? 0 : right[1])), item, left, right);
var AvlTree$balance = t => (t[0] === 0) ? [0] : (() => {
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
var AvlTree$add_item = (ord, tree, item) => (() => {
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
var AvlTree$contains = (ord, tree, item) => (() => {
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
var AvlTree$min = tree => (() => {
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
var AvlTree$remove_item = (ord, tree, item) => (() => {
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
var AvlTree$fold_left_Tree = (t, left_v, fn) => (t[0] === 0) ? left_v : (() => {
    const key = t[3];
    return (() => {
      const left = t[4];
      return (() => {
        const right = t[5];
        return AvlTree$fold_left_Tree(right, fn(AvlTree$fold_left_Tree(left, left_v, fn), key), fn);
      })();
    })();
  })();
var AvlTree$fold_right_Tree = (t, right_v, fn) => (t[0] === 0) ? right_v : (() => {
    const key = t[3];
    return (() => {
      const left = t[4];
      return (() => {
        const right = t[5];
        return AvlTree$fold_right_Tree(left, fn(key, AvlTree$fold_right_Tree(right, right_v, fn)), fn);
      })();
    })();
  })();
var AvlTree$_module = ord => ((_a0, _a1, _a2, _a3, _a4, _a5, _a6, _a7) => [_a0,
  _a1,
  _a2,
  _a3,
  _a4,
  _a5,
  _a6,
  _a7])(ord, [0], (_slots => (t, a) => AvlTree$add_item(_slots[0], t, a))([ord]), (_slots => a_1 => AvlTree$add_item(_slots[0], [0], a_1))([ord]), (_slots => (t_1, a_2) => AvlTree$contains(_slots[0], t_1, a_2))([ord]), (_slots => (t_2, a_3) => AvlTree$remove_item(_slots[0], t_2, a_3))([ord]), AvlTree$fold_left_Tree, AvlTree$fold_right_Tree);
var AvlTree$_a = AvlTree$_module((_a0 => [_a0])((_a0, _a1) => (_a0 < _a1) ? [0] : (_a0 === _a1) ? [1] : [2]));
var AvlTree$contains_test = ((_a0, _a1) => [1,
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
_tests["AvlTree::contains_test"] = AvlTree$contains_test;
var AvlTree$add_increases_size = (t, i, msg) => ((_a0, _a1) => [0, _a0, _a1])(((() => {
    const _anon30 = AvlTree$_a[2](t, i);
    return (_anon30[0] === 0) ? 0 : _anon30[1];
  })() - ((t[0] === 0) ? 0 : t[1]) === 1) ? [1] : [0], msg);
var AvlTree$rem_decreases_size = (t, i, msg) => ((_a0, _a1) => [0,
  _a0,
  _a1])((((t[0] === 0) ? 0 : t[1]) - (() => {
    const _anon31 = AvlTree$_a[5](t, i);
    return (_anon31[0] === 0) ? 0 : _anon31[1];
  })() === 1) ? [1] : [0], msg);
var AvlTree$size_tests = ((_a0, _a1) => [1,
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
var AvlTree$log2 = i => _int_loop(i, 0, (n, cnt) => ((_a0, _a1) => [_a0,
    _a1])(2 ? Math.trunc(n / 2) : 0, 1 + cnt));
var AvlTree$height_tests = ((_a0, _a1) => [1,
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
var AvlTree$fold_left_tests = ((_a0, _a1) => [1,
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
var AvlTree$fold_right_tests = ((_a0, _a1) => [1,
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
var AvlTree$tests = ((_a0, _a1) => [1,
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
_tests["AvlTree::tests"] = AvlTree$tests;
var Bo_Test$test = ((_a0, _a1) => [0, _a0, _a1])([1], _js_to_bosatsu_string("trivial"));
_tests["Bo/Test::test"] = Bo_Test$test;
var Bosatsu_Bool$not = b => (b[0] === 1) ? [0] : [1];
var Bosatsu_Bool$test = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("not tests"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0,
      _a0,
      _a1])([1], _js_to_bosatsu_string("not(True)")), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1) => [0,
        _a0,
        _a1])([1], _js_to_bosatsu_string("not(False)")), [0])));
_tests["Bosatsu/Bool::test"] = Bosatsu_Bool$test;
var Bosatsu_Char$str_to_char_tests = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("string_to_Char"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
        const _anon0 = (_a0 => [1, _a0])([1, "s", [0]]);
        return (() => {
          let _anon1;
          return (_anon0[0] === 1 && (() => {
            _anon1 = _anon0[1];
            return true;
          })() && (_bosatsu_to_js_string(_anon1) === "s")) ? [1] : [0];
        })();
      })(), _js_to_bosatsu_string("s")), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1) => [0,
        _a0,
        _a1])([1], _js_to_bosatsu_string("empty")), ((_a0, _a1) => [1,
        _a0,
        _a1])(((_a0, _a1) => [0,
          _a0,
          _a1])([1], _js_to_bosatsu_string("foo")), ((_a0, _a1) => [1,
          _a0,
          _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
              const _anon2 = (_a0 => [1, _a0])([1, "\ud83d\udc4b", [0]]);
              return (() => {
                let _anon3;
                return (_anon2[0] === 1 && (() => {
                  _anon3 = _anon2[1];
                  return true;
                })() && (_bosatsu_to_js_string(_anon3) === "\ud83d\udc4b")) ? [1] : [0];
              })();
            })(), _js_to_bosatsu_string("foo")), [0])))));
var Bosatsu_Char$length_String = s => (() => {
  const loop = (s_1, acc) => (() => {
    let _anon5;
    return (() => {
      let _anon6;
      return (() => {
        let _anon8;
        return (() => {
          let _anon10;
          return (() => {
            (() => {
              _anon8 = s_1;
              return true;
            })();
            return (() => {
              (() => {
                _anon10 = acc;
                return true;
              })();
              return (() => {
                (() => {
                  _anon5 = [1];
                  return true;
                })();
                return (() => {
                  while (_anon5[0] === 1) {
                    (_bosatsu_to_js_string(_anon8) === "") ? (() => {
                        (() => {
                          _anon5 = [0];
                          return true;
                        })();
                        return (() => {
                          (() => {
                            _anon6 = _anon10;
                            return true;
                          })();
                          return [];
                        })();
                      })() : (() => {
                        let _anon4;
                        return (() => {
                          (() => {
                            const _str = _bosatsu_to_js_string(_anon8);
                            return true && (() => {
                              _anon4 = _js_to_bosatsu_string(_str.substring(0 + 1));
                              return true;
                            })();
                          })();
                          return (() => {
                            const tail = _anon4;
                            return (() => {
                              const _anon7 = tail;
                              return (() => {
                                const _anon9 = 1 + _anon10;
                                return (() => {
                                  (() => {
                                    _anon8 = _anon7;
                                    return true;
                                  })();
                                  return (() => {
                                    (() => {
                                      _anon10 = _anon9;
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
                  return _anon6;
                })();
              })();
            })();
          })();
        })();
      })();
    })();
  })();
  return loop(s, 0);
})();
var Bosatsu_Char$len_test = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("len tests"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
        const _anon11 = Bosatsu_Char$length_String(_js_to_bosatsu_string(""));
        return (_anon11 === 0) ? [1] : [0];
      })(), _js_to_bosatsu_string("empty")), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
          const _anon12 = Bosatsu_Char$length_String(_js_to_bosatsu_string("x"));
          return (_anon12 === 1) ? [1] : [0];
        })(), _js_to_bosatsu_string("x")), ((_a0, _a1) => [1,
        _a0,
        _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
            const _anon13 = Bosatsu_Char$length_String(_js_to_bosatsu_string("hello"));
            return (_anon13 === 5) ? [1] : [0];
          })(), _js_to_bosatsu_string("hello")), ((_a0, _a1) => [1,
          _a0,
          _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
              const _anon14 = Bosatsu_Char$length_String(_js_to_bosatsu_string("\ud83d\udc4b"));
              return (_anon14 === 1) ? [1] : [0];
            })(), _js_to_bosatsu_string("\ud83d\udc4b")), [0])))));
_tests["Bosatsu/Char::len_test"] = Bosatsu_Char$len_test;
var Bosatsu_Char$last_tests = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("last_String"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0,
      _a0,
      _a1])([1], _js_to_bosatsu_string("empty")), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
          const _anon15 = (_a0 => [1, _a0])([1, "x", [0]]);
          return (() => {
            let _anon16;
            return (_anon15[0] === 1 && (() => {
              _anon16 = _anon15[1];
              return true;
            })() && (_bosatsu_to_js_string(_anon16) === "x")) ? [1] : [0];
          })();
        })(), _js_to_bosatsu_string("x")), ((_a0, _a1) => [1,
        _a0,
        _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
            const _anon17 = (_a0 => [1, _a0])([1, "4", [0]]);
            return (() => {
              let _anon18;
              return (_anon17[0] === 1 && (() => {
                _anon18 = _anon17[1];
                return true;
              })() && (_bosatsu_to_js_string(_anon18) === "4")) ? [1] : [0];
            })();
          })(), _js_to_bosatsu_string("1234")), ((_a0, _a1) => [1,
          _a0,
          _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
              const _anon19 = (_a0 => [1, _a0])([1, "\ud83d\udc4b", [0]]);
              return (() => {
                let _anon20;
                return (_anon19[0] === 1 && (() => {
                  _anon20 = _anon19[1];
                  return true;
                })() && (_bosatsu_to_js_string(_anon20) === "\ud83d\udc4b")) ? [1] : [0];
              })();
            })(), _js_to_bosatsu_string("\ud83d\udc4b")), [0])))));
var Bosatsu_Char$partition_tests = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("partition tests"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
        const _anon21 = _partition_String(_js_to_bosatsu_string("foo"), _js_to_bosatsu_string("f"));
        return (() => {
          let _anon22;
          return (() => {
            let _anon24;
            return (() => {
              let _anon23;
              return (_anon21[0] === 1 && (() => {
                _anon22 = _anon21[1];
                return true;
              })() && ((() => {
                _anon24 = _anon22[1];
                return true;
              })() && ((() => {
                _anon23 = _anon22[0];
                return true;
              })() && (_bosatsu_to_js_string(_anon23) === "" && (_bosatsu_to_js_string(_anon24) === "oo"))))) ? [1] : [0];
            })();
          })();
        })();
      })(), _js_to_bosatsu_string("foo partition_String f")), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
          const _anon25 = _partition_String(_js_to_bosatsu_string("foo"), _js_to_bosatsu_string(""));
          return (_anon25[0] === 0) ? [1] : [0];
        })(), _js_to_bosatsu_string("foo partition_String \"\"")), ((_a0, _a1) => [1,
        _a0,
        _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
            const _anon26 = _partition_String(_js_to_bosatsu_string("foo"), _js_to_bosatsu_string("x"));
            return (_anon26[0] === 0) ? [1] : [0];
          })(), _js_to_bosatsu_string("foo partition_String x")), ((_a0, _a1) => [1,
          _a0,
          _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
              const _anon27 = _rpartition_String(_js_to_bosatsu_string("foo"), _js_to_bosatsu_string("o"));
              return (() => {
                let _anon28;
                return (() => {
                  let _anon30;
                  return (() => {
                    let _anon29;
                    return (_anon27[0] === 1 && (() => {
                      _anon28 = _anon27[1];
                      return true;
                    })() && ((() => {
                      _anon30 = _anon28[1];
                      return true;
                    })() && ((() => {
                      _anon29 = _anon28[0];
                      return true;
                    })() && (_bosatsu_to_js_string(_anon29) === "fo" && (_bosatsu_to_js_string(_anon30) === ""))))) ? [1] : [0];
                  })();
                })();
              })();
            })(), _js_to_bosatsu_string("foo rpartition_String o")), ((_a0, _a1) => [1,
            _a0,
            _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                const _anon31 = _rpartition_String(_js_to_bosatsu_string("foo"), _js_to_bosatsu_string(""));
                return (_anon31[0] === 0) ? [1] : [0];
              })(), _js_to_bosatsu_string("foo rpartition_String \"\"")), ((_a0, _a1) => [1,
              _a0,
              _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                  const _anon32 = _rpartition_String(_js_to_bosatsu_string("foo"), _js_to_bosatsu_string("x"));
                  return (_anon32[0] === 0) ? [1] : [0];
                })(), _js_to_bosatsu_string("foo rpartition_String x")), [0])))))));
var Bosatsu_Char$match_tests = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("match tests"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0,
      _a0,
      _a1])([1], _js_to_bosatsu_string("test matching 1")), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1) => [0,
        _a0,
        _a1])([1], _js_to_bosatsu_string("test matching 2")), ((_a0, _a1) => [1,
        _a0,
        _a1])(((_a0, _a1) => [0,
          _a0,
          _a1])([1], _js_to_bosatsu_string("test matching 2.5")), ((_a0, _a1) => [1,
          _a0,
          _a1])(((_a0, _a1) => [0,
            _a0,
            _a1])([1], _js_to_bosatsu_string("test matching 3")), ((_a0, _a1) => [1,
            _a0,
            _a1])(((_a0, _a1) => [0,
              _a0,
              _a1])([1], _js_to_bosatsu_string("test matching 4")), ((_a0, _a1) => [1,
              _a0,
              _a1])(((_a0, _a1) => [0,
                _a0,
                _a1])([1], _js_to_bosatsu_string("test matching 5")), ((_a0, _a1) => [1,
                _a0,
                _a1])(((_a0, _a1) => [0,
                  _a0,
                  _a1])([1], _js_to_bosatsu_string("test matching 6")), [0]))))))));
var Bosatsu_Char$glob_match_tests = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("glob_match_suites"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0,
      _a0,
      _a1])([1], _js_to_bosatsu_string("starts_with_foo(foobar)")), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1) => [0,
        _a0,
        _a1])([1], _js_to_bosatsu_string("starts_with_foo(foobar)")), ((_a0, _a1) => [1,
        _a0,
        _a1])(((_a0, _a1) => [0,
          _a0,
          _a1])([1], _js_to_bosatsu_string("ends_with_foo(foobar)")), ((_a0, _a1) => [1,
          _a0,
          _a1])(((_a0, _a1) => [0,
            _a0,
            _a1])([1], _js_to_bosatsu_string("ends_with_foo(foobar)")), ((_a0, _a1) => [1,
            _a0,
            _a1])(((_a0, _a1) => [0,
              _a0,
              _a1])([1], _js_to_bosatsu_string("contains_foo(foobar)")), ((_a0, _a1) => [1,
              _a0,
              _a1])(((_a0, _a1) => [0,
                _a0,
                _a1])([1], _js_to_bosatsu_string("contains_foo(barbar)")), ((_a0, _a1) => [1,
                _a0,
                _a1])(((_a0, _a1) => [0,
                  _a0,
                  _a1])([1], _js_to_bosatsu_string("there is foo and bar")), ((_a0, _a1) => [1,
                  _a0,
                  _a1])(((_a0, _a1) => [0,
                    _a0,
                    _a1])([1], _js_to_bosatsu_string("there is foobar")), ((_a0, _a1) => [1,
                    _a0,
                    _a1])(((_a0, _a1) => [0,
                      _a0,
                      _a1])([1], _js_to_bosatsu_string("there is foo but not the other")), [0]))))))))));
var Bosatsu_Char$tests = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("Char tests"), ((_a0, _a1) => [1,
    _a0,
    _a1])(Bosatsu_Char$str_to_char_tests, ((_a0, _a1) => [1,
      _a0,
      _a1])(Bosatsu_Char$len_test, ((_a0, _a1) => [1,
        _a0,
        _a1])(Bosatsu_Char$last_tests, ((_a0, _a1) => [1,
          _a0,
          _a1])(Bosatsu_Char$match_tests, ((_a0, _a1) => [1,
            _a0,
            _a1])(Bosatsu_Char$glob_match_tests, ((_a0, _a1) => [1,
              _a0,
              _a1])(Bosatsu_Char$partition_tests, [0])))))));
_tests["Bosatsu/Char::tests"] = Bosatsu_Char$tests;
var Bosatsu_Nat$cmp_Nat = (a, b) => (() => {
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
var Bosatsu_Nat$times2 = n => (() => {
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
var Bosatsu_Nat$add = (n1, n2) => (() => {
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
var Bosatsu_Nat$sub_Nat = (n1, n2) => (() => {
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
var Bosatsu_Nat$mult = (n1, n2) => (() => {
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
var Bosatsu_Nat$is_even = n => (() => {
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
var Bosatsu_Nat$div2 = n => (() => {
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
var Bosatsu_Nat$divmod = (numerator, divisor) => (() => {
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
var Bosatsu_Nat$one = (n => n + 1)(0);
var Bosatsu_Nat$exp = (base, power) => (base === 0) ? (power === 0) ? Bosatsu_Nat$one : 0 : (() => {
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
var Bosatsu_Nat$to_Int = n => (() => {
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
var Bosatsu_Nat$to_Nat = i => _int_loop(i, 0, (i_1, nat) => ((_a0, _a1) => [_a0,
    _a1])((-1) + i_1, (n => n + 1)(nat)));
var Bosatsu_Nat$n1 = (n => n + 1)(0);
var Bosatsu_Nat$n2 = (n => n + 1)(Bosatsu_Nat$n1);
var Bosatsu_Nat$n3 = (n => n + 1)(Bosatsu_Nat$n2);
var Bosatsu_Nat$n4 = (n => n + 1)(Bosatsu_Nat$n3);
var Bosatsu_Nat$n5 = (n => n + 1)(Bosatsu_Nat$n4);
var Bosatsu_Nat$addLaw = (n1, n2, label) => ((_a0, _a1) => [0,
  _a0,
  _a1])((Bosatsu_Nat$to_Int(Bosatsu_Nat$add(n1, n2)) === (Bosatsu_Nat$to_Int(n1) + Bosatsu_Nat$to_Int(n2))) ? [1] : [0], label);
var Bosatsu_Nat$multLaw = (n1, n2, label) => ((_a0, _a1) => [0,
  _a0,
  _a1])((Bosatsu_Nat$to_Int(Bosatsu_Nat$mult(n1, n2)) === (Bosatsu_Nat$to_Int(n1) * Bosatsu_Nat$to_Int(n2))) ? [1] : [0], label);
var Bosatsu_Nat$from_to_law = (i, message) => ((_a0, _a1) => [0,
  _a0,
  _a1])((Bosatsu_Nat$to_Int(_int_loop(i, 0, (i1, nat) => ((_a0, _a1) => [_a0,
        _a1])((-1) + i1, (n => n + 1)(nat)))) === i) ? [1] : [0], message);
var Bosatsu_Nat$from_to_suite = ((_a0, _a1) => [1,
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
var Bosatsu_Nat$tests = ((_a0, _a1) => [1,
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
_tests["Bosatsu/Nat::tests"] = Bosatsu_Nat$tests;
var Bosatsu_Nothing$impossible = n => n[0];
var Bosatsu_Option$eq_Option = eq => (_slots => (left, right) => (() => {
  const _anon0 = ((_a0, _a1) => [_a0, _a1])(left, right);
  return (() => {
    let _anon2;
    return (() => {
      let _anon1;
      return ((() => {
        _anon2 = _anon0[1];
        return true;
      })() && ((() => {
        _anon1 = _anon0[0];
        return true;
      })() && (_anon1[0] === 1 && (_anon2[0] === 1)))) ? (() => {
          const a = _anon1[1];
          return (() => {
            const b = _anon2[1];
            return _slots[0](a, b);
          })();
        })() : (() => {
          let _anon4;
          return (() => {
            let _anon3;
            return ((() => {
              _anon4 = _anon0[1];
              return true;
            })() && ((() => {
              _anon3 = _anon0[0];
              return true;
            })() && (_anon3[0] === 0 && (_anon4[0] === 0)))) ? [1] : [0];
          })();
        })();
    })();
  })();
})())([eq]);
var Bosatsu_Prog$map = (prog, fn) => Bosatsu_Prog$flat_map(prog, (_slots => res => Bosatsu_Prog$pure(_slots[0](res)))([fn]));
var Bosatsu_Prog$map_err = (prog, fn) => Bosatsu_Prog$recover(prog, (_slots => res => Bosatsu_Prog$raise_error(_slots[0](res)))([fn]));
var Bosatsu_Prog$with_env = (p, env) => Bosatsu_Prog$remap_env(p, (_slots => a => _slots[0])([env]));
var Bosatsu_Prog$ignore_env = p => Bosatsu_Prog$remap_env(p, a => []);
var Bosatsu_Prog$_await = p => (_slots => fn => Bosatsu_Prog$flat_map(_slots[0], fn))([p]);
var Bosatsu_Prog$recursive = fn => (_slots => a => Bosatsu_Prog$apply_fix(a, _slots[0]))([fn]);
var Bosatsu_Prog$unit = Bosatsu_Prog$pure([]);
var Bosatsu_Prog$count_down = a => Bosatsu_Prog$apply_fix(a, loop => (_slots => i => (() => {
    const _anon0 = (i < 0) ? [0] : (i === 0) ? [1] : [2];
    return (_anon0[0] === 1) ? Bosatsu_Prog$println(_js_to_bosatsu_string("\ndone")) : (_anon0[0] === 0) ? Bosatsu_Prog$println(_js_to_bosatsu_string("\ndone")) : Bosatsu_Prog$flat_map(Bosatsu_Prog$print(_concat_String(((_a0, _a1) => [1,
                _a0,
                _a1])(_int_to_String(i), ((_a0, _a1) => [1,
                  _a0,
                  _a1])(_js_to_bosatsu_string(", "), [0])))), (_slots => a_1 => _slots[0]((-1) + _slots[1]))([_slots[0],
              i]));
  })())([loop]));
var Bosatsu_Prog$to_run = (_a0 => [_a0])(Bosatsu_Prog$flat_map(Bosatsu_Prog$read_env, args => Bosatsu_Prog$ignore_env((() => {
        const arg_str = foldl_List(args, _js_to_bosatsu_string(""), (s, item) => (_bosatsu_to_js_string(s) === "") ? item : _concat_String(((_a0, _a1) => [1,
                _a0,
                _a1])(s, ((_a0, _a1) => [1,
                  _a0,
                  _a1])(_js_to_bosatsu_string(", "), ((_a0, _a1) => [1,
                    _a0,
                    _a1])(item, [0])))));
        return Bosatsu_Prog$flat_map(Bosatsu_Prog$recover(Bosatsu_Prog$read_stdin_utf8_bytes(1000), (() => {
              const prog = Bosatsu_Prog$println(_js_to_bosatsu_string("<failed to read stdin"));
              return (_slots => a => Bosatsu_Prog$flat_map(_slots[0], a_1 => Bosatsu_Prog$pure(_js_to_bosatsu_string(""))))([prog]);
            })()), (_slots => stdin => Bosatsu_Prog$flat_map(Bosatsu_Prog$println(_concat_String(((_a0, _a1) => [1,
                  _a0,
                  _a1])(_js_to_bosatsu_string("found stdin: "), ((_a0, _a1) => [1,
                    _a0,
                    _a1])(stdin, [0])))), (_slots => a_2 => Bosatsu_Prog$flat_map(Bosatsu_Prog$count_down(10), (_slots => a_3 => Bosatsu_Prog$flat_map(Bosatsu_Prog$println(_concat_String(((_a0, _a1) => [1,
                      _a0,
                      _a1])(_js_to_bosatsu_string("args = "), ((_a0, _a1) => [1,
                        _a0,
                        _a1])(_slots[0], [0])))), a_4 => Bosatsu_Prog$pure(0)))([_slots[0]])))([_slots[0]])))([arg_str]));
      })())));
var BuildLibrary$refl = (_a0 => [_a0])(x => x);
var BuildLibrary$map_Build = (b, fn) => (_a0 => [2, _a0])(((_a0, _a1) => [_a0, _a1])(b, fn));
var BuildLibrary$map2_Build = (ba, bb, fn) => (_a0 => [3, _a0])(((_a0, _a1, _a2) => [_a0,
    _a1,
    _a2])(ba, bb, fn));
var BuildLibrary$file = s => ((_a0, _a1) => [0, _a0, _a1])(s, BuildLibrary$refl);
var BuildLibrary$empty = (_a0 => [1, _a0])([0]);
var BuildLibrary$build_all = items => (items[0] === 0) ? BuildLibrary$empty : (() => {
    const h = items[1];
    return (() => {
      const t = items[2];
      return (_a0 => [3, _a0])(((_a0, _a1, _a2) => [_a0,
          _a1,
          _a2])(h, BuildLibrary$build_all(t), (_a0, _a1) => [1, _a0, _a1]));
    })();
  })();
var BuildLibrary$files = fs => BuildLibrary$build_all(Bosatsu_Predef$map_List(fs, f => ((_a0, _a1) => [0,
      _a0,
      _a1])(f, BuildLibrary$refl)));
var BuildLibrary$library = (sources, deps) => ((_a0, _a1, _a2) => [4,
  _a0,
  _a1,
  _a2])(sources, deps, BuildLibrary$refl);
var BuildLibrary$build = args => (() => {
  const srcs = args[0];
  return (() => {
    const deps = args[1];
    return ((_a0, _a1, _a2) => [4,
      _a0,
      _a1,
      _a2])(srcs, deps, BuildLibrary$refl);
  })();
})();
var DictTools$merge = (left, right, fn) => foldl_List(Bosatsu_Predef$items(right), left, (_slots => (d, a) => (() => {
    const k = a[0];
    return (() => {
      const v = a[1];
      return (() => {
        const _anon0 = Bosatsu_Predef$get_key(d, k);
        return (_anon0[0] === 0) ? Bosatsu_Predef$add_key(d, k, v) : (() => {
            const v0 = _anon0[1];
            return Bosatsu_Predef$add_key(d, k, _slots[0](v0, v));
          })();
      })();
    })();
  })())([fn]));
var Euler_One$keep = i => (() => {
  const _anon0 = ((3 ? i % 3 : i) === 0) ? [1] : [0];
  return (_anon0[0] === 1) ? [1] : ((5 ? i % 5 : i) === 0) ? [1] : [0];
})();
var Euler_One$computed = foldl_List(flat_map_List(range(1000), i => (() => {
      const _anon1 = Euler_One$keep(i);
      return (_anon1[0] === 1) ? ((_a0, _a1) => [1, _a0, _a1])(i, [0]) : [0];
    })()), 0, (_a0, _a1) => _a0 + _a1);
var Euler_One$test = ((_a0, _a1) => [0,
  _a0,
  _a1])((Euler_One$computed === 233168) ? [1] : [0], _concat_String(((_a0, _a1) => [1,
      _a0,
      _a1])(_js_to_bosatsu_string("expected 233168 got "), ((_a0, _a1) => [1,
        _a0,
        _a1])(_int_to_String(Euler_One$computed), [0]))));
_tests["Euler/One::test"] = Euler_One$test;
var Euler_P6$sum = (fn, n) => _int_loop(n, 0, (_slots => (i, r) => (() => {
    const i_1 = (-1) + i;
    return ((_a0, _a1) => [_a0, _a1])(i_1, r + _slots[0](i_1));
  })())([fn]));
var Euler_P6$diff = n => Euler_P6$sum(x => (() => { const x1 = 1 + x; return x * (x1 * x1); })(), n);
var Euler_P6$test0 = ((_a0, _a1) => [0,
  _a0,
  _a1])((Euler_P6$diff(10) === 2640) ? [1] : [0], _js_to_bosatsu_string("matched problem"));
var Euler_P6$test1 = ((_a0, _a1) => [0,
  _a0,
  _a1])((Euler_P6$diff(100) === 25164150) ? [1] : [0], _js_to_bosatsu_string("matched problem"));
var Euler_P6$tests = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("two examples"), ((_a0, _a1) => [1,
    _a0,
    _a1])(Euler_P6$test0, ((_a0, _a1) => [1, _a0, _a1])(Euler_P6$test1, [0])));
_tests["Euler/P6::tests"] = Euler_P6$tests;
var Euler_Three$smallest_factor = n => (n === 1) ? 1 : _int_loop(n, (-1), (_slots => (i, a) => (() => {
      const trial = 2 + (_slots[0] - i);
      return (() => {
        const _anon0 = ((trial ? _slots[0] % trial : _slots[0]) === 0) ? [1] : [0];
        return (_anon0[0] === 1) ? ((_a0, _a1) => [_a0,
            _a1])(0, trial) : ((_a0, _a1) => [_a0, _a1])((-1) + i, (-1));
      })();
    })())([n]));
var Euler_Three$all_factors = n => _int_loop(n, [0], (i, facs) => (() => {
    const next_factor = Euler_Three$smallest_factor(i);
    return (() => {
      const _anon1 = next_factor ? Math.trunc(i / next_factor) : 0;
      return (_anon1 === 1) ? ((_a0, _a1) => [_a0, _a1])(0, ((_a0, _a1) => [1,
            _a0,
            _a1])(next_factor, facs)) : (() => {
          const smaller = _anon1;
          return ((_a0, _a1) => [_a0, _a1])(smaller, ((_a0, _a1) => [1,
              _a0,
              _a1])(next_factor, facs));
        })();
    })();
  })());
var Euler_Three$test = ((_a0, _a1) => [0, _a0, _a1])(((() => {
    const _anon2 = Euler_Three$all_factors(600851475143);
    return (_anon2[0] === 0) ? 600851475143 : _anon2[1];
  })() === 6857) ? [1] : [0], _js_to_bosatsu_string("trial"));
_tests["Euler/Three::test"] = Euler_Three$test;
var IntTest$assert_eq = (got, expect, message) => ((_a0, _a1) => [0,
  _a0,
  _a1])((got === expect) ? [1] : [0], _concat_String(((_a0, _a1) => [1,
      _a0,
      _a1])(message, ((_a0, _a1) => [1,
        _a0,
        _a1])(_js_to_bosatsu_string(": got = "), ((_a0, _a1) => [1,
          _a0,
          _a1])(_int_to_String(got), ((_a0, _a1) => [1,
            _a0,
            _a1])(_js_to_bosatsu_string(", expected = "), ((_a0, _a1) => [1,
              _a0,
              _a1])(_int_to_String(expect), [0])))))));
var IntTest$diff1 = 36893488147419103232 - 1;
var IntTest$and_test_cases = ((_a0, _a1) => [1, _a0, _a1])(((_a0, _a1, _a2) => [_a0,
    _a1,
    _a2])(3 & 1, 1, _js_to_bosatsu_string("3 & 1")), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1, _a2) => [_a0,
      _a1,
      _a2])(0 & 1, 0, _js_to_bosatsu_string("0 & 1")), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1, _a2) => [_a0,
        _a1,
        _a2])(1 & 1, 1, _js_to_bosatsu_string("1 & 1")), ((_a0, _a1) => [1,
        _a0,
        _a1])(((_a0, _a1, _a2) => [_a0,
          _a1,
          _a2])(2 & 1, 0, _js_to_bosatsu_string("2 & 1")), ((_a0, _a1) => [1,
          _a0,
          _a1])(((_a0, _a1, _a2) => [_a0,
            _a1,
            _a2])(3 & 1, 1, _js_to_bosatsu_string("3 & 1")), ((_a0, _a1) => [1,
            _a0,
            _a1])(((_a0, _a1, _a2) => [_a0,
              _a1,
              _a2])(4 & 1, 0, _js_to_bosatsu_string("4 & 1")), ((_a0, _a1) => [1,
              _a0,
              _a1])(((_a0, _a1, _a2) => [_a0,
                _a1,
                _a2])((-3) & 1, 1, _js_to_bosatsu_string("-3 & 1")), ((_a0, _a1) => [1,
                _a0,
                _a1])(((_a0, _a1, _a2) => [_a0,
                  _a1,
                  _a2])((-5) & (-2), (-6), _js_to_bosatsu_string("-5 & -2")), ((_a0, _a1) => [1,
                  _a0,
                  _a1])(((_a0, _a1, _a2) => [_a0,
                    _a1,
                    _a2])(73786976294838206464 & 36893488147419103232, 0, _js_to_bosatsu_string("(73786976294838206464) & (36893488147419103232)")), ((_a0, _a1) => [1,
                    _a0,
                    _a1])(((_a0, _a1, _a2) => [_a0,
                      _a1,
                      _a2])(IntTest$diff1 & IntTest$diff1, IntTest$diff1, _js_to_bosatsu_string("diff1")), ((_a0, _a1) => [1,
                      _a0,
                      _a1])(((_a0, _a1, _a2) => [_a0,
                        _a1,
                        _a2])(36893488147419103232 - 1 & (36893488147419103232 - 1), 36893488147419103232 - 1, _js_to_bosatsu_string("(36893488147419103232 - 1) & (36893488147419103232 - 1)")), ((_a0, _a1) => [1,
                        _a0,
                        _a1])(((_a0, _a1, _a2) => [_a0,
                          _a1,
                          _a2])(36893488147419103231 & 36893488147419103231, 36893488147419103231, _js_to_bosatsu_string("(36893488147419103231) & (36893488147419103231)")), ((_a0, _a1) => [1,
                          _a0,
                          _a1])(((_a0, _a1, _a2) => [_a0,
                            _a1,
                            _a2])((-36893488147419103232) & (-36893488147419103232), (-36893488147419103232), _js_to_bosatsu_string("-36893488147419103232 & -36893488147419103232")), ((_a0, _a1) => [1,
                            _a0,
                            _a1])(((_a0, _a1, _a2) => [_a0,
                              _a1,
                              _a2])((-73786976294838206464) & (36893488147419103232 - 1), 0, _js_to_bosatsu_string("-73786976294838206464 & (36893488147419103232 - 1)")), ((_a0, _a1) => [1,
                              _a0,
                              _a1])(((_a0, _a1, _a2) => [_a0,
                                _a1,
                                _a2])(3 & (-1), 3, _js_to_bosatsu_string("3 & -1")), ((_a0, _a1) => [1,
                                _a0,
                                _a1])(((_a0, _a1, _a2) => [_a0,
                                  _a1,
                                  _a2])((-3) & 2, 0, _js_to_bosatsu_string("-3 & 2")), ((_a0, _a1) => [1,
                                  _a0,
                                  _a1])(((_a0, _a1, _a2) => [_a0,
                                    _a1,
                                    _a2])(36893488147419103232 - 1 & (-1), 36893488147419103232 - 1, _js_to_bosatsu_string("(36893488147419103232 - 1) & -1")), ((_a0, _a1) => [1,
                                    _a0,
                                    _a1])(((_a0, _a1, _a2) => [_a0,
                                      _a1,
                                      _a2])((-73786976294838206464) & (36893488147419103232 - 1), 0, _js_to_bosatsu_string("-73786976294838206464 & (36893488147419103232 - 1)")), [0]))))))))))))))))));
var IntTest$and_tests = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("and tests"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
        const _anon0 = 2 & 1;
        return (_anon0 === 0) ? [1] : [0];
      })(), _js_to_bosatsu_string("2 & 1 matches 0")), Bosatsu_Predef$map_List(IntTest$and_test_cases, a => (() => {
        const got = a[0];
        return (() => {
          const ex = a[1];
          return (() => {
            const m = a[2];
            return IntTest$assert_eq(got, ex, m);
          })();
        })();
      })())));
var IntTest$or_test_cases = ((_a0, _a1) => [1, _a0, _a1])(((_a0, _a1, _a2) => [_a0,
    _a1,
    _a2])(3 | 1, 3, _js_to_bosatsu_string("3 | 1")), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1, _a2) => [_a0,
      _a1,
      _a2])((-3) | 1, (-3), _js_to_bosatsu_string("-3 | 1")), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1, _a2) => [_a0,
        _a1,
        _a2])((-5) | (-2), (-1), _js_to_bosatsu_string("-5 | -2")), ((_a0, _a1) => [1,
        _a0,
        _a1])(((_a0, _a1, _a2) => [_a0,
          _a1,
          _a2])(73786976294838206464 | 36893488147419103232, 110680464442257309696, _js_to_bosatsu_string("(73786976294838206464) | (36893488147419103232)")), ((_a0, _a1) => [1,
          _a0,
          _a1])(((_a0, _a1, _a2) => [_a0,
            _a1,
            _a2])(IntTest$diff1 | IntTest$diff1, IntTest$diff1, _js_to_bosatsu_string("diff1")), ((_a0, _a1) => [1,
            _a0,
            _a1])(((_a0, _a1, _a2) => [_a0,
              _a1,
              _a2])(36893488147419103232 - 1 | (36893488147419103232 - 1), 36893488147419103232 - 1, _js_to_bosatsu_string("(36893488147419103232 - 1) | (36893488147419103232 - 1)")), ((_a0, _a1) => [1,
              _a0,
              _a1])(((_a0, _a1, _a2) => [_a0,
                _a1,
                _a2])(36893488147419103231 | 36893488147419103231, 36893488147419103231, _js_to_bosatsu_string("(36893488147419103231) | (36893488147419103231)")), ((_a0, _a1) => [1,
                _a0,
                _a1])(((_a0, _a1, _a2) => [_a0,
                  _a1,
                  _a2])((-36893488147419103232) | (-36893488147419103232), (-36893488147419103232), _js_to_bosatsu_string("-36893488147419103232 | -36893488147419103232")), ((_a0, _a1) => [1,
                  _a0,
                  _a1])(((_a0, _a1, _a2) => [_a0,
                    _a1,
                    _a2])((-73786976294838206464) | (36893488147419103232 - 1), (-36893488147419103233), _js_to_bosatsu_string("-73786976294838206464 | (36893488147419103232 - 1)")), ((_a0, _a1) => [1,
                    _a0,
                    _a1])(((_a0, _a1, _a2) => [_a0,
                      _a1,
                      _a2])(3 | (-1), (-1), _js_to_bosatsu_string("3 | -1")), ((_a0, _a1) => [1,
                      _a0,
                      _a1])(((_a0, _a1, _a2) => [_a0,
                        _a1,
                        _a2])((-3) | 2, (-1), _js_to_bosatsu_string("-3 | 2")), ((_a0, _a1) => [1,
                        _a0,
                        _a1])(((_a0, _a1, _a2) => [_a0,
                          _a1,
                          _a2])(36893488147419103232 - 1 | (-1), (-1), _js_to_bosatsu_string("(36893488147419103232 - 1) | -1")), ((_a0, _a1) => [1,
                          _a0,
                          _a1])(((_a0, _a1, _a2) => [_a0,
                            _a1,
                            _a2])((-73786976294838206464) | (36893488147419103232 - 1), (-36893488147419103233), _js_to_bosatsu_string("-73786976294838206464 | (36893488147419103232 - 1)")), [0])))))))))))));
var IntTest$or_tests = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("or tests"), Bosatsu_Predef$map_List(IntTest$or_test_cases, a => (() => {
      const got = a[0];
      return (() => {
        const ex = a[1];
        return (() => {
          const m = a[2];
          return IntTest$assert_eq(got, ex, m);
        })();
      })();
    })()));
var IntTest$xor_test_cases = ((_a0, _a1) => [1, _a0, _a1])(((_a0, _a1, _a2) => [_a0,
    _a1,
    _a2])(3 ^ 1, 2, _js_to_bosatsu_string("3 ^ 1")), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1, _a2) => [_a0,
      _a1,
      _a2])((-3) ^ 1, (-4), _js_to_bosatsu_string("-3 ^ 1")), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1, _a2) => [_a0,
        _a1,
        _a2])((-5) ^ (-2), 5, _js_to_bosatsu_string("-5 ^ -2")), ((_a0, _a1) => [1,
        _a0,
        _a1])(((_a0, _a1, _a2) => [_a0,
          _a1,
          _a2])(73786976294838206464 ^ 36893488147419103232, 110680464442257309696, _js_to_bosatsu_string("(73786976294838206464) ^ (36893488147419103232)")), ((_a0, _a1) => [1,
          _a0,
          _a1])(((_a0, _a1, _a2) => [_a0,
            _a1,
            _a2])(IntTest$diff1 ^ IntTest$diff1, 0, _js_to_bosatsu_string("diff1")), ((_a0, _a1) => [1,
            _a0,
            _a1])(((_a0, _a1, _a2) => [_a0,
              _a1,
              _a2])(36893488147419103232 - 1 ^ (36893488147419103232 - 1), 0, _js_to_bosatsu_string("(36893488147419103232 - 1) ^ (36893488147419103232 - 1)")), ((_a0, _a1) => [1,
              _a0,
              _a1])(((_a0, _a1, _a2) => [_a0,
                _a1,
                _a2])(36893488147419103231 ^ 36893488147419103231, 0, _js_to_bosatsu_string("(36893488147419103231) ^ (36893488147419103231)")), ((_a0, _a1) => [1,
                _a0,
                _a1])(((_a0, _a1, _a2) => [_a0,
                  _a1,
                  _a2])((-36893488147419103232) ^ (-36893488147419103232), 0, _js_to_bosatsu_string("-36893488147419103232 ^ -36893488147419103232")), ((_a0, _a1) => [1,
                  _a0,
                  _a1])(((_a0, _a1, _a2) => [_a0,
                    _a1,
                    _a2])((-73786976294838206464) ^ (36893488147419103232 - 1), (-36893488147419103233), _js_to_bosatsu_string("-73786976294838206464 ^ (36893488147419103232 - 1)")), ((_a0, _a1) => [1,
                    _a0,
                    _a1])(((_a0, _a1, _a2) => [_a0,
                      _a1,
                      _a2])(3 ^ (-1), (-4), _js_to_bosatsu_string("3 ^ -1")), ((_a0, _a1) => [1,
                      _a0,
                      _a1])(((_a0, _a1, _a2) => [_a0,
                        _a1,
                        _a2])((-3) ^ 2, (-1), _js_to_bosatsu_string("-3 ^ 2")), ((_a0, _a1) => [1,
                        _a0,
                        _a1])(((_a0, _a1, _a2) => [_a0,
                          _a1,
                          _a2])(36893488147419103232 - 1 ^ (-1), (-36893488147419103232), _js_to_bosatsu_string("(36893488147419103232 - 1) ^ -1")), ((_a0, _a1) => [1,
                          _a0,
                          _a1])(((_a0, _a1, _a2) => [_a0,
                            _a1,
                            _a2])((-73786976294838206464) ^ (36893488147419103232 - 1), (-36893488147419103233), _js_to_bosatsu_string("-73786976294838206464 ^ (36893488147419103232 - 1)")), [0])))))))))))));
var IntTest$xor_tests = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("xor tests"), Bosatsu_Predef$map_List(IntTest$xor_test_cases, a => (() => {
      const got = a[0];
      return (() => {
        const ex = a[1];
        return (() => {
          const m = a[2];
          return IntTest$assert_eq(got, ex, m);
        })();
      })();
    })()));
var IntTest$add_sub = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("addition and subtraction"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0,
      _a0,
      _a1])((42 + 58 === 100) ? [1] : [0], _js_to_bosatsu_string("42 + 58 == 100")), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1) => [0,
        _a0,
        _a1])(((-42) * 2 === (-84)) ? [1] : [0], _js_to_bosatsu_string("-42 * 2 == -84")), ((_a0, _a1) => [1,
        _a0,
        _a1])(((_a0, _a1) => [0,
          _a0,
          _a1])((0 - 1 === (-1)) ? [1] : [0], _js_to_bosatsu_string("0 - 1 == -1")), ((_a0, _a1) => [1,
          _a0,
          _a1])(((_a0, _a1) => [0,
            _a0,
            _a1])((2147483647 + 1 === 2147483648) ? [1] : [0], _concat_String(((_a0, _a1) => [1,
                _a0,
                _a1])(_js_to_bosatsu_string("2_147_483_647 + 1 == 2_147_483_648 but got "), ((_a0, _a1) => [1,
                  _a0,
                  _a1])(_int_to_String(2147483647 + 1), [0])))), ((_a0, _a1) => [1,
            _a0,
            _a1])(((_a0, _a1) => [0,
              _a0,
              _a1])((2147483647 - 1 === 2147483646) ? [1] : [0], _js_to_bosatsu_string("2_147_483_647 - 1 == 2_147_483_646")), ((_a0, _a1) => [1,
              _a0,
              _a1])(((_a0, _a1) => [0,
                _a0,
                _a1])(((-2147483648) - 1 === (-2147483649)) ? [1] : [0], _concat_String(((_a0, _a1) => [1,
                    _a0,
                    _a1])(_js_to_bosatsu_string("-2_147_483_648 - 1 == -2_147_483_649 but got "), ((_a0, _a1) => [1,
                      _a0,
                      _a1])(_int_to_String((-2147483648) - 1), [0])))), ((_a0, _a1) => [1,
                _a0,
                _a1])(((_a0, _a1) => [0,
                  _a0,
                  _a1])(((-2147483647) - 1 === (-2147483648)) ? [1] : [0], _concat_String(((_a0, _a1) => [1,
                      _a0,
                      _a1])(_js_to_bosatsu_string("-2_147_483_647 - 1 == -2_147_483_648 but got "), ((_a0, _a1) => [1,
                        _a0,
                        _a1])(_int_to_String((-2147483647) - 1), [0])))), ((_a0, _a1) => [1,
                  _a0,
                  _a1])(((_a0, _a1) => [0,
                    _a0,
                    _a1])(((-2147483647) - 2 === (-2147483649)) ? [1] : [0], _concat_String(((_a0, _a1) => [1,
                        _a0,
                        _a1])(_js_to_bosatsu_string("-2_147_483_647 - 2 == -2_147_483_649 but got "), ((_a0, _a1) => [1,
                          _a0,
                          _a1])(_int_to_String((-2147483647) - 2), [0])))), ((_a0, _a1) => [1,
                    _a0,
                    _a1])(((_a0, _a1) => [0,
                      _a0,
                      _a1])((0 - (-2147483648) === 2147483648) ? [1] : [0], _concat_String(((_a0, _a1) => [1,
                          _a0,
                          _a1])(_js_to_bosatsu_string("-(-2_147_483_648) works: "), ((_a0, _a1) => [1,
                            _a0,
                            _a1])(_int_to_String(0 - (-2147483648)), [0])))), ((_a0, _a1) => [1,
                      _a0,
                      _a1])(((_a0, _a1) => [0,
                        _a0,
                        _a1])((0 - (-2147483647) === 2147483647) ? [1] : [0], _concat_String(((_a0, _a1) => [1,
                            _a0,
                            _a1])(_js_to_bosatsu_string("-(-2_147_483_647) works: "), ((_a0, _a1) => [1,
                              _a0,
                              _a1])(_int_to_String(0 - (-2147483647)), [0])))), ((_a0, _a1) => [1,
                        _a0,
                        _a1])(((_a0, _a1) => [0,
                          _a0,
                          _a1])((2147483647 * 2 === 4294967294) ? [1] : [0], _concat_String(((_a0, _a1) => [1,
                              _a0,
                              _a1])(_js_to_bosatsu_string("2_147_483_647 * 2 == 4_294_967_294 but got: "), ((_a0, _a1) => [1,
                                _a0,
                                _a1])(_int_to_String(2147483647 * 2), [0])))), ((_a0, _a1) => [1,
                          _a0,
                          _a1])(((_a0, _a1) => [0,
                            _a0,
                            _a1])((2147483647 * 10 - (2147483647 * 9) === 2147483647) ? [1] : [0], _concat_String(((_a0, _a1) => [1,
                                _a0,
                                _a1])(_js_to_bosatsu_string("(x := 2_147_483_647) * 10 -  x * 9 == x result = "), ((_a0, _a1) => [1,
                                  _a0,
                                  _a1])(_int_to_String(2147483647 * 10 - (2147483647 * 9)), [0])))), ((_a0, _a1) => [1,
                            _a0,
                            _a1])(((_a0, _a1) => [0,
                              _a0,
                              _a1])((9223372036854775807 + 1 === 9223372036854775808) ? [1] : [0], _js_to_bosatsu_string("9_223_372_036_854_775_807 + 1 == 9_223_372_036_854_775_808")), ((_a0, _a1) => [1,
                              _a0,
                              _a1])(((_a0, _a1) => [0,
                                _a0,
                                _a1])(((-9223372036854775808) - 1 === (-9223372036854775809)) ? [1] : [0], _js_to_bosatsu_string("-9_223_372_036_854_775_808 - 1 == -9_223_372_036_854_775_809")), ((_a0, _a1) => [1,
                                _a0,
                                _a1])(((_a0, _a1) => [0,
                                  _a0,
                                  _a1])((9223372036854775807 * 2 === 18446744073709551614) ? [1] : [0], _js_to_bosatsu_string("9_223_372_036_854_775_807 * 2 == 18_446_744_073_709_551_614")), ((_a0, _a1) => [1,
                                  _a0,
                                  _a1])(((_a0, _a1) => [0,
                                    _a0,
                                    _a1])(((-9223372036854775808) * 2 === (-18446744073709551616)) ? [1] : [0], _js_to_bosatsu_string("-9_223_372_036_854_775_808 * 2 == -18_446_744_073_709_551_616")), ((_a0, _a1) => [1,
                                    _a0,
                                    _a1])(((_a0, _a1) => [0,
                                      _a0,
                                      _a1])((9223372036854775807 + 1 * 2 === 18446744073709551616) ? [1] : [0], _js_to_bosatsu_string("(9_223_372_036_854_775_807 + 1) * 2 == 18_446_744_073_709_551_616")), ((_a0, _a1) => [1,
                                      _a0,
                                      _a1])(((_a0, _a1) => [0,
                                        _a0,
                                        _a1])((9223372036854775807 * 9 - (9223372036854775807 * 8) === 9223372036854775807) ? [1] : [0], _js_to_bosatsu_string("(9_223_372_036_854_775_807 * 9) - (9_223_372_036_854_775_807 * 8) == 9_223_372_036_854_775_807")), ((_a0, _a1) => [1,
                                        _a0,
                                        _a1])(((_a0, _a1) => [0,
                                          _a0,
                                          _a1])((IntTest$diff1 === 36893488147419103231) ? [1] : [0], _js_to_bosatsu_string("diff1 value check")), [0]))))))))))))))))))));
var IntTest$string_tests = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("string tests"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
        const _anon1 = _int_to_String(IntTest$diff1);
        return (_bosatsu_to_js_string(_anon1) === "36893488147419103231") ? [1] : [0];
      })(), _int_to_String(IntTest$diff1)), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
          const _anon2 = _int_to_String(IntTest$diff1 & IntTest$diff1);
          return (_bosatsu_to_js_string(_anon2) === "36893488147419103231") ? [1] : [0];
        })(), _int_to_String(IntTest$diff1)), [0])));
var IntTest$shifts = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("shift tests"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
        const _anon3 = (-23) << 1 >> 1;
        return (_anon3 === (-23)) ? [1] : [0];
      })(), _concat_String(((_a0, _a1) => [1,
          _a0,
          _a1])(_js_to_bosatsu_string("(-23 << 1) >> 1 returned "), ((_a0, _a1) => [1,
            _a0,
            _a1])(_int_to_String((-23) << 1 >> 1), [0])))), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
          const _anon4 = (-23) << 1;
          return (_anon4 === (-46)) ? [1] : [0];
        })(), _concat_String(((_a0, _a1) => [1,
            _a0,
            _a1])(_js_to_bosatsu_string("(-23 << 1) returned "), ((_a0, _a1) => [1,
              _a0,
              _a1])(_int_to_String((-23) << 1), [0])))), ((_a0, _a1) => [1,
        _a0,
        _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
            const _anon5 = 23 << 1 >> 1;
            return (_anon5 === 23) ? [1] : [0];
          })(), _concat_String(((_a0, _a1) => [1,
              _a0,
              _a1])(_js_to_bosatsu_string("(23 << 1) >> 1 returned "), ((_a0, _a1) => [1,
                _a0,
                _a1])(_int_to_String(23 << 1 >> 1), [0])))), ((_a0, _a1) => [1,
          _a0,
          _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
              const _anon6 = (-8034984067920789066) << 1 >> 1;
              return (_anon6 === (-8034984067920789066)) ? [1] : [0];
            })(), _concat_String(((_a0, _a1) => [1,
                _a0,
                _a1])(_js_to_bosatsu_string("(-8034984067920789066 << 1) >> 1 returned "), ((_a0, _a1) => [1,
                  _a0,
                  _a1])(_int_to_String((-8034984067920789066) << 1 >> 1), [0])))), ((_a0, _a1) => [1,
            _a0,
            _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                const _anon7 = 16470527259447964275 << 20 >> 20;
                return (_anon7 === 16470527259447964275) ? [1] : [0];
              })(), _concat_String(((_a0, _a1) => [1,
                  _a0,
                  _a1])(_js_to_bosatsu_string("(16470527259447964275 << 20) >> 20) got "), ((_a0, _a1) => [1,
                    _a0,
                    _a1])(_int_to_String(16470527259447964275 << 20 >> 20), [0])))), ((_a0, _a1) => [1,
              _a0,
              _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                  const _anon8 = 16470527259447964275 << 20;
                  return (_anon8 === 17270599591602908587622400) ? [1] : [0];
                })(), _concat_String(((_a0, _a1) => [1,
                    _a0,
                    _a1])(_js_to_bosatsu_string("(16470527259447964275 << 20) returned "), ((_a0, _a1) => [1,
                      _a0,
                      _a1])(_int_to_String(16470527259447964275 << 20), [0])))), ((_a0, _a1) => [1,
                _a0,
                _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                    const _anon9 = 17270599591602908587622400 >> 20;
                    return (_anon9 === 16470527259447964275) ? [1] : [0];
                  })(), _concat_String(((_a0, _a1) => [1,
                      _a0,
                      _a1])(_js_to_bosatsu_string("17270599591602908587622400 >> 20 returned "), ((_a0, _a1) => [1,
                        _a0,
                        _a1])(_int_to_String(17270599591602908587622400 >> 20), [0])))), [0]))))))));
var IntTest$tests = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("integer tests"), ((_a0, _a1) => [1,
    _a0,
    _a1])(IntTest$add_sub, ((_a0, _a1) => [1,
      _a0,
      _a1])(IntTest$string_tests, ((_a0, _a1) => [1,
        _a0,
        _a1])(IntTest$and_tests, ((_a0, _a1) => [1,
          _a0,
          _a1])(IntTest$or_tests, ((_a0, _a1) => [1,
            _a0,
            _a1])(IntTest$xor_tests, ((_a0, _a1) => [1,
              _a0,
              _a1])(IntTest$shifts, [0])))))));
_tests["IntTest::tests"] = IntTest$tests;
var ListPat$one_count = (is, acc) => (() => {
  let _anon5;
  return (() => {
    let _anon6;
    return (() => {
      let _anon8;
      return (() => {
        let _anon10;
        return (() => {
          (() => {
            _anon8 = is;
            return true;
          })();
          return (() => {
            (() => {
              _anon10 = acc;
              return true;
            })();
            return (() => {
              (() => {
                _anon5 = [1];
                return true;
              })();
              return (() => {
                while (_anon5[0] === 1) {
                  (() => {
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
                                  _anon4 = _anon8;
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
                                      })() && (_anon1 === 1)) ? (() => {
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
                      })() ? (() => {
                          const rest = _anon0[2];
                          return (() => {
                            const _anon7 = rest;
                            return (() => {
                              const _anon9 = 1 + _anon10;
                              return (() => {
                                (() => {
                                  _anon8 = _anon7;
                                  return true;
                                })();
                                return (() => {
                                  (() => {
                                    _anon10 = _anon9;
                                    return true;
                                  })();
                                  return [];
                                })();
                              })();
                            })();
                          })();
                        })() : (() => {
                          (() => {
                            _anon5 = [0];
                            return true;
                          })();
                          return (() => {
                            (() => {
                              _anon6 = _anon10;
                              return true;
                            })();
                            return [];
                          })();
                        })();
                    })();
                  })();
                }
                return _anon6;
              })();
            })();
          })();
        })();
      })();
    })();
  })();
})();
var ListPat$tests = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("ListPat tests"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
        const _anon17 = (() => {
          const _anon11 = [0];
          return (() => {
            let _anon12;
            return (() => {
              let _anon13;
              return (() => {
                let _anon14;
                return (() => {
                  let _anon16;
                  return (() => {
                    const _anon15 = (() => {
                      (() => {
                        _anon14 = [0];
                        return true;
                      })();
                      return (() => {
                        (() => {
                          _anon16 = _anon11;
                          return true;
                        })();
                        return (() => {
                          while (_anon16[0] === 1) {
                            (() => {
                              (() => {
                                _anon12 = _anon16;
                                return true;
                              })();
                              return (_anon12[0] === 1 && (() => {
                                _anon13 = _anon12[1];
                                return true;
                              })() && (_anon13[0] === 1)) ? (() => {
                                  (() => {
                                    _anon16 = [0];
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
                                  (() => {
                                    _anon16 = _anon16[2];
                                    return true;
                                  })();
                                  return [];
                                })();
                            })();
                          }
                          return _anon14;
                        })();
                      })();
                    })();
                    return _anon14[0] === 1;
                  })();
                })();
              })() ? [1] : [0];
            })();
          })();
        })();
        return (_anon17[0] === 0) ? [1] : [0];
      })(), _js_to_bosatsu_string("any([])")), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
          const _anon24 = (() => {
            const _anon18 = ((_a0, _a1) => [1, _a0, _a1])([1], [0]);
            return (() => {
              let _anon19;
              return (() => {
                let _anon20;
                return (() => {
                  let _anon21;
                  return (() => {
                    let _anon23;
                    return (() => {
                      const _anon22 = (() => {
                        (() => {
                          _anon21 = [0];
                          return true;
                        })();
                        return (() => {
                          (() => {
                            _anon23 = _anon18;
                            return true;
                          })();
                          return (() => {
                            while (_anon23[0] === 1) {
                              (() => {
                                (() => {
                                  _anon19 = _anon23;
                                  return true;
                                })();
                                return (_anon19[0] === 1 && (() => {
                                  _anon20 = _anon19[1];
                                  return true;
                                })() && (_anon20[0] === 1)) ? (() => {
                                    (() => {
                                      _anon23 = [0];
                                      return true;
                                    })();
                                    return (() => {
                                      (() => {
                                        _anon21 = [1];
                                        return true;
                                      })();
                                      return [];
                                    })();
                                  })() : (() => {
                                    (() => {
                                      _anon23 = _anon23[2];
                                      return true;
                                    })();
                                    return [];
                                  })();
                              })();
                            }
                            return _anon21;
                          })();
                        })();
                      })();
                      return _anon21[0] === 1;
                    })();
                  })();
                })() ? [1] : [0];
              })();
            })();
          })();
          return (_anon24[0] === 1) ? [1] : [0];
        })(), _js_to_bosatsu_string("any([True])")), ((_a0, _a1) => [1,
        _a0,
        _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
            const _anon31 = (() => {
              const _anon25 = ((_a0, _a1) => [1, _a0, _a1])([0], [0]);
              return (() => {
                let _anon26;
                return (() => {
                  let _anon27;
                  return (() => {
                    let _anon28;
                    return (() => {
                      let _anon30;
                      return (() => {
                        const _anon29 = (() => {
                          (() => {
                            _anon28 = [0];
                            return true;
                          })();
                          return (() => {
                            (() => {
                              _anon30 = _anon25;
                              return true;
                            })();
                            return (() => {
                              while (_anon30[0] === 1) {
                                (() => {
                                  (() => {
                                    _anon26 = _anon30;
                                    return true;
                                  })();
                                  return (_anon26[0] === 1 && (() => {
                                    _anon27 = _anon26[1];
                                    return true;
                                  })() && (_anon27[0] === 1)) ? (() => {
                                      (() => {
                                        _anon30 = [0];
                                        return true;
                                      })();
                                      return (() => {
                                        (() => {
                                          _anon28 = [1];
                                          return true;
                                        })();
                                        return [];
                                      })();
                                    })() : (() => {
                                      (() => {
                                        _anon30 = _anon30[2];
                                        return true;
                                      })();
                                      return [];
                                    })();
                                })();
                              }
                              return _anon28;
                            })();
                          })();
                        })();
                        return _anon28[0] === 1;
                      })();
                    })();
                  })() ? [1] : [0];
                })();
              })();
            })();
            return (_anon31[0] === 0) ? [1] : [0];
          })(), _js_to_bosatsu_string("any([False])")), ((_a0, _a1) => [1,
          _a0,
          _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
              const _anon38 = (() => {
                const _anon32 = ((_a0, _a1) => [1,
                  _a0,
                  _a1])([0], ((_a0, _a1) => [1, _a0, _a1])([0], [0]));
                return (() => {
                  let _anon33;
                  return (() => {
                    let _anon34;
                    return (() => {
                      let _anon35;
                      return (() => {
                        let _anon37;
                        return (() => {
                          const _anon36 = (() => {
                            (() => {
                              _anon35 = [0];
                              return true;
                            })();
                            return (() => {
                              (() => {
                                _anon37 = _anon32;
                                return true;
                              })();
                              return (() => {
                                while (_anon37[0] === 1) {
                                  (() => {
                                    (() => {
                                      _anon33 = _anon37;
                                      return true;
                                    })();
                                    return (_anon33[0] === 1 && (() => {
                                      _anon34 = _anon33[1];
                                      return true;
                                    })() && (_anon34[0] === 1)) ? (() => {
                                        (() => {
                                          _anon37 = [0];
                                          return true;
                                        })();
                                        return (() => {
                                          (() => {
                                            _anon35 = [1];
                                            return true;
                                          })();
                                          return [];
                                        })();
                                      })() : (() => {
                                        (() => {
                                          _anon37 = _anon37[2];
                                          return true;
                                        })();
                                        return [];
                                      })();
                                  })();
                                }
                                return _anon35;
                              })();
                            })();
                          })();
                          return _anon35[0] === 1;
                        })();
                      })();
                    })() ? [1] : [0];
                  })();
                })();
              })();
              return (_anon38[0] === 0) ? [1] : [0];
            })(), _js_to_bosatsu_string("any([False, False])")), ((_a0, _a1) => [1,
            _a0,
            _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                const _anon45 = (() => {
                  const _anon39 = ((_a0, _a1) => [1,
                    _a0,
                    _a1])([0], ((_a0, _a1) => [1, _a0, _a1])([1], [0]));
                  return (() => {
                    let _anon40;
                    return (() => {
                      let _anon41;
                      return (() => {
                        let _anon42;
                        return (() => {
                          let _anon44;
                          return (() => {
                            const _anon43 = (() => {
                              (() => {
                                _anon42 = [0];
                                return true;
                              })();
                              return (() => {
                                (() => {
                                  _anon44 = _anon39;
                                  return true;
                                })();
                                return (() => {
                                  while (_anon44[0] === 1) {
                                    (() => {
                                      (() => {
                                        _anon40 = _anon44;
                                        return true;
                                      })();
                                      return (_anon40[0] === 1 && (() => {
                                        _anon41 = _anon40[1];
                                        return true;
                                      })() && (_anon41[0] === 1)) ? (() => {
                                          (() => {
                                            _anon44 = [0];
                                            return true;
                                          })();
                                          return (() => {
                                            (() => {
                                              _anon42 = [1];
                                              return true;
                                            })();
                                            return [];
                                          })();
                                        })() : (() => {
                                          (() => {
                                            _anon44 = _anon44[2];
                                            return true;
                                          })();
                                          return [];
                                        })();
                                    })();
                                  }
                                  return _anon42;
                                })();
                              })();
                            })();
                            return _anon42[0] === 1;
                          })();
                        })();
                      })() ? [1] : [0];
                    })();
                  })();
                })();
                return (_anon45[0] === 1) ? [1] : [0];
              })(), _js_to_bosatsu_string("any([False, True])")), ((_a0, _a1) => [1,
              _a0,
              _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                  const _anon52 = (() => {
                    const _anon46 = ((_a0, _a1) => [1,
                      _a0,
                      _a1])([1], ((_a0, _a1) => [1, _a0, _a1])([0], [0]));
                    return (() => {
                      let _anon47;
                      return (() => {
                        let _anon48;
                        return (() => {
                          let _anon49;
                          return (() => {
                            let _anon51;
                            return (() => {
                              const _anon50 = (() => {
                                (() => {
                                  _anon49 = [0];
                                  return true;
                                })();
                                return (() => {
                                  (() => {
                                    _anon51 = _anon46;
                                    return true;
                                  })();
                                  return (() => {
                                    while (_anon51[0] === 1) {
                                      (() => {
                                        (() => {
                                          _anon47 = _anon51;
                                          return true;
                                        })();
                                        return (_anon47[0] === 1 && (() => {
                                          _anon48 = _anon47[1];
                                          return true;
                                        })() && (_anon48[0] === 1)) ? (() => {
                                            (() => {
                                              _anon51 = [0];
                                              return true;
                                            })();
                                            return (() => {
                                              (() => {
                                                _anon49 = [1];
                                                return true;
                                              })();
                                              return [];
                                            })();
                                          })() : (() => {
                                            (() => {
                                              _anon51 = _anon51[2];
                                              return true;
                                            })();
                                            return [];
                                          })();
                                      })();
                                    }
                                    return _anon49;
                                  })();
                                })();
                              })();
                              return _anon49[0] === 1;
                            })();
                          })();
                        })() ? [1] : [0];
                      })();
                    })();
                  })();
                  return (_anon52[0] === 1) ? [1] : [0];
                })(), _js_to_bosatsu_string("any([True, False])")), ((_a0, _a1) => [1,
                _a0,
                _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                    const _anon59 = (() => {
                      const _anon53 = ((_a0, _a1) => [1,
                        _a0,
                        _a1])([1], ((_a0, _a1) => [1, _a0, _a1])([1], [0]));
                      return (() => {
                        let _anon54;
                        return (() => {
                          let _anon55;
                          return (() => {
                            let _anon56;
                            return (() => {
                              let _anon58;
                              return (() => {
                                const _anon57 = (() => {
                                  (() => {
                                    _anon56 = [0];
                                    return true;
                                  })();
                                  return (() => {
                                    (() => {
                                      _anon58 = _anon53;
                                      return true;
                                    })();
                                    return (() => {
                                      while (_anon58[0] === 1) {
                                        (() => {
                                          (() => {
                                            _anon54 = _anon58;
                                            return true;
                                          })();
                                          return (_anon54[0] === 1 && (() => {
                                            _anon55 = _anon54[1];
                                            return true;
                                          })() && (_anon55[0] === 1)) ? (() => {
                                              (() => {
                                                _anon58 = [0];
                                                return true;
                                              })();
                                              return (() => {
                                                (() => {
                                                  _anon56 = [1];
                                                  return true;
                                                })();
                                                return [];
                                              })();
                                            })() : (() => {
                                              (() => {
                                                _anon58 = _anon58[2];
                                                return true;
                                              })();
                                              return [];
                                            })();
                                        })();
                                      }
                                      return _anon56;
                                    })();
                                  })();
                                })();
                                return _anon56[0] === 1;
                              })();
                            })();
                          })() ? [1] : [0];
                        })();
                      })();
                    })();
                    return (_anon59[0] === 1) ? [1] : [0];
                  })(), _js_to_bosatsu_string("any([True, True])")), ((_a0, _a1) => [1,
                  _a0,
                  _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                      const _anon60 = ListPat$one_count(((_a0, _a1) => [1,
                          _a0,
                          _a1])(0, ((_a0, _a1) => [1,
                            _a0,
                            _a1])(0, ((_a0, _a1) => [1,
                              _a0,
                              _a1])(1, ((_a0, _a1) => [1,
                                _a0,
                                _a1])(0, ((_a0, _a1) => [1,
                                  _a0,
                                  _a1])(1, [0]))))), 0);
                      return (_anon60 === 2) ? [1] : [0];
                    })(), _js_to_bosatsu_string("one_count 1")), [0])))))))));
_tests["ListPat::tests"] = ListPat$tests;
var Parser$empty = (_a0 => [_a0])(a => [0]);
var Parser$parse = (p, str) => (() => {
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
var Parser$expect = str => (_a0 => [_a0])((_slots => s => (() => {
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
var Parser$map = (p, fn) => (() => {
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
var Parser$flat_map = (p, fn) => (() => {
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
var Parser$one_of = ps => (ps[0] === 0) ? Parser$empty : (() => {
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
                const _anon11 = Parser$one_of(prest);
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
var Parser$then_parse = (pa, pb) => (() => {
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
var Parser$expect_int = i => Parser$map((() => {
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
var Parser$digit = Parser$one_of(Bosatsu_Predef$map_List(range(10), Parser$expect_int));
var Parser$recur_max = (n, fn, _in) => (() => {
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
var Parser$digits_n = at_most => (() => {
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
var Parser$tests = ((_a0, _a1) => [1,
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
_tests["Parser::tests"] = Parser$tests;
var PatternExamples$combine = _concat_String(((_a0, _a1) => [1,
    _a0,
    _a1])(_js_to_bosatsu_string("foo: "), ((_a0, _a1) => [1,
      _a0,
      _a1])(_js_to_bosatsu_string("this is foo"), ((_a0, _a1) => [1,
        _a0,
        _a1])(_js_to_bosatsu_string(" bar: "), ((_a0, _a1) => [1,
          _a0,
          _a1])(_js_to_bosatsu_string("this is bar"), [0])))));
var PatternExamples$fb = (() => {
  let _anon3;
  return (() => {
    let _anon4;
    return (() => {
      const _str = _bosatsu_to_js_string(PatternExamples$combine);
      return _str.startsWith("foo: ") && false;
    })() ? (() => {
        const f = _anon3;
        return (() => {
          const b = _anon4;
          return (() => {
            const _anon2 = (() => {
              const _anon0 = _cmp_String(f, _js_to_bosatsu_string("this is foo"));
              return (_anon0[0] === 1) ? [1] : [0];
            })();
            return (_anon2[0] === 1) ? (() => {
                const _anon1 = _cmp_String(b, _js_to_bosatsu_string("this is bar"));
                return (_anon1[0] === 1) ? [1] : [0];
              })() : [0];
          })();
        })();
      })() : [0];
  })();
})();
var PatternExamples$test0 = ((_a0, _a1) => [0,
  _a0,
  _a1])(PatternExamples$fb, _js_to_bosatsu_string("foo-bar match"));
var PatternExamples$get_foos = s => (() => {
  let _anon5;
  return (() => {
    let _anon6;
    return (() => {
      const _str = _bosatsu_to_js_string(s);
      return false;
    })() ? (() => {
        const foo = _anon5;
        return (() => {
          const rest = _anon6;
          return ((_a0, _a1) => [1,
            _a0,
            _a1])(foo, PatternExamples$get_foos(rest));
        })();
      })() : [0];
  })();
})();
var PatternExamples$test1 = (() => {
  const _anon7 = PatternExamples$get_foos(_js_to_bosatsu_string("foo: (1) foo: (2)"));
  return (() => {
    let _anon9;
    return (() => {
      let _anon8;
      return (() => {
        let _anon11;
        return (() => {
          let _anon10;
          return (_anon7[0] === 1 && (() => {
            _anon8 = _anon7[1];
            return true;
          })() && (() => {
            _anon9 = _anon7[2];
            return true;
          })() && (_bosatsu_to_js_string(_anon8) === "1" && (_anon9[0] === 1 && (() => {
            _anon10 = _anon9[1];
            return true;
          })() && (() => {
            _anon11 = _anon9[2];
            return true;
          })() && (_bosatsu_to_js_string(_anon10) === "2" && (_anon11[0] === 0))))) ? ((_a0, _a1) => [0,
              _a0,
              _a1])([1], _js_to_bosatsu_string("get_foos")) : (() => {
              let _anon12;
              return (_anon7[0] === 1 && (() => {
                _anon12 = _anon7[2];
                return true;
              })() && (_anon12[0] === 0)) ? (() => {
                  const one = _anon7[1];
                  return ((_a0, _a1) => [0,
                    _a0,
                    _a1])([0], _concat_String(((_a0, _a1) => [1,
                        _a0,
                        _a1])(_js_to_bosatsu_string("get_foos: "), ((_a0, _a1) => [1,
                          _a0,
                          _a1])(one, [0]))));
                })() : ((_a0, _a1) => [0,
                  _a0,
                  _a1])([0], _js_to_bosatsu_string("get_foos"));
            })();
        })();
      })();
    })();
  })();
})();
var PatternExamples$test2 = ((_a0, _a1) => [0, _a0, _a1])([1], _js_to_bosatsu_string("test unnamed match"));
var PatternExamples$tests = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("PatternExamples"), ((_a0, _a1) => [1,
    _a0,
    _a1])(PatternExamples$test0, ((_a0, _a1) => [1,
      _a0,
      _a1])(PatternExamples$test1, ((_a0, _a1) => [1,
        _a0,
        _a1])(PatternExamples$test2, [0]))));
_tests["PatternExamples::tests"] = PatternExamples$tests;
var PredefTests$oi = opt => (opt[0] === 0) ? _js_to_bosatsu_string("None") : (() => {
    const v = opt[1];
    return _concat_String(((_a0, _a1) => [1,
        _a0,
        _a1])(_js_to_bosatsu_string("Some("), ((_a0, _a1) => [1,
          _a0,
          _a1])(_int_to_String(v), ((_a0, _a1) => [1,
            _a0,
            _a1])(_js_to_bosatsu_string(")"), [0]))));
  })();
var PredefTests$test_int = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("Int tests"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
        const _anon0 = (-3) ? 4 % (-3) : 4;
        return (_anon0 === (-2)) ? [1] : [0];
      })(), _concat_String(((_a0, _a1) => [1,
          _a0,
          _a1])(_js_to_bosatsu_string("(4 % -3) == -2 got: "), ((_a0, _a1) => [1,
            _a0,
            _a1])(_int_to_String((-3) ? 4 % (-3) : 4), [0])))), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
          const _anon1 = (-2) ? (-8) % (-2) : (-8);
          return (_anon1 === 0) ? [1] : [0];
        })(), _concat_String(((_a0, _a1) => [1,
            _a0,
            _a1])(_js_to_bosatsu_string("(-8 % -2) == 0 got: "), ((_a0, _a1) => [1,
              _a0,
              _a1])(_int_to_String((-2) ? (-8) % (-2) : (-8)), [0])))), ((_a0, _a1) => [1,
        _a0,
        _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
            const _anon2 = (-2) ? Math.trunc((-8) / (-2)) : 0;
            return (_anon2 === 4) ? [1] : [0];
          })(), _concat_String(((_a0, _a1) => [1,
              _a0,
              _a1])(_js_to_bosatsu_string("(-8 / -2) == 4 got: "), ((_a0, _a1) => [1,
                _a0,
                _a1])(_int_to_String((-2) ? Math.trunc((-8) / (-2)) : 0), [0])))), ((_a0, _a1) => [1,
          _a0,
          _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
              const _anon3 = (-3) ? Math.trunc(5 / (-3)) : 0;
              return (_anon3 === (-2)) ? [1] : [0];
            })(), _concat_String(((_a0, _a1) => [1,
                _a0,
                _a1])(_js_to_bosatsu_string("(5 / -3) == -2 got: "), ((_a0, _a1) => [1,
                  _a0,
                  _a1])(_int_to_String((-3) ? Math.trunc(5 / (-3)) : 0), [0])))), ((_a0, _a1) => [1,
            _a0,
            _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                const _anon4 = 3 ? Math.trunc((-5) / 3) : 0;
                return (_anon4 === (-2)) ? [1] : [0];
              })(), _concat_String(((_a0, _a1) => [1,
                  _a0,
                  _a1])(_js_to_bosatsu_string("(-5 / 3) == -2 got: "), ((_a0, _a1) => [1,
                    _a0,
                    _a1])(_int_to_String(3 ? Math.trunc((-5) / 3) : 0), [0])))), ((_a0, _a1) => [1,
              _a0,
              _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                  const _anon5 = 3 ? Math.trunc(5 / 3) : 0;
                  return (_anon5 === 1) ? [1] : [0];
                })(), _concat_String(((_a0, _a1) => [1,
                    _a0,
                    _a1])(_js_to_bosatsu_string("(5 / 3) == 1 got "), ((_a0, _a1) => [1,
                      _a0,
                      _a1])(_int_to_String(3 ? Math.trunc(5 / 3) : 0), [0])))), ((_a0, _a1) => [1,
                _a0,
                _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                    const _anon6 = (-3) ? Math.trunc((-5) / (-3)) : 0;
                    return (_anon6 === 1) ? [1] : [0];
                  })(), _concat_String(((_a0, _a1) => [1,
                      _a0,
                      _a1])(_js_to_bosatsu_string("(5 / 3) == 1 got "), ((_a0, _a1) => [1,
                        _a0,
                        _a1])(_int_to_String((-3) ? Math.trunc((-5) / (-3)) : 0), [0])))), ((_a0, _a1) => [1,
                  _a0,
                  _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                      const _anon7 = (-3) ? 5 % (-3) : 5;
                      return (_anon7 === (-1)) ? [1] : [0];
                    })(), _concat_String(((_a0, _a1) => [1,
                        _a0,
                        _a1])(_js_to_bosatsu_string("(5 % -3) == -1 got "), ((_a0, _a1) => [1,
                          _a0,
                          _a1])(_int_to_String((-3) ? 5 % (-3) : 5), [0])))), ((_a0, _a1) => [1,
                    _a0,
                    _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                        const _anon8 = 3 ? (-5) % 3 : (-5);
                        return (_anon8 === 1) ? [1] : [0];
                      })(), _concat_String(((_a0, _a1) => [1,
                          _a0,
                          _a1])(_js_to_bosatsu_string("(5 % -3) == 1 got "), ((_a0, _a1) => [1,
                            _a0,
                            _a1])(_int_to_String(3 ? (-5) % 3 : (-5)), [0])))), ((_a0, _a1) => [1,
                      _a0,
                      _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                          const _anon9 = 3 ? 5 % 3 : 5;
                          return (_anon9 === 2) ? [1] : [0];
                        })(), _concat_String(((_a0, _a1) => [1,
                            _a0,
                            _a1])(_js_to_bosatsu_string("(5 % 3) == 2 got "), ((_a0, _a1) => [1,
                              _a0,
                              _a1])(_int_to_String(3 ? 5 % 3 : 5), [0])))), ((_a0, _a1) => [1,
                        _a0,
                        _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                            const _anon10 = (-3) ? (-5) % (-3) : (-5);
                            return (_anon10 === (-2)) ? [1] : [0];
                          })(), _concat_String(((_a0, _a1) => [1,
                              _a0,
                              _a1])(_js_to_bosatsu_string("(-5 % -3) == -2 got "), ((_a0, _a1) => [1,
                                _a0,
                                _a1])(_int_to_String((-3) ? (-5) % (-3) : (-5)), [0])))), ((_a0, _a1) => [1,
                          _a0,
                          _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                              const _anon11 = 599767409 ? Math.trunc((-15934641381326140386510) / 599767409) : 0;
                              return (_anon11 === (-26568034778506)) ? [1] : [0];
                            })(), _concat_String(((_a0, _a1) => [1,
                                _a0,
                                _a1])(_js_to_bosatsu_string("(-15934641381326140386510 / 599767409) matches -26568034778506 got "), ((_a0, _a1) => [1,
                                  _a0,
                                  _a1])(_int_to_String(599767409 ? Math.trunc((-15934641381326140386510) / 599767409) : 0), [0])))), ((_a0, _a1) => [1,
                            _a0,
                            _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                                const _anon12 = 599767409 ? (-15934641381326140386510) % 599767409 : (-15934641381326140386510);
                                return (_anon12 === 292124444) ? [1] : [0];
                              })(), _concat_String(((_a0, _a1) => [1,
                                  _a0,
                                  _a1])(_js_to_bosatsu_string("(-15934641381326140386510 % 599767409) matches 292124444 got "), ((_a0, _a1) => [1,
                                    _a0,
                                    _a1])(_int_to_String(599767409 ? (-15934641381326140386510) % 599767409 : (-15934641381326140386510)), [0])))), ((_a0, _a1) => [1,
                              _a0,
                              _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                                  const _anon13 = (-7104274460) ? Math.trunc(2885517232792582372714 / (-7104274460)) : 0;
                                  return (_anon13 === (-406166350842)) ? [1] : [0];
                                })(), _concat_String(((_a0, _a1) => [1,
                                    _a0,
                                    _a1])(_js_to_bosatsu_string("(2885517232792582372714 / -7104274460) matches -406166350842 got "), ((_a0, _a1) => [1,
                                      _a0,
                                      _a1])(_int_to_String((-7104274460) ? Math.trunc(2885517232792582372714 / (-7104274460)) : 0), [0])))), ((_a0, _a1) => [1,
                                _a0,
                                _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                                    const _anon14 = (-7104274460) ? 2885517232792582372714 % (-7104274460) : 2885517232792582372714;
                                    return (_anon14 === (-5637722606)) ? [1] : [0];
                                  })(), _concat_String(((_a0, _a1) => [1,
                                      _a0,
                                      _a1])(_js_to_bosatsu_string("(2885517232792582372714 % -7104274460) matches -5637722606 got "), ((_a0, _a1) => [1,
                                        _a0,
                                        _a1])(_int_to_String((-7104274460) ? 2885517232792582372714 % (-7104274460) : 2885517232792582372714), [0])))), ((_a0, _a1) => [1,
                                  _a0,
                                  _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                                      const _anon15 = 7104274460 ? Math.trunc(671836834585 / 7104274460) : 0;
                                      return (_anon15 === 94) ? [1] : [0];
                                    })(), _concat_String(((_a0, _a1) => [1,
                                        _a0,
                                        _a1])(_js_to_bosatsu_string("(671836834585 / 7104274460) matches 94 got "), ((_a0, _a1) => [1,
                                          _a0,
                                          _a1])(_int_to_String(7104274460 ? Math.trunc(671836834585 / 7104274460) : 0), [0])))), ((_a0, _a1) => [1,
                                    _a0,
                                    _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                                        const _anon16 = 7104274460 ? 671836834585 % 7104274460 : 671836834585;
                                        return (_anon16 === 4035035345) ? [1] : [0];
                                      })(), _concat_String(((_a0, _a1) => [1,
                                          _a0,
                                          _a1])(_js_to_bosatsu_string("(671836834585 % 7104274460) matches 4035035345 got "), ((_a0, _a1) => [1,
                                            _a0,
                                            _a1])(_int_to_String(7104274460 ? 671836834585 % 7104274460 : 671836834585), [0])))), ((_a0, _a1) => [1,
                                      _a0,
                                      _a1])(((_a0, _a1) => [0,
                                        _a0,
                                        _a1])((() => {
                                          const _anon17 = (-3) ? (-4) % (-3) : (-4);
                                          return (_anon17 === (-1)) ? [1] : [0];
                                        })(), _js_to_bosatsu_string("(-4 % -3) == -1")), ((_a0, _a1) => [1,
                                        _a0,
                                        _a1])(((_a0, _a1) => [0,
                                          _a0,
                                          _a1])((() => {
                                            const _anon18 = 3 ? 13 % 3 : 13;
                                            return (_anon18 === 1) ? [1] : [0];
                                          })(), _js_to_bosatsu_string("13 % 3 == 1")), ((_a0, _a1) => [1,
                                          _a0,
                                          _a1])(((_a0, _a1) => [0,
                                            _a0,
                                            _a1])((() => {
                                              const _anon19 = 16 ? Math.trunc((-113) / 16) : 0;
                                              return (_anon19 === (-8)) ? [1] : [0];
                                            })(), _js_to_bosatsu_string("-113/16 == -8")), ((_a0, _a1) => [1,
                                            _a0,
                                            _a1])(((_a0, _a1) => [0,
                                              _a0,
                                              _a1])((() => {
                                                const _anon20 = (-3) ? 54 % (-3) : 54;
                                                return (_anon20 === 0) ? [1] : [0];
                                              })(), _js_to_bosatsu_string("54 % -3 == 0")), ((_a0, _a1) => [1,
                                              _a0,
                                              _a1])(((_a0, _a1) => [0,
                                                _a0,
                                                _a1])((() => {
                                                  const _anon21 = (-3) ? Math.trunc(54 / (-3)) : 0;
                                                  return (_anon21 === (-18)) ? [1] : [0];
                                                })(), _js_to_bosatsu_string("54 / -3 == -18")), ((_a0, _a1) => [1,
                                                _a0,
                                                _a1])(((_a0, _a1) => [0,
                                                  _a0,
                                                  _a1])((() => {
                                                    const _anon22 = 0 ? 54 % 0 : 54;
                                                    return (_anon22 === 54) ? [1] : [0];
                                                  })(), _js_to_bosatsu_string("54 % 0 == 54")), ((_a0, _a1) => [1,
                                                  _a0,
                                                  _a1])(((_a0, _a1) => [0,
                                                    _a0,
                                                    _a1])((() => {
                                                      const _anon23 = 0 ? Math.trunc(54 / 0) : 0;
                                                      return (_anon23 === 0) ? [1] : [0];
                                                    })(), _js_to_bosatsu_string("54 / 0 == 0")), ((_a0, _a1) => [1,
                                                    _a0,
                                                    _a1])(((_a0, _a1) => [0,
                                                      _a0,
                                                      _a1])((() => {
                                                        const _anon24 = 0 ? (-54) % 0 : (-54);
                                                        return (_anon24 === (-54)) ? [1] : [0];
                                                      })(), _js_to_bosatsu_string("-54 % 0 == -54")), ((_a0, _a1) => [1,
                                                      _a0,
                                                      _a1])(((_a0, _a1) => [0,
                                                        _a0,
                                                        _a1])((() => {
                                                          const _anon25 = 0 ? Math.trunc((-54) / 0) : 0;
                                                          return (_anon25 === 0) ? [1] : [0];
                                                        })(), _js_to_bosatsu_string("-54 / 0 == 0")), ((_a0, _a1) => [1,
                                                        _a0,
                                                        _a1])(((_a0, _a1) => [0,
                                                          _a0,
                                                          _a1])((() => {
                                                            const _anon26 = 1 << 1;
                                                            return (_anon26 === 2) ? [1] : [0];
                                                          })(), _js_to_bosatsu_string("1 << 1 == 2")), ((_a0, _a1) => [1,
                                                          _a0,
                                                          _a1])(((_a0, _a1) => [0,
                                                            _a0,
                                                            _a1])((() => {
                                                              const _anon27 = 1 >> 1;
                                                              return (_anon27 === 0) ? [1] : [0];
                                                            })(), _js_to_bosatsu_string("1 >> 1 == 0")), ((_a0, _a1) => [1,
                                                            _a0,
                                                            _a1])(((_a0, _a1) => [0,
                                                              _a0,
                                                              _a1])((() => {
                                                                const _anon28 = 1 ^ 1;
                                                                return (_anon28 === 0) ? [1] : [0];
                                                              })(), _js_to_bosatsu_string("1 ^ 1 == 0")), ((_a0, _a1) => [1,
                                                              _a0,
                                                              _a1])(((_a0, _a1) => [0,
                                                                _a0,
                                                                _a1])((() => {
                                                                  const _anon29 = (-1) & 23;
                                                                  return (_anon29 === 23) ? [1] : [0];
                                                                })(), _js_to_bosatsu_string("-1 & 23 == 23")), ((_a0, _a1) => [1,
                                                                _a0,
                                                                _a1])(((_a0, _a1) => [0,
                                                                  _a0,
                                                                  _a1])((() => {
                                                                    const _anon30 = (-1) | 23;
                                                                    return (_anon30 === (-1)) ? [1] : [0];
                                                                  })(), _js_to_bosatsu_string("-1 | 23 == -1")), ((_a0, _a1) => [1,
                                                                  _a0,
                                                                  _a1])(((_a0, _a1) => [0,
                                                                    _a0,
                                                                    _a1])((() => {
                                                                      const _anon31 = ~(-2);
                                                                      return (_anon31 === 1) ? [1] : [0];
                                                                    })(), _js_to_bosatsu_string("~(-2) == 1")), ((_a0, _a1) => [1,
                                                                    _a0,
                                                                    _a1])(((_a0, _a1) => [0,
                                                                      _a0,
                                                                      _a1])((() => {
                                                                        const _anon32 = _gcd(4, 3);
                                                                        return (_anon32 === 1) ? [1] : [0];
                                                                      })(), _js_to_bosatsu_string("gcd(4, 3) == 1")), ((_a0, _a1) => [1,
                                                                      _a0,
                                                                      _a1])(((_a0, _a1) => [0,
                                                                        _a0,
                                                                        _a1])((() => {
                                                                          const _anon33 = _gcd((-4), (-3));
                                                                          return (_anon33 === (-1)) ? [1] : [0];
                                                                        })(), _js_to_bosatsu_string("gcd(-4, -3) == -1")), ((_a0, _a1) => [1,
                                                                        _a0,
                                                                        _a1])(((_a0, _a1) => [0,
                                                                          _a0,
                                                                          _a1])((() => {
                                                                            const _anon34 = _gcd(4, 2);
                                                                            return (_anon34 === 2) ? [1] : [0];
                                                                          })(), _js_to_bosatsu_string("gcd(4, 2) == 2")), ((_a0, _a1) => [1,
                                                                          _a0,
                                                                          _a1])(((_a0, _a1) => [0,
                                                                            _a0,
                                                                            _a1])((() => {
                                                                              const _anon35 = _int_to_String(0);
                                                                              return (_bosatsu_to_js_string(_anon35) === "0") ? [1] : [0];
                                                                            })(), _js_to_bosatsu_string("0 str")), ((_a0, _a1) => [1,
                                                                            _a0,
                                                                            _a1])(((_a0, _a1) => [0,
                                                                              _a0,
                                                                              _a1])((() => {
                                                                                const _anon36 = _int_to_String(123);
                                                                                return (_bosatsu_to_js_string(_anon36) === "123") ? [1] : [0];
                                                                              })(), _js_to_bosatsu_string("123 str")), ((_a0, _a1) => [1,
                                                                              _a0,
                                                                              _a1])(((_a0, _a1) => [0,
                                                                                _a0,
                                                                                _a1])((() => {
                                                                                  const _anon37 = _int_to_String((-123));
                                                                                  return (_bosatsu_to_js_string(_anon37) === "-123") ? [1] : [0];
                                                                                })(), _js_to_bosatsu_string("-123 str")), ((_a0, _a1) => [1,
                                                                                _a0,
                                                                                _a1])(((_a0, _a1) => [0,
                                                                                  _a0,
                                                                                  _a1])((() => {
                                                                                    const _anon38 = _string_to_Int(_js_to_bosatsu_string("123"));
                                                                                    return (() => {
                                                                                      let _anon39;
                                                                                      return (_anon38[0] === 1 && (() => {
                                                                                        _anon39 = _anon38[1];
                                                                                        return true;
                                                                                      })() && (_anon39 === 123)) ? [1] : [0];
                                                                                    })();
                                                                                  })(), _js_to_bosatsu_string("123 string_to_Int")), ((_a0, _a1) => [1,
                                                                                  _a0,
                                                                                  _a1])(((_a0, _a1) => [0,
                                                                                    _a0,
                                                                                    _a1])((() => {
                                                                                      const _anon40 = _string_to_Int(_js_to_bosatsu_string("-123"));
                                                                                      return (() => {
                                                                                        let _anon41;
                                                                                        return (_anon40[0] === 1 && (() => {
                                                                                          _anon41 = _anon40[1];
                                                                                          return true;
                                                                                        })() && (_anon41 === (-123))) ? [1] : [0];
                                                                                      })();
                                                                                    })(), _js_to_bosatsu_string("-123 string_to_Int")), ((_a0, _a1) => [1,
                                                                                    _a0,
                                                                                    _a1])(((_a0, _a1) => [0,
                                                                                      _a0,
                                                                                      _a1])((() => {
                                                                                        const _anon42 = _string_to_Int(_js_to_bosatsu_string("-123x"));
                                                                                        return (_anon42[0] === 0) ? [1] : [0];
                                                                                      })(), _js_to_bosatsu_string("-123x string_to_Int")), ((_a0, _a1) => [1,
                                                                                      _a0,
                                                                                      _a1])(((_a0, _a1) => [0,
                                                                                        _a0,
                                                                                        _a1])((() => {
                                                                                          const _anon43 = _string_to_Int(_concat_String(((_a0, _a1) => [1,
                                                                                                _a0,
                                                                                                _a1])(_js_to_bosatsu_string("-"), ((_a0, _a1) => [1,
                                                                                                  _a0,
                                                                                                  _a1])(_int_to_String(123), [0]))));
                                                                                          return (() => {
                                                                                            let _anon44;
                                                                                            return (_anon43[0] === 1 && (() => {
                                                                                              _anon44 = _anon43[1];
                                                                                              return true;
                                                                                            })() && (_anon44 === (-123))) ? [1] : [0];
                                                                                          })();
                                                                                        })(), _js_to_bosatsu_string("-123 string_to_Int")), ((_a0, _a1) => [1,
                                                                                        _a0,
                                                                                        _a1])(((_a0, _a1) => [0,
                                                                                          _a0,
                                                                                          _a1])((() => {
                                                                                            const _anon45 = _string_to_Int(_js_to_bosatsu_string("9223372036854775807"));
                                                                                            return (() => {
                                                                                              let _anon46;
                                                                                              return (_anon45[0] === 1 && (() => {
                                                                                                _anon46 = _anon45[1];
                                                                                                return true;
                                                                                              })() && (_anon46 === 9223372036854775807)) ? [1] : [0];
                                                                                            })();
                                                                                          })(), _concat_String(((_a0, _a1) => [1,
                                                                                              _a0,
                                                                                              _a1])(_js_to_bosatsu_string("Long.Max "), ((_a0, _a1) => [1,
                                                                                                _a0,
                                                                                                _a1])(PredefTests$oi(_string_to_Int(_js_to_bosatsu_string("9223372036854775807"))), ((_a0, _a1) => [1,
                                                                                                  _a0,
                                                                                                  _a1])(_js_to_bosatsu_string(" != "), ((_a0, _a1) => [1,
                                                                                                    _a0,
                                                                                                    _a1])(PredefTests$oi((_a0 => [1,
                                                                                                        _a0])(9223372036854775807)), ((_a0, _a1) => [1,
                                                                                                      _a0,
                                                                                                      _a1])(_js_to_bosatsu_string("string_to_Int"), [0]))))))), ((_a0, _a1) => [1,
                                                                                          _a0,
                                                                                          _a1])(((_a0, _a1) => [0,
                                                                                            _a0,
                                                                                            _a1])((() => {
                                                                                              const _anon47 = _string_to_Int(_js_to_bosatsu_string("9223372036854775808"));
                                                                                              return (() => {
                                                                                                let _anon48;
                                                                                                return (_anon47[0] === 1 && (() => {
                                                                                                  _anon48 = _anon47[1];
                                                                                                  return true;
                                                                                                })() && (_anon48 === 9223372036854775808)) ? [1] : [0];
                                                                                              })();
                                                                                            })(), _concat_String(((_a0, _a1) => [1,
                                                                                                _a0,
                                                                                                _a1])(_js_to_bosatsu_string("Long.Max + 1: "), ((_a0, _a1) => [1,
                                                                                                  _a0,
                                                                                                  _a1])(PredefTests$oi(_string_to_Int(_js_to_bosatsu_string("9223372036854775808"))), ((_a0, _a1) => [1,
                                                                                                    _a0,
                                                                                                    _a1])(_js_to_bosatsu_string(" string_to_Int"), [0]))))), ((_a0, _a1) => [1,
                                                                                            _a0,
                                                                                            _a1])(((_a0, _a1) => [0,
                                                                                              _a0,
                                                                                              _a1])((() => {
                                                                                                const _anon49 = _string_to_Int(_js_to_bosatsu_string("2147483647"));
                                                                                                return (() => {
                                                                                                  let _anon50;
                                                                                                  return (_anon49[0] === 1 && (() => {
                                                                                                    _anon50 = _anon49[1];
                                                                                                    return true;
                                                                                                  })() && (_anon50 === 2147483647)) ? [1] : [0];
                                                                                                })();
                                                                                              })(), _js_to_bosatsu_string("Int.Max string_to_Int")), ((_a0, _a1) => [1,
                                                                                              _a0,
                                                                                              _a1])(((_a0, _a1) => [0,
                                                                                                _a0,
                                                                                                _a1])((() => {
                                                                                                  const _anon51 = _string_to_Int(_js_to_bosatsu_string("2147483648"));
                                                                                                  return (() => {
                                                                                                    let _anon52;
                                                                                                    return (_anon51[0] === 1 && (() => {
                                                                                                      _anon52 = _anon51[1];
                                                                                                      return true;
                                                                                                    })() && (_anon52 === 2147483648)) ? [1] : [0];
                                                                                                  })();
                                                                                                })(), _concat_String(((_a0, _a1) => [1,
                                                                                                    _a0,
                                                                                                    _a1])(_js_to_bosatsu_string("Int.Max + 1 string_to_Int: "), ((_a0, _a1) => [1,
                                                                                                      _a0,
                                                                                                      _a1])(PredefTests$oi(_string_to_Int(_js_to_bosatsu_string("2147483648"))), ((_a0, _a1) => [1,
                                                                                                        _a0,
                                                                                                        _a1])(_js_to_bosatsu_string(" != "), ((_a0, _a1) => [1,
                                                                                                          _a0,
                                                                                                          _a1])(PredefTests$oi((_a0 => [1,
                                                                                                              _a0])(2147483648)), [0])))))), ((_a0, _a1) => [1,
                                                                                                _a0,
                                                                                                _a1])(((_a0, _a1) => [0,
                                                                                                  _a0,
                                                                                                  _a1])((() => {
                                                                                                    const _anon53 = _string_to_Int(_js_to_bosatsu_string("-9223372036854775808"));
                                                                                                    return (() => {
                                                                                                      let _anon54;
                                                                                                      return (_anon53[0] === 1 && (() => {
                                                                                                        _anon54 = _anon53[1];
                                                                                                        return true;
                                                                                                      })() && (_anon54 === (-9223372036854775808))) ? [1] : [0];
                                                                                                    })();
                                                                                                  })(), _js_to_bosatsu_string("Long.Min string_to_Int")), ((_a0, _a1) => [1,
                                                                                                  _a0,
                                                                                                  _a1])(((_a0, _a1) => [0,
                                                                                                    _a0,
                                                                                                    _a1])((() => {
                                                                                                      const _anon55 = _string_to_Int(_js_to_bosatsu_string("-9223372036854775809"));
                                                                                                      return (() => {
                                                                                                        let _anon56;
                                                                                                        return (_anon55[0] === 1 && (() => {
                                                                                                          _anon56 = _anon55[1];
                                                                                                          return true;
                                                                                                        })() && (_anon56 === (-9223372036854775809))) ? [1] : [0];
                                                                                                      })();
                                                                                                    })(), _js_to_bosatsu_string("Long.Min - 1 string_to_Int")), ((_a0, _a1) => [1,
                                                                                                    _a0,
                                                                                                    _a1])(((_a0, _a1) => [0,
                                                                                                      _a0,
                                                                                                      _a1])((() => {
                                                                                                        const _anon57 = _string_to_Int(_js_to_bosatsu_string("-2147483648"));
                                                                                                        return (() => {
                                                                                                          let _anon58;
                                                                                                          return (_anon57[0] === 1 && (() => {
                                                                                                            _anon58 = _anon57[1];
                                                                                                            return true;
                                                                                                          })() && (_anon58 === (-2147483648))) ? [1] : [0];
                                                                                                        })();
                                                                                                      })(), _js_to_bosatsu_string("Int.Min string_to_Int")), ((_a0, _a1) => [1,
                                                                                                      _a0,
                                                                                                      _a1])(((_a0, _a1) => [0,
                                                                                                        _a0,
                                                                                                        _a1])((() => {
                                                                                                          const _anon59 = _string_to_Int(_js_to_bosatsu_string("-2147483649"));
                                                                                                          return (() => {
                                                                                                            let _anon60;
                                                                                                            return (_anon59[0] === 1 && (() => {
                                                                                                              _anon60 = _anon59[1];
                                                                                                              return true;
                                                                                                            })() && (_anon60 === (-2147483649))) ? [1] : [0];
                                                                                                          })();
                                                                                                        })(), _js_to_bosatsu_string("Int.Min - 1 string_to_Int")), ((_a0, _a1) => [1,
                                                                                                        _a0,
                                                                                                        _a1])(((_a0, _a1) => [0,
                                                                                                          _a0,
                                                                                                          _a1])((() => {
                                                                                                            const _anon61 = _string_to_Int(_js_to_bosatsu_string("-2147483649z"));
                                                                                                            return (_anon61[0] === 0) ? [1] : [0];
                                                                                                          })(), _js_to_bosatsu_string("-2147483649z string_to_Int")), [0]))))))))))))))))))))))))))))))))))))))))))))))))))));
_tests["PredefTests::test_int"] = PredefTests$test_int;
var PredefTests$test_string = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("String tests"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
        const _anon62 = _partition_String(_js_to_bosatsu_string("foo"), _js_to_bosatsu_string("f"));
        return (() => {
          let _anon63;
          return (() => {
            let _anon65;
            return (() => {
              let _anon64;
              return (_anon62[0] === 1 && (() => {
                _anon63 = _anon62[1];
                return true;
              })() && ((() => {
                _anon65 = _anon63[1];
                return true;
              })() && ((() => {
                _anon64 = _anon63[0];
                return true;
              })() && (_bosatsu_to_js_string(_anon64) === "" && (_bosatsu_to_js_string(_anon65) === "oo"))))) ? [1] : [0];
            })();
          })();
        })();
      })(), _js_to_bosatsu_string("foo partition_String f")), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
          const _anon66 = _partition_String(_js_to_bosatsu_string("foo"), _js_to_bosatsu_string(""));
          return (_anon66[0] === 0) ? [1] : [0];
        })(), _js_to_bosatsu_string("foo partition_String \"\"")), ((_a0, _a1) => [1,
        _a0,
        _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
            const _anon67 = _partition_String(_js_to_bosatsu_string("foo"), _js_to_bosatsu_string("x"));
            return (_anon67[0] === 0) ? [1] : [0];
          })(), _js_to_bosatsu_string("foo partition_String x")), ((_a0, _a1) => [1,
          _a0,
          _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
              const _anon68 = _rpartition_String(_js_to_bosatsu_string("foo"), _js_to_bosatsu_string("o"));
              return (() => {
                let _anon69;
                return (() => {
                  let _anon71;
                  return (() => {
                    let _anon70;
                    return (_anon68[0] === 1 && (() => {
                      _anon69 = _anon68[1];
                      return true;
                    })() && ((() => {
                      _anon71 = _anon69[1];
                      return true;
                    })() && ((() => {
                      _anon70 = _anon69[0];
                      return true;
                    })() && (_bosatsu_to_js_string(_anon70) === "fo" && (_bosatsu_to_js_string(_anon71) === ""))))) ? [1] : [0];
                  })();
                })();
              })();
            })(), _js_to_bosatsu_string("foo rpartition_String o")), ((_a0, _a1) => [1,
            _a0,
            _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                const _anon72 = _rpartition_String(_js_to_bosatsu_string("foo"), _js_to_bosatsu_string(""));
                return (_anon72[0] === 0) ? [1] : [0];
              })(), _js_to_bosatsu_string("foo rpartition_String \"\"")), ((_a0, _a1) => [1,
              _a0,
              _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
                  const _anon73 = _rpartition_String(_js_to_bosatsu_string("foo"), _js_to_bosatsu_string("x"));
                  return (_anon73[0] === 0) ? [1] : [0];
                })(), _js_to_bosatsu_string("foo rpartition_String x")), [0])))))));
_tests["PredefTests::test_string"] = PredefTests$test_string;
var PredefTests$test = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("Predef tests"), ((_a0, _a1) => [1,
    _a0,
    _a1])(PredefTests$test_int, ((_a0, _a1) => [1,
      _a0,
      _a1])(PredefTests$test_string, [0])));
_tests["PredefTests::test"] = PredefTests$test;
var RecordSet_Library$list_of_rows = b => (() => {
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
var RecordSet_Library$restructure = (a, f) => (() => {
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
var RecordSet_Library$new_record_set = ((_a0, _a1, _a2, _a3, _a4) => [_a0,
  _a1,
  _a2,
  _a3,
  _a4])([], [0], [], a => a_1 => [], a_2 => [0]);
var RecordSet_Library$ps_end = ((_a0, _a1, _a2, _a3, _a4) => [_a0,
  _a1,
  _a2,
  _a3,
  _a4])(a => [], a_1 => [], [], a_2 => a_3 => [], a_4 => [0]);
var RecordSet_Library$ps = (c, d) => (() => {
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
var RecordSet_Library$equal_List = (is_equal, l1, l2) => (() => {
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
var RecordSet_Library$equal_RowEntry = (re1, re2) => (() => {
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
var RecordSet_Library$equal_rows = (a, b) => RecordSet_Library$equal_List(RecordSet_Library$equal_RowEntry, a, b);
var RecordSet_Library$rs_empty = RecordSet_Library$restructure(RecordSet_Library$new_record_set, a => RecordSet_Library$ps((() => {
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
var RecordSet_Library$rs = (() => {
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
var RecordSet_Library$rs0 = RecordSet_Library$restructure(RecordSet_Library$rs, d => (() => {
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
var RecordSet_Library$tests = ((_a0, _a1) => [1,
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
_tests["RecordSet/Library::tests"] = RecordSet_Library$tests;
var StrConcatExample$res0 = (() => {
  const _anon0 = _concat_String(((_a0, _a1) => [1,
      _a0,
      _a1])(_js_to_bosatsu_string("hello"), ((_a0, _a1) => [1,
        _a0,
        _a1])(_js_to_bosatsu_string(" "), ((_a0, _a1) => [1,
          _a0,
          _a1])(_js_to_bosatsu_string("atticus"), ((_a0, _a1) => [1,
            _a0,
            _a1])(_js_to_bosatsu_string(" and "), ((_a0, _a1) => [1,
              _a0,
              _a1])(_js_to_bosatsu_string("mahina"), [0]))))));
  return (_bosatsu_to_js_string(_anon0) === "hello atticus and mahina") ? [1] : [0];
})();
var StrConcatExample$res1 = (() => {
  const _anon1 = _concat_String(((_a0, _a1) => [1,
      _a0,
      _a1])(_js_to_bosatsu_string("hello"), ((_a0, _a1) => [1,
        _a0,
        _a1])(_js_to_bosatsu_string(" "), ((_a0, _a1) => [1,
          _a0,
          _a1])(_js_to_bosatsu_string("atticus"), ((_a0, _a1) => [1,
            _a0,
            _a1])(_js_to_bosatsu_string(" and "), ((_a0, _a1) => [1,
              _a0,
              _a1])(_js_to_bosatsu_string("sarah"), [0]))))));
  return (_bosatsu_to_js_string(_anon1) === "hello atticus and sarah") ? [1] : [0];
})();
var StrConcatExample$res3 = (() => {
  const _anon2 = _concat_String(((_a0, _a1) => [1,
      _a0,
      _a1])(_js_to_bosatsu_string("hello"), ((_a0, _a1) => [1,
        _a0,
        _a1])(_js_to_bosatsu_string(" "), ((_a0, _a1) => [1,
          _a0,
          _a1])(_js_to_bosatsu_string("atticus"), ((_a0, _a1) => [1,
            _a0,
            _a1])(_js_to_bosatsu_string(" and "), ((_a0, _a1) => [1,
              _a0,
              _a1])(_js_to_bosatsu_string("mahina"), [0]))))));
  return (() => {
    let _anon3;
    return (() => {
      const _str = _bosatsu_to_js_string(_anon2);
      return _str.startsWith("hello ") && (() => {
        _anon3 = _js_to_bosatsu_string(_str.substring(0 + 6));
        return true;
      })();
    })() ? (() => {
        const rest = _anon3;
        return (() => {
          const _str = _bosatsu_to_js_string(rest);
          return _str.startsWith("atticus") && false;
        })() ? [1] : [0];
      })() : [0];
  })();
})();
var StrConcatExample$res4 = (() => {
  const _anon7 = _concat_String(((_a0, _a1) => [1,
      _a0,
      _a1])(_js_to_bosatsu_string("hello"), ((_a0, _a1) => [1,
        _a0,
        _a1])(_js_to_bosatsu_string(" "), ((_a0, _a1) => [1,
          _a0,
          _a1])(_js_to_bosatsu_string("atticus"), ((_a0, _a1) => [1,
            _a0,
            _a1])(_js_to_bosatsu_string(" and "), ((_a0, _a1) => [1,
              _a0,
              _a1])(_js_to_bosatsu_string("mahina"), [0]))))));
  return (() => {
    let _anon8;
    return (() => {
      let _anon9;
      return (() => {
        const _str = _bosatsu_to_js_string(_anon7);
        return false;
      })() ? (() => {
          const left = _anon8;
          return (() => {
            const right = _anon9;
            return (() => {
              const _anon4 = ((_a0, _a1) => [_a0, _a1])(left, right);
              return (() => {
                let _anon6;
                return (() => {
                  let _anon5;
                  return ((() => {
                    _anon6 = _anon4[1];
                    return true;
                  })() && ((() => {
                    _anon5 = _anon4[0];
                    return true;
                  })() && (_bosatsu_to_js_string(_anon5) === "hello " && (_bosatsu_to_js_string(_anon6) === " and mahina")))) ? [1] : [0];
                })();
              })();
            })();
          })();
        })() : [0];
    })();
  })();
})();
var StrConcatExample$test = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("interpolation"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0,
      _a0,
      _a1])(StrConcatExample$res0, _js_to_bosatsu_string("res0")), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1) => [0,
        _a0,
        _a1])(StrConcatExample$res1, _js_to_bosatsu_string("res1")), ((_a0, _a1) => [1,
        _a0,
        _a1])(((_a0, _a1) => [0,
          _a0,
          _a1])([1], _js_to_bosatsu_string("res2")), ((_a0, _a1) => [1,
          _a0,
          _a1])(((_a0, _a1) => [0,
            _a0,
            _a1])(StrConcatExample$res3, _js_to_bosatsu_string("res3")), ((_a0, _a1) => [1,
            _a0,
            _a1])(((_a0, _a1) => [0,
              _a0,
              _a1])(StrConcatExample$res4, _js_to_bosatsu_string("res4")), [0]))))));
_tests["StrConcatExample::test"] = StrConcatExample$test;
var TreeList$foldTree = (t, init, fn) => (() => {
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
var TreeList$empty = (_a0 => [_a0])([0]);
var TreeList$cons = (head, a) => (() => {
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
var TreeList$decons = a => (() => {
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
var TreeList$head = tl => (() => {
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
var TreeList$get = (b, idx) => (() => {
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
var TreeList$fold = (a, init, fn) => (() => {
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
var TreeList$to_List = list => Bosatsu_Predef$reverse(TreeList$fold(list, [0], (l, h) => ((_a0, _a1) => [1,
      _a0,
      _a1])(h, l)));
var TreeList$eq_TreeList = fn => (_slots => (a, b) => (() => {
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
var TreeList$eq_ti = TreeList$eq_TreeList((_a0, _a1) => (_a0 === _a1) ? [1] : [0]);
var TreeList$tl12 = TreeList$cons(2, TreeList$cons(1, TreeList$empty));
var TreeList$list14 = (() => {
  const list = ((_a0, _a1) => [1, _a0, _a1])(1, ((_a0, _a1) => [1,
      _a0,
      _a1])(2, ((_a0, _a1) => [1, _a0, _a1])(3, ((_a0, _a1) => [1,
          _a0,
          _a1])(4, [0]))));
  return foldl_List(Bosatsu_Predef$reverse(list), TreeList$empty, (lst, h) => TreeList$cons(h, lst));
})();
var TreeList$cons14 = TreeList$cons(1, TreeList$cons(2, TreeList$cons(3, TreeList$cons(4, TreeList$empty))));
var TreeList$tests = ((_a0, _a1) => [1,
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
_tests["TreeList::tests"] = TreeList$tests;
var TypeConstraint$refl = (_a0 => [_a0])(x => x);
var TypeConstraint$refl_sub = (_a0 => [_a0])(x => x);
var TypeConstraint$refl_sup = (_a0 => [_a0])(x => x);
var TypeConstraint$substitute = (eq, fa) => (() => {
  const cast = eq[0];
  return cast(fa);
})();
var TypeConstraint$widen = (s, fa) => (() => {
  const sub = s[0];
  return sub(fa);
})();
var TypeConstraint$narrow = (s, fa) => (() => {
  const sup = s[0];
  return sup(fa);
})();
var TypeConstraint$sub_to_sup = sub => (() => {
  const _anon0 = (() => {
    const sub0 = sub[0];
    return sub0((_a0 => [_a0])(TypeConstraint$refl_sup));
  })();
  return _anon0[0];
})();
var TypeConstraint$sup_to_sub = sup => (() => {
  const _anon1 = (() => {
    const sup0 = sup[0];
    return sup0((_a0 => [_a0])(TypeConstraint$refl_sub));
  })();
  return _anon1[0];
})();
var TypeConstraint$eq_to_sub = eq => (() => {
  const cast = eq[0];
  return cast(TypeConstraint$refl_sub);
})();
var TypeConstraint$eq_to_sup = eq => (() => {
  const cast = eq[0];
  return cast(TypeConstraint$refl_sup);
})();
var TypeConstraint$cast = (s, a) => (() => {
  const _anon2 = (() => {
    const cast = s[0];
    return cast((_a0 => [_a0])(a));
  })();
  return _anon2[0];
})();
var TypeConstraint$upcast = (s, a) => (() => {
  const _anon3 = (() => {
    const sub = s[0];
    return sub((_a0 => [_a0])(a));
  })();
  return _anon3[0];
})();
var TypeConstraint$downcast = (s, a) => (() => {
  const _anon6 = (() => {
    const _anon5 = (() => {
      const _anon4 = (() => {
        const sup0 = s[0];
        return sup0((_a0 => [_a0])(TypeConstraint$refl_sub));
      })();
      return _anon4[0];
    })();
    return (() => {
      const sub = _anon5[0];
      return sub((_a0 => [_a0])(a));
    })();
  })();
  return _anon6[0];
})();
var TypeConstraint$compose_sub = (first, second) => (() => {
  const sub = second[0];
  return sub(first);
})();
var TypeConstraint$compose_sup = (first, second) => (() => {
  const sup = second[0];
  return sup(first);
})();
var TypeConstraint$flip_eq = eq => (() => {
  const _anon7 = (() => {
    const cast = eq[0];
    return cast((_a0 => [_a0])(TypeConstraint$refl));
  })();
  return _anon7[0];
})();
var TypeConstraint$compose_eq = (first, second) => (() => {
  const cast = second[0];
  return cast(first);
})();
var TypeConstraint$ignore = ((_a0, _a1, _a2, _a3, _a4, _a5) => [_a0,
  _a1,
  _a2,
  _a3,
  _a4,
  _a5])(TypeConstraint$refl_sub, TypeConstraint$refl_sub, TypeConstraint$refl_sub, TypeConstraint$refl_sub, TypeConstraint$refl_sub, TypeConstraint$refl_sub);
var BazelDepsApi$maven_central = ((_a0, _a1, _a2) => [_a0,
  _a1,
  _a2])(_js_to_bosatsu_string("mavencentral"), _js_to_bosatsu_string("default"), _js_to_bosatsu_string("https://repo.maven.apache.org/maven2/"));
var BazelDepsApi$default_options_with_scala = ((_a0, _a1, _a2, _a3, _a4, _a5) => [_a0,
  _a1,
  _a2,
  _a3,
  _a4,
  _a5])(((_a0, _a1) => [1,
    _a0,
    _a1])(_js_to_bosatsu_string("load(\"@io_bazel_rules_scala//scala:scala_import.bzl\", \"scala_import\")"), [0]), ((_a0, _a1) => [1,
    _a0,
    _a1])(_js_to_bosatsu_string("java"), ((_a0, _a1) => [1,
      _a0,
      _a1])(_js_to_bosatsu_string("scala:2.11.11"), [0])), ((_a0, _a1) => [1,
    _a0,
    _a1])(BazelDepsApi$maven_central, [0]), _js_to_bosatsu_string("runtime_deps"), _js_to_bosatsu_string("coursier"), _js_to_bosatsu_string("highest"));
var BazelDepsApi$scala_dep = (orgname, artifact, version) => Bosatsu_Predef$add_key(Bosatsu_Predef$empty_Dict(Bosatsu_Predef$string_Order), orgname, Bosatsu_Predef$add_key(Bosatsu_Predef$empty_Dict(Bosatsu_Predef$string_Order), artifact, ((_a0, _a1, _a2, _a3) => [_a0,
      _a1,
      _a2,
      _a3])([0], _js_to_bosatsu_string("scala"), version, [0])));
var BazelDepsApi$scala_dep_modules = (orgname, artifact, modules, version, _exports) => Bosatsu_Predef$add_key(Bosatsu_Predef$empty_Dict(Bosatsu_Predef$string_Order), orgname, Bosatsu_Predef$add_key(Bosatsu_Predef$empty_Dict(Bosatsu_Predef$string_Order), artifact, ((_a0, _a1, _a2, _a3) => [_a0,
      _a1,
      _a2,
      _a3])(modules, _js_to_bosatsu_string("scala"), version, _exports)));
var BazelDepsApi$merge_artifact = (left, right) => (() => {
  const lm = left[0];
  return (() => {
    const le = left[3];
    return (() => {
      const rm = right[0];
      return (() => {
        const rr = right[1];
        return (() => {
          const rv = right[2];
          return (() => {
            const re = right[3];
            return ((_a0, _a1, _a2, _a3) => [_a0,
              _a1,
              _a2,
              _a3])(Bosatsu_Predef$concat(lm, rm), rr, rv, Bosatsu_Predef$concat(le, re));
          })();
        })();
      })();
    })();
  })();
})();
var BazelDepsApi$merge_deps = (left, right) => DictTools$merge(left, right, (l, r) => DictTools$merge(l, r, BazelDepsApi$merge_artifact));
var BazelDepsApi$merge_dep_List = deps => foldl_List(deps, Bosatsu_Predef$empty_Dict(Bosatsu_Predef$string_Order), BazelDepsApi$merge_deps);
var BazelDepsApi$standard_scala_replacements = Bosatsu_Predef$add_key(Bosatsu_Predef$add_key(Bosatsu_Predef$empty_Dict(Bosatsu_Predef$string_Order), _js_to_bosatsu_string("org.scala-lang.modules"), Bosatsu_Predef$add_key(Bosatsu_Predef$add_key(Bosatsu_Predef$empty_Dict(Bosatsu_Predef$string_Order), _js_to_bosatsu_string("scala-xml"), ((_a0, _a1) => [_a0,
          _a1])(_js_to_bosatsu_string("scala"), _js_to_bosatsu_string("@io_bazel_rules_scala_scala_xml//:io_bazel_rules_scala_scala_xml"))), _js_to_bosatsu_string("scala-parser-combinators"), ((_a0, _a1) => [_a0,
        _a1])(_js_to_bosatsu_string("scala"), _js_to_bosatsu_string("@io_bazel_rules_scala_scala_parser_combinators//:io_bazel_rules_scala_scala_parser_combinators")))), _js_to_bosatsu_string("org.scala-lang"), Bosatsu_Predef$add_key(Bosatsu_Predef$add_key(Bosatsu_Predef$add_key(Bosatsu_Predef$empty_Dict(Bosatsu_Predef$string_Order), _js_to_bosatsu_string("scala-reflect"), ((_a0, _a1) => [_a0,
          _a1])(_js_to_bosatsu_string("scala/unmangled"), _js_to_bosatsu_string("@io_bazel_rules_scala_scala_reflect//:io_bazel_rules_scala_scala_reflect"))), _js_to_bosatsu_string("scala-library"), ((_a0, _a1) => [_a0,
        _a1])(_js_to_bosatsu_string("scala/unmangled"), _js_to_bosatsu_string("@io_bazel_rules_scala_scala_library//:io_bazel_rules_scala_scala_library"))), _js_to_bosatsu_string("scala-compiler"), ((_a0, _a1) => [_a0,
      _a1])(_js_to_bosatsu_string("scala/unmangled"), _js_to_bosatsu_string("@io_bazel_rules_scala_scala_compiler//:io_bazel_rules_scala_scala_compiler"))));
var Bosatsu_BinNat$is_even = b => (b[0] === 0) ? [1] : (b[0] === 2) ? [1] : [0];
var Bosatsu_BinNat$toInt = b => (b[0] === 0) ? 0 : (b[0] === 1) ? (() => {
      const n = b[1];
      return 1 + (Bosatsu_BinNat$toInt(n) + Bosatsu_BinNat$toInt(n));
    })() : (() => {
      const n_1 = b[1];
      return 2 + (Bosatsu_BinNat$toInt(n_1) + Bosatsu_BinNat$toInt(n_1));
    })();
var Bosatsu_BinNat$toNat = b => (b[0] === 0) ? 0 : (b[0] === 1) ? (() => {
      const n = b[1];
      return (n => n + 1)(Bosatsu_Nat$times2(Bosatsu_BinNat$toNat(n)));
    })() : (() => {
      const n_1 = b[1];
      return (n => n + 1)((n => n + 1)(Bosatsu_Nat$times2(Bosatsu_BinNat$toNat(n_1))));
    })();
var Bosatsu_BinNat$toBinNat = n => (() => {
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
var Bosatsu_BinNat$cmp_BinNat = (a, b) => (a[0] === 1) ? (() => {
    const a1 = a[1];
    return (b[0] === 1) ? (() => {
        const b1 = b[1];
        return Bosatsu_BinNat$cmp_BinNat(a1, b1);
      })() : (b[0] === 2) ? (() => {
          const b1_1 = b[1];
          return (() => {
            const _anon2 = Bosatsu_BinNat$cmp_BinNat(a1, b1_1);
            return (_anon2[0] === 0) ? [0] : (_anon2[0] === 1) ? [0] : [2];
          })();
        })() : [2];
  })() : (a[0] === 2) ? (() => {
      const a1_1 = a[1];
      return (b[0] === 2) ? (() => {
          const b1_2 = b[1];
          return Bosatsu_BinNat$cmp_BinNat(a1_1, b1_2);
        })() : (b[0] === 1) ? (() => {
            const b1_3 = b[1];
            return (() => {
              const _anon3 = Bosatsu_BinNat$cmp_BinNat(a1_1, b1_3);
              return (_anon3[0] === 2) ? [2] : (_anon3[0] === 1) ? [2] : [0];
            })();
          })() : [2];
    })() : (b[0] === 1) ? [0] : (b[0] === 2) ? [0] : [1];
var Bosatsu_BinNat$eq_BinNat = (a, b) => (() => {
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
var Bosatsu_BinNat$next = b => (b[0] === 1) ? (() => {
    const half = b[1];
    return (_a0 => [2, _a0])(half);
  })() : (b[0] === 2) ? (() => {
      const half1 = b[1];
      return (_a0 => [1, _a0])(Bosatsu_BinNat$next(half1));
    })() : (_a0 => [1, _a0])([0]);
var Bosatsu_BinNat$prev = b => (b[0] === 0) ? [0] : (() => {
    let _anon10;
    return (b[0] === 1 && (() => {
      _anon10 = b[1];
      return true;
    })() && (_anon10[0] === 0)) ? [0] : (b[0] === 1) ? (() => {
          const half = b[1];
          return (_a0 => [2, _a0])(Bosatsu_BinNat$prev(half));
        })() : (() => {
          const half1 = b[1];
          return (_a0 => [1, _a0])(half1);
        })();
  })();
var Bosatsu_BinNat$add_BinNat = (left, right) => (left[0] === 1) ? (() => {
    const odd = left;
    return (() => {
      const left_1 = left[1];
      return (right[0] === 1) ? (() => {
          const right_1 = right[1];
          return (_a0 => [2, _a0])(Bosatsu_BinNat$add_BinNat(left_1, right_1));
        })() : (right[0] === 2) ? (() => {
            const right_2 = right[1];
            return (_a0 => [1,
              _a0])(Bosatsu_BinNat$add_BinNat(left_1, Bosatsu_BinNat$next(right_2)));
          })() : odd;
    })();
  })() : (left[0] === 2) ? (() => {
      const even = left;
      return (() => {
        const left_2 = left[1];
        return (right[0] === 1) ? (() => {
            const right_3 = right[1];
            return (_a0 => [1,
              _a0])(Bosatsu_BinNat$add_BinNat(left_2, Bosatsu_BinNat$next(right_3)));
          })() : (right[0] === 2) ? (() => {
              const right_4 = right[1];
              return (_a0 => [2,
                _a0])(Bosatsu_BinNat$add_BinNat(left_2, Bosatsu_BinNat$next(right_4)));
            })() : even;
      })();
    })() : right;
var Bosatsu_BinNat$times2 = b => (b[0] === 1) ? (() => {
    const n = b[1];
    return (_a0 => [2, _a0])(Bosatsu_BinNat$times2(n));
  })() : (b[0] === 2) ? (() => {
      const n_1 = b[1];
      return (_a0 => [2, _a0])((_a0 => [1, _a0])(n_1));
    })() : [0];
var Bosatsu_BinNat$doub_prev = b => (b[0] === 1) ? (() => {
    const n = b[1];
    return (_a0 => [1, _a0])((_a0 => [1, _a0])(Bosatsu_BinNat$times2(n)));
  })() : (b[0] === 2) ? (() => {
      const n_1 = b[1];
      return (_a0 => [1, _a0])((_a0 => [1, _a0])((_a0 => [1, _a0])(n_1)));
    })() : [0];
var Bosatsu_BinNat$sub_Option = (left, right) => (left[0] === 0) ? (right[0] === 0) ? (_a0 => [1,
      _a0])([0]) : [0] : (left[0] === 1) ? (() => {
      const odd = left;
      return (() => {
        const left_1 = left[1];
        return (right[0] === 0) ? (_a0 => [1,
            _a0])(odd) : (right[0] === 1) ? (() => {
              const right_1 = right[1];
              return (() => {
                const _anon11 = Bosatsu_BinNat$sub_Option(left_1, right_1);
                return (_anon11[0] === 1) ? (() => {
                    const n_m = _anon11[1];
                    return (_a0 => [1, _a0])(Bosatsu_BinNat$times2(n_m));
                  })() : [0];
              })();
            })() : (() => {
              const right_2 = right[1];
              return (() => {
                const _anon12 = Bosatsu_BinNat$sub_Option(left_1, right_2);
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
                const _anon13 = Bosatsu_BinNat$sub_Option(left_2, right_3);
                return (_anon13[0] === 1) ? (() => {
                    const n_m_2 = _anon13[1];
                    return (_a0 => [1, _a0])((_a0 => [1, _a0])(n_m_2));
                  })() : [0];
              })();
            })() : (() => {
              const right_4 = right[1];
              return (() => {
                const _anon14 = Bosatsu_BinNat$sub_Option(left_2, right_4);
                return (_anon14[0] === 1) ? (() => {
                    const n_m_3 = _anon14[1];
                    return (_a0 => [1, _a0])(Bosatsu_BinNat$times2(n_m_3));
                  })() : [0];
              })();
            })();
      })();
    })();
var Bosatsu_BinNat$sub_BinNat = (left, right) => (() => {
  const _anon15 = Bosatsu_BinNat$sub_Option(left, right);
  return (_anon15[0] === 1) ? _anon15[1] : [0];
})();
var Bosatsu_BinNat$div2 = b => (b[0] === 0) ? [0] : (b[0] === 1) ? b[1] : (() => {
      const n = b[1];
      return Bosatsu_BinNat$next(n);
    })();
var Bosatsu_BinNat$times_BinNat = (left, right) => (left[0] === 0) ? [0] : (left[0] === 1) ? (() => {
      const left_1 = left[1];
      return (right[0] === 0) ? [0] : (() => {
          const right_1 = right;
          return Bosatsu_BinNat$add_BinNat(Bosatsu_BinNat$times2(Bosatsu_BinNat$times_BinNat(left_1, right_1)), right_1);
        })();
    })() : (() => {
      const left_2 = left[1];
      return (right[0] === 0) ? [0] : (() => {
          const right_2 = right;
          return Bosatsu_BinNat$times2(Bosatsu_BinNat$add_BinNat(Bosatsu_BinNat$times_BinNat(left_2, right_2), right_2));
        })();
    })();
var Bosatsu_BinNat$one = (_a0 => [1, _a0])([0]);
var Bosatsu_BinNat$divmod = (numerator, divisor) => (() => {
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
var Bosatsu_BinNat$exp = (base, power) => (power[0] === 0) ? Bosatsu_BinNat$one : (power[0] === 1) ? (() => {
      const n = power[1];
      return (() => {
        const bn = Bosatsu_BinNat$exp(base, n);
        return Bosatsu_BinNat$times_BinNat(Bosatsu_BinNat$times_BinNat(bn, bn), base);
      })();
    })() : (() => {
      const n_1 = power[1];
      return (() => {
        const bn1 = Bosatsu_BinNat$times_BinNat(Bosatsu_BinNat$exp(base, n_1), base);
        return Bosatsu_BinNat$times_BinNat(bn1, bn1);
      })();
    })();
var Bosatsu_BinNat$fold_left_BinNat = (fn, init, cnt) => (() => {
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
var Bosatsu_BinNat$fib = b => (() => {
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
var Bosatsu_BinNat$next_law = (i, msg) => ((_a0, _a1) => [0,
  _a0,
  _a1])((Bosatsu_BinNat$toInt(Bosatsu_BinNat$next(Bosatsu_BinNat$toBinNat(i))) === (1 + i)) ? [1] : [0], msg);
var Bosatsu_BinNat$times2_law = (i, msg) => ((_a0, _a1) => [0,
  _a0,
  _a1])((Bosatsu_BinNat$toInt(Bosatsu_BinNat$times2(Bosatsu_BinNat$toBinNat(i))) === (i + i)) ? [1] : [0], msg);
var Bosatsu_BinNat$two = Bosatsu_BinNat$next(Bosatsu_BinNat$one);
var Bosatsu_BinNat$three = Bosatsu_BinNat$next(Bosatsu_BinNat$two);
var Bosatsu_BinNat$four = Bosatsu_BinNat$next(Bosatsu_BinNat$three);
var Bosatsu_BinNat$test = ((_a0, _a1) => [1,
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
_tests["Bosatsu/BinNat::test"] = Bosatsu_BinNat$test;
var Bosatsu_FibBench$fib_Nat = n => (n === 0) ? 1 : (() => {
    let _anon0;
    return (n > 0 && (() => {
      _anon0 = n - 1;
      return true;
    })() && (_anon0 === 0)) ? 1 : (() => {
        let _anon1;
        return (() => {
          let _anon2;
          return (() => {
            (() => {
              _anon1 = n - 1;
              return true;
            })() && (() => {
              _anon2 = _anon1 - 1;
              return true;
            })();
            return (() => {
              const prev = _anon1;
              return (() => {
                const p1 = _anon2;
                return Bosatsu_FibBench$fib_Nat(prev) + Bosatsu_FibBench$fib_Nat(p1);
              })();
            })();
          })();
        })();
      })();
  })();
var Bosatsu_FibBench$print_fib = str => (() => {
  const _anon3 = _string_to_Int(str);
  return (_anon3[0] === 1) ? (() => {
      const i = _anon3[1];
      return Bosatsu_Prog$println(_concat_String(((_a0, _a1) => [1,
            _a0,
            _a1])(_js_to_bosatsu_string("fib("), ((_a0, _a1) => [1,
              _a0,
              _a1])(str, ((_a0, _a1) => [1,
                _a0,
                _a1])(_js_to_bosatsu_string(") = "), ((_a0, _a1) => [1,
                  _a0,
                  _a1])(_int_to_String(Bosatsu_FibBench$fib_Nat(Bosatsu_Nat$to_Nat(i))), [0]))))));
    })() : Bosatsu_Prog$println(_concat_String(((_a0, _a1) => [1,
          _a0,
          _a1])(_js_to_bosatsu_string("could not parse "), ((_a0, _a1) => [1,
            _a0,
            _a1])(str, [0]))));
})();
var Bosatsu_FibBench$list_len = (lst, acc) => (() => {
  let _anon4;
  return (() => {
    let _anon5;
    return (() => {
      let _anon7;
      return (() => {
        let _anon9;
        return (() => {
          (() => {
            _anon7 = lst;
            return true;
          })();
          return (() => {
            (() => {
              _anon9 = acc;
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
                          _anon5 = _anon9;
                          return true;
                        })();
                        return [];
                      })();
                    })() : (() => {
                      const tail = _anon7[2];
                      return (() => {
                        const _anon6 = tail;
                        return (() => {
                          const _anon8 = 1 + _anon9;
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
var Bosatsu_FibBench$_a = args => (() => {
  let _anon10;
  return (() => {
    let _anon11;
    return (args[0] === 1 && (() => {
      _anon10 = args[2];
      return true;
    })() && (_anon10[0] === 1 && (() => {
      _anon11 = _anon10[2];
      return true;
    })() && (_anon11[0] === 0))) ? (() => {
        const n = _anon10[1];
        return Bosatsu_FibBench$print_fib(n);
      })() : Bosatsu_Prog$println(_concat_String(((_a0, _a1) => [1,
            _a0,
            _a1])(_js_to_bosatsu_string("expected exactly one arg, got: "), ((_a0, _a1) => [1,
              _a0,
              _a1])(_int_to_String(Bosatsu_FibBench$list_len(args, 0)), [0]))));
  })();
})();
var Bosatsu_FibBench$main = (_a0 => [_a0])(Bosatsu_Prog$_await(Bosatsu_Prog$read_env)(args => Bosatsu_Prog$_await(Bosatsu_Prog$ignore_env(Bosatsu_FibBench$_a(args)))(a => Bosatsu_Prog$pure(0))));
var Bosatsu_List$any = as => (() => {
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
var Bosatsu_List$for_all = (xs, fn) => (() => {
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
var Bosatsu_List$sum = as => foldl_List(as, 0, (_a0, _a1) => _a0 + _a1);
var Bosatsu_List$exists = (xs, fn) => (() => {
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
var Bosatsu_List$uncons = xs => (xs[0] === 0) ? [0] : (() => {
    const h = xs[1];
    return (() => {
      const t = xs[2];
      return (_a0 => [1, _a0])(((_a0, _a1) => [_a0, _a1])(h, t));
    })();
  })();
var Bosatsu_List$head = xs => (xs[0] === 0) ? [0] : (() => {
    const h = xs[1];
    return (_a0 => [1, _a0])(h);
  })();
var Bosatsu_List$eq_List = fn => (_slots => (a, b) => (a[0] === 0) ? (b[0] === 0) ? [1] : [0] : (() => {
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
  })())([Bosatsu_List$eq_List, fn]);
var Bosatsu_List$zip = (left, right) => (left[0] === 0) ? [0] : (() => {
    const ah = left[1];
    return (() => {
      const at = left[2];
      return (right[0] === 0) ? [0] : (() => {
          const bh = right[1];
          return (() => {
            const bt = right[2];
            return ((_a0, _a1) => [1, _a0, _a1])(((_a0, _a1) => [_a0,
                _a1])(ah, bh), Bosatsu_List$zip(at, bt));
          })();
        })();
    })();
  })();
var Bosatsu_List$size1 = (list, acc) => (() => {
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
var Bosatsu_List$size = list => Bosatsu_List$size1(list, 0);
var Bosatsu_List$sort = (ord, list) => (() => {
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
var Bosatsu_List$op_59984 = Bosatsu_List$eq_List((_a0, _a1) => (_a0 === _a1) ? [1] : [0]);
var Bosatsu_List$headTest = ((_a0, _a1) => [0, _a0, _a1])((() => {
    const _anon31 = (_a0 => [1, _a0])(1);
    return (() => {
      let _anon32;
      return (_anon31[0] === 1 && (() => {
        _anon32 = _anon31[1];
        return true;
      })() && (_anon32 === 1)) ? [1] : [0];
    })();
  })(), _js_to_bosatsu_string("head test"));
var Bosatsu_List$unconsTest = ((_a0, _a1) => [0, _a0, _a1])((() => {
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
var Bosatsu_List$zipTest = ((_a0, _a1) => [1,
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
var Bosatsu_List$sortTest = ((_a0, _a1) => [1,
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
var Bosatsu_List$stringTests = ((_a0, _a1) => [1,
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
var Bosatsu_List$il = _int_loop(5, 0, (i, a) => ((_a0, _a1) => [_a0, _a1])((-1) + i, i + a));
var Bosatsu_List$tests = ((_a0, _a1) => [1,
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
_tests["Bosatsu/List::tests"] = Bosatsu_List$tests;
var BuildExample$lib1 = BuildLibrary$library(BuildLibrary$files(((_a0, _a1) => [1,
      _a0,
      _a1])(_js_to_bosatsu_string("file1.bosatsu"), ((_a0, _a1) => [1,
        _a0,
        _a1])(_js_to_bosatsu_string("file2.bosatsu"), [0]))), BuildLibrary$empty);
var BuildExample$lib2 = BuildLibrary$build(((_a0, _a1) => [_a0,
    _a1])(BuildLibrary$files([0]), BuildLibrary$build_all(((_a0, _a1) => [1,
        _a0,
        _a1])(BuildExample$lib1, [0]))));
var Eval$done = _a0 => [0, _a0];
var Eval$flat_map = (e, fn) => (_a0 => [1, _a0])((_slots => cb => cb(_slots[0], _slots[1]))([e,
      fn]));
var Eval$bind = e => (_slots => fn => (_a0 => [1,
  _a0])((_slots => cb => cb(_slots[0], _slots[1]))([_slots[0], fn])))([e]);
var Eval$map = (e, fn) => (_a0 => [1,
  _a0])((_slots => cb => cb(_slots[0], (_slots => x => (_a0 => [0,
      _a0])(_slots[0](x)))([_slots[1]])))([e, fn]));
var Eval$run = (budget, arg) => (budget === 0) ? [0] : (() => {
    let _anon4;
    return (() => {
      (() => {
        _anon4 = budget - 1;
        return true;
      })();
      return (() => {
        const balance = _anon4;
        return (() => {
          let _anon0;
          return (arg[0] === 0 && (() => {
            _anon0 = arg[2];
            return true;
          })() && (_anon0[0] === 0)) ? (() => {
              const a = arg[1];
              return (() => {
                const fn = _anon0[1];
                return (_a0 => [1, _a0])(fn(a));
              })();
            })() : (() => {
              let _anon1;
              return (arg[0] === 1 && (() => {
                _anon1 = arg[1];
                return true;
              })() && (_anon1[0] === 0)) ? (() => {
                  const a_1 = _anon1[1];
                  return (() => {
                    const stack = arg[2];
                    return Eval$run(balance, ((_a0, _a1) => [0,
                        _a0,
                        _a1])(a_1, stack));
                  })();
                })() : (() => {
                  let _anon2;
                  return (arg[0] === 1 && (() => {
                    _anon2 = arg[1];
                    return true;
                  })() && (_anon2[0] === 1)) ? (() => {
                      const use = _anon2[1];
                      return (() => {
                        const stack_1 = arg[2];
                        return use((_slots => (prev, fn_1) => _slots[0](_slots[1], ((_a0, _a1) => [1,
                              _a0,
                              _a1])(prev, (_a0 => [1,
                                _a0])((_slots => use_1 => use_1(_slots[0], _slots[1]))([fn_1,
                                    _slots[2]])))))([Eval$run,
                              balance,
                              stack_1]));
                      })();
                    })() : (() => {
                      let _anon3;
                      return (() => {
                        (() => {
                          _anon3 = arg[2];
                          return true;
                        })();
                        return (() => {
                          const a_2 = arg[1];
                          return (() => {
                            const use_2 = _anon3[1];
                            return use_2((_slots => (fn_2, stack_2) => _slots[0](_slots[1], ((_a0, _a1) => [1,
                                  _a0,
                                  _a1])(fn_2(_slots[2]), stack_2)))([Eval$run,
                                  balance,
                                  a_2]));
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
var Eval$_eval = (budget, ea) => Eval$run(Bosatsu_Nat$to_Nat(budget), ((_a0, _a1) => [1,
    _a0,
    _a1])(ea, (_a0 => [0, _a0])(a => a)));
var Bosatsu_BinInt$cmp = (a, b) => (a[0] === 0) ? (() => {
    const a_1 = a[1];
    return (b[0] === 0) ? (() => {
        const b_1 = b[1];
        return Bosatsu_BinNat$cmp_BinNat(a_1, b_1);
      })() : [2];
  })() : (() => {
    const a_2 = a[1];
    return (b[0] === 1) ? (() => {
        const b_2 = b[1];
        return Bosatsu_BinNat$cmp_BinNat(b_2, a_2);
      })() : [0];
  })();
var Bosatsu_BinInt$eq = (a, b) => (a[0] === 0) ? (() => {
    const a_1 = a[1];
    return (b[0] === 0) ? (() => {
        const b_1 = b[1];
        return Bosatsu_BinNat$eq_BinNat(a_1, b_1);
      })() : [0];
  })() : (() => {
    const a_2 = a[1];
    return (b[0] === 1) ? (() => {
        const b_2 = b[1];
        return Bosatsu_BinNat$eq_BinNat(a_2, b_2);
      })() : [0];
  })();
var Bosatsu_BinInt$binNat_to_BinInt = _a0 => [0, _a0];
var Bosatsu_BinInt$not = bi => (bi[0] === 0) ? (() => {
    const b = bi[1];
    return (_a0 => [1, _a0])(b);
  })() : (() => {
    const b_1 = bi[1];
    return (_a0 => [0, _a0])(b_1);
  })();
var Bosatsu_BinInt$int_to_BinInt = i => (() => {
  const _anon0 = (i < 0) ? [0] : (i === 0) ? [1] : [2];
  return (_anon0[0] === 0) ? (_a0 => [1,
      _a0])(Bosatsu_BinNat$toBinNat(~i)) : (_a0 => [0,
      _a0])(Bosatsu_BinNat$toBinNat(i));
})();
var Bosatsu_BinInt$binInt_to_Int = bi => (bi[0] === 0) ? (() => {
    const bn = bi[1];
    return Bosatsu_BinNat$toInt(bn);
  })() : (() => {
    const x = bi[1];
    return ~Bosatsu_BinNat$toInt(x);
  })();
var Bosatsu_BinInt$negate = bi => (() => {
  let _anon1;
  return (bi[0] === 0 && (() => {
    _anon1 = bi[1];
    return true;
  })() && (_anon1[0] === 0)) ? bi : (bi[0] === 0) ? (() => {
        const bn = bi[1];
        return (_a0 => [1, _a0])(Bosatsu_BinNat$prev(bn));
      })() : (() => {
        const x = bi[1];
        return (_a0 => [0, _a0])(Bosatsu_BinNat$next(x));
      })();
})();
var Bosatsu_BinInt$abs = bi => (bi[0] === 0) ? bi[1] : (() => {
    const x = bi[1];
    return Bosatsu_BinNat$next(x);
  })();
var Bosatsu_BinInt$add = (x, y) => (() => {
  const _anon6 = ((_a0, _a1) => [_a0, _a1])(x, y);
  return (() => {
    let _anon8;
    return (() => {
      let _anon7;
      return ((() => {
        _anon8 = _anon6[1];
        return true;
      })() && ((() => {
        _anon7 = _anon6[0];
        return true;
      })() && (_anon7[0] === 0 && (_anon8[0] === 0)))) ? (() => {
          const x_1 = _anon7[1];
          return (() => {
            const y_1 = _anon8[1];
            return (_a0 => [0, _a0])(Bosatsu_BinNat$add_BinNat(x_1, y_1));
          })();
        })() : (() => {
          let _anon10;
          return (() => {
            let _anon9;
            return ((() => {
              _anon10 = _anon6[1];
              return true;
            })() && ((() => {
              _anon9 = _anon6[0];
              return true;
            })() && (_anon9[0] === 0 && (_anon10[0] === 1)))) ? (() => {
                const x_2 = _anon9[1];
                return (() => {
                  const y_2 = _anon10[1];
                  return (() => {
                    const ypos = Bosatsu_BinNat$next(y_2);
                    return (() => {
                      const _anon3 = Bosatsu_BinNat$sub_Option(x_2, ypos);
                      return (_anon3[0] === 1) ? (() => {
                          const bi = _anon3[1];
                          return (_a0 => [0, _a0])(bi);
                        })() : (() => {
                          const bi_1 = (_a0 => [0,
                            _a0])(Bosatsu_BinNat$sub_BinNat(ypos, x_2));
                          return (() => {
                            let _anon2;
                            return (bi_1[0] === 0 && (() => {
                              _anon2 = bi_1[1];
                              return true;
                            })() && (_anon2[0] === 0)) ? bi_1 : (() => {
                                const bn = bi_1[1];
                                return (_a0 => [1,
                                  _a0])(Bosatsu_BinNat$prev(bn));
                              })();
                          })();
                        })();
                    })();
                  })();
                })();
              })() : (() => {
                let _anon12;
                return (() => {
                  let _anon11;
                  return ((() => {
                    _anon12 = _anon6[1];
                    return true;
                  })() && ((() => {
                    _anon11 = _anon6[0];
                    return true;
                  })() && (_anon11[0] === 1 && (_anon12[0] === 0)))) ? (() => {
                      const x_3 = _anon11[1];
                      return (() => {
                        const y_3 = _anon12[1];
                        return (() => {
                          const xpos = Bosatsu_BinNat$next(x_3);
                          return (() => {
                            const _anon5 = Bosatsu_BinNat$sub_Option(y_3, xpos);
                            return (_anon5[0] === 1) ? (() => {
                                const bi_2 = _anon5[1];
                                return (_a0 => [0, _a0])(bi_2);
                              })() : (() => {
                                const bi_3 = (_a0 => [0,
                                  _a0])(Bosatsu_BinNat$sub_BinNat(xpos, y_3));
                                return (() => {
                                  let _anon4;
                                  return (bi_3[0] === 0 && (() => {
                                    _anon4 = bi_3[1];
                                    return true;
                                  })() && (_anon4[0] === 0)) ? bi_3 : (() => {
                                      const bn_1 = bi_3[1];
                                      return (_a0 => [1,
                                        _a0])(Bosatsu_BinNat$prev(bn_1));
                                    })();
                                })();
                              })();
                          })();
                        })();
                      })();
                    })() : (() => {
                      let _anon14;
                      return (() => {
                        let _anon13;
                        return (() => {
                          (() => {
                            _anon14 = _anon6[1];
                            return true;
                          })() && (() => {
                            _anon13 = _anon6[0];
                            return true;
                          })();
                          return (() => {
                            const x_4 = _anon13[1];
                            return (() => {
                              const y_4 = _anon14[1];
                              return (_a0 => [1,
                                _a0])(Bosatsu_BinNat$next(Bosatsu_BinNat$add_BinNat(x_4, y_4)));
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
var Bosatsu_BinInt$sub = (a, b) => Bosatsu_BinInt$add(a, (() => {
    let _anon15;
    return (b[0] === 0 && (() => {
      _anon15 = b[1];
      return true;
    })() && (_anon15[0] === 0)) ? b : (b[0] === 0) ? (() => {
          const bn = b[1];
          return (_a0 => [1, _a0])(Bosatsu_BinNat$prev(bn));
        })() : (() => {
          const x = b[1];
          return (_a0 => [0, _a0])(Bosatsu_BinNat$next(x));
        })();
  })());
var Bosatsu_Dict$eq_Pair = (eq_a, eq_b) => (_slots => (a, b) => (() => {
  const l1 = a[0];
  return (() => {
    const l2 = a[1];
    return (() => {
      const r1 = b[0];
      return (() => {
        const r2 = b[1];
        return (() => {
          const _anon0 = _slots[0](l1, r1);
          return (_anon0[0] === 1) ? _slots[1](l2, r2) : [0];
        })();
      })();
    })();
  })();
})())([eq_a, eq_b]);
var Bosatsu_Dict$eq_Dict = (eq_key, eq_value) => (_slots => (left, right) => Bosatsu_List$eq_List((_slots => (a, b) => (() => {
    const l1 = a[0];
    return (() => {
      const l2 = a[1];
      return (() => {
        const r1 = b[0];
        return (() => {
          const r2 = b[1];
          return (() => {
            const _anon1 = _slots[0](l1, r1);
            return (_anon1[0] === 1) ? _slots[1](l2, r2) : [0];
          })();
        })();
      })();
    })();
  })())([_slots[0],
      _slots[1]]))(Bosatsu_Predef$items(left), Bosatsu_Predef$items(right)))([eq_key,
    eq_value]);
var Bosatsu_Example_ApplicativeTraverse$applicative_from_pure_map_product = (pure, map, product) => ((_a0, _a1, _a2, _a3, _a4) => [_a0,
  _a1,
  _a2,
  _a3,
  _a4])(pure, map, (_slots => (fn, fa) => _slots[0](b => (() => {
      const fn_1 = b[0];
      return (() => {
        const a = b[1];
        return fn_1(a);
      })();
    })(), _slots[1](fn, fa)))([map,
      product]), (_slots => (fa_1, fb, fn_2) => _slots[0]((_slots => c => (() => {
      const a_1 = c[0];
      return (() => {
        const b_1 = c[1];
        return _slots[0](a_1, b_1);
      })();
    })())([fn_2]), _slots[1](fa_1, fb)))([map, product]), product);
var Bosatsu_Example_ApplicativeTraverse$applicative_from_pure_ap = (pure, ap) => ((_a0, _a1, _a2, _a3, _a4) => [_a0,
  _a1,
  _a2,
  _a3,
  _a4])(pure, (_slots => (fn, fa) => _slots[0](_slots[1](fn), fa))([ap,
      pure]), ap, (_slots => (fa_1, fb, fn_1) => _slots[0](_slots[0](_slots[1]((_slots => a => (_slots => b => _slots[0](_slots[1], b))([_slots[0],
            a]))([fn_1])), fa_1), fb))([ap,
      pure]), (_slots => (fa_2, fb_1) => _slots[0](_slots[0](_slots[1](a_1 => (_slots => b_1 => ((_a0, _a1) => [_a0,
          _a1])(_slots[0], b_1))([a_1])), fa_2), fb_1))([ap, pure]));
var Bosatsu_Example_ApplicativeTraverse$applicative_Option = Bosatsu_Example_ApplicativeTraverse$applicative_from_pure_map_product(_a0 => [1,
    _a0], (fn, opt) => (opt[0] === 1) ? (() => {
      const a = opt[1];
      return (_a0 => [1, _a0])(fn(a));
    })() : [0], (as, bs) => (() => {
    const _anon0 = ((_a0, _a1) => [_a0, _a1])(as, bs);
    return (() => {
      let _anon2;
      return (() => {
        let _anon1;
        return ((() => {
          _anon2 = _anon0[1];
          return true;
        })() && ((() => {
          _anon1 = _anon0[0];
          return true;
        })() && (_anon1[0] === 1 && (_anon2[0] === 1)))) ? (() => {
            const a_1 = _anon1[1];
            return (() => {
              const b = _anon2[1];
              return (_a0 => [1, _a0])(((_a0, _a1) => [_a0, _a1])(a_1, b));
            })();
          })() : [0];
      })();
    })();
  })());
var Bosatsu_Example_ApplicativeTraverse$trav_l = (app, fn, lst) => (() => {
  const pure = app[0];
  return (() => {
    const map = app[1];
    return (() => {
      const product = app[4];
      return (() => {
        const loop = (_slots => (lst_1, ftail) => (() => {
          let _anon3;
          return (() => {
            let _anon4;
            return (() => {
              let _anon6;
              return (() => {
                let _anon8;
                return (() => {
                  (() => {
                    _anon6 = lst_1;
                    return true;
                  })();
                  return (() => {
                    (() => {
                      _anon8 = ftail;
                      return true;
                    })();
                    return (() => {
                      (() => {
                        _anon3 = [1];
                        return true;
                      })();
                      return (() => {
                        while (_anon3[0] === 1) {
                          (_anon6[0] === 0) ? (() => {
                              (() => {
                                _anon3 = [0];
                                return true;
                              })();
                              return (() => {
                                (() => {
                                  _anon4 = _anon8;
                                  return true;
                                })();
                                return [];
                              })();
                            })() : (() => {
                              const h = _anon6[1];
                              return (() => {
                                const t = _anon6[2];
                                return (() => {
                                  const _anon5 = t;
                                  return (() => {
                                    const _anon7 = _slots[1](a => (() => {
                                        const h_1 = a[0];
                                        return (() => {
                                          const t_1 = a[1];
                                          return ((_a0, _a1) => [1,
                                            _a0,
                                            _a1])(h_1, t_1);
                                        })();
                                      })(), _slots[2](_slots[0](h), _anon8));
                                    return (() => {
                                      (() => {
                                        _anon6 = _anon5;
                                        return true;
                                      })();
                                      return (() => {
                                        (() => {
                                          _anon8 = _anon7;
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
                        return _anon4;
                      })();
                    })();
                  })();
                })();
              })();
            })();
          })();
        })())([fn, map, product]);
        return map(Bosatsu_Predef$reverse, loop(lst, pure([0])));
      })();
    })();
  })();
})();
var Bosatsu_Example_ApplicativeTraverse$traverse_List = (_a0 => [_a0])(Bosatsu_Example_ApplicativeTraverse$trav_l);
var Bosatsu_Example_ApplicativeTraverse$eq_opt_list_int = Bosatsu_Option$eq_Option(Bosatsu_List$eq_List((_a0, _a1) => (_a0 === _a1) ? [1] : [0]));
var Bosatsu_Example_ApplicativeTraverse$test = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("applicative/traverse tests"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0,
      _a0,
      _a1])(Bosatsu_Example_ApplicativeTraverse$eq_opt_list_int(Bosatsu_Example_ApplicativeTraverse$trav_l(Bosatsu_Example_ApplicativeTraverse$applicative_Option, x => (_a0 => [1,
            _a0])(x + x), ((_a0, _a1) => [1, _a0, _a1])(1, ((_a0, _a1) => [1,
              _a0,
              _a1])(2, ((_a0, _a1) => [1, _a0, _a1])(3, [0])))), (_a0 => [1,
          _a0])(((_a0, _a1) => [1, _a0, _a1])(2, ((_a0, _a1) => [1,
              _a0,
              _a1])(4, ((_a0, _a1) => [1,
                _a0,
                _a1])(6, [0]))))), _js_to_bosatsu_string("double")), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1) => [0,
        _a0,
        _a1])(Bosatsu_Example_ApplicativeTraverse$eq_opt_list_int(Bosatsu_Example_ApplicativeTraverse$trav_l(Bosatsu_Example_ApplicativeTraverse$applicative_Option, a => [0], ((_a0, _a1) => [1,
              _a0,
              _a1])(1, ((_a0, _a1) => [1, _a0, _a1])(2, ((_a0, _a1) => [1,
                  _a0,
                  _a1])(3, [0])))), [0]), _js_to_bosatsu_string("all to None")), ((_a0, _a1) => [1,
        _a0,
        _a1])(((_a0, _a1) => [0,
          _a0,
          _a1])(Bosatsu_Example_ApplicativeTraverse$eq_opt_list_int(Bosatsu_Example_ApplicativeTraverse$trav_l(Bosatsu_Example_ApplicativeTraverse$applicative_Option, x_1 => (() => {
                const _anon9 = (x_1 === 3) ? [1] : [0];
                return (_anon9[0] === 1) ? [0] : (_a0 => [1, _a0])(x_1);
              })(), ((_a0, _a1) => [1, _a0, _a1])(1, ((_a0, _a1) => [1,
                  _a0,
                  _a1])(2, ((_a0, _a1) => [1,
                    _a0,
                    _a1])(3, [0])))), [0]), _js_to_bosatsu_string("3 to None")), ((_a0, _a1) => [1,
          _a0,
          _a1])(((_a0, _a1) => [0,
            _a0,
            _a1])(Bosatsu_Example_ApplicativeTraverse$eq_opt_list_int(Bosatsu_Example_ApplicativeTraverse$trav_l(Bosatsu_Example_ApplicativeTraverse$applicative_Option, x_2 => (() => {
                  const _anon10 = (x_2 === 3) ? [1] : [0];
                  return (_anon10[0] === 1) ? [0] : (_a0 => [1, _a0])(x_2);
                })(), [0]), (_a0 => [1,
                _a0])([0])), _js_to_bosatsu_string("empty to Some")), [0])))));
_tests["Bosatsu/Example/ApplicativeTraverse::test"] = Bosatsu_Example_ApplicativeTraverse$test;
var Bosatsu_Rand$bitmask_64 = 1 << 64 - 1;
var Bosatsu_Rand$rotl = (x, k) => x << k & Bosatsu_Rand$bitmask_64 | (x >> (0 - ((-64) + k)) & Bosatsu_Rand$bitmask_64);
var Bosatsu_Rand$next = state => (() => {
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
var Bosatsu_Rand$state_from_Int = i => (() => {
  const not_zero = (() => {
    const _anon0 = i ^ 54564800212389664567110541424720236321503446907971127334425124977866121780221;
    return (_anon0 === 0) ? 54564800212389664567110541424720236321503446907971127334425124977866121780221 : _anon0;
  })();
  return ((_a0, _a1, _a2, _a3) => [_a0,
    _a1,
    _a2,
    _a3])(not_zero & Bosatsu_Rand$bitmask_64, not_zero >> 64 & Bosatsu_Rand$bitmask_64, not_zero >> 128 & Bosatsu_Rand$bitmask_64, not_zero >> 192 & Bosatsu_Rand$bitmask_64);
})();
var Bosatsu_Rand$map_Rand = (r, fn) => (() => {
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
var Bosatsu_Rand$flat_map_Rand = (r, fn) => (() => {
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
var Bosatsu_Rand$prod_Rand = (ra, rb) => (() => {
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
var Bosatsu_Rand$const_Rand = a => (_a0 => [_a0])((_slots => s => ((_a0, _a1) => [_a0,
    _a1])(s, _slots[0]))([a]));
var Bosatsu_Rand$nat_2 = (n => n + 1)((n => n + 1)(0));
var Bosatsu_Rand$parity = (n, i) => (n === 0) ? (() => {
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
          const _anon8 = Bosatsu_Rand$parity(p, i >> Bosatsu_Nat$to_Int(Bosatsu_Nat$exp(Bosatsu_Rand$nat_2, n)));
          return (_anon8[0] === 1) ? (() => {
              const _anon7 = Bosatsu_Rand$parity(p, i);
              return (_anon7[0] === 1) ? [0] : [1];
            })() : Bosatsu_Rand$parity(p, i);
        })();
      })();
    })();
  })();
var Bosatsu_Rand$six = (n => n + 1)((n => n + 1)((n => n + 1)((n => n + 1)((n => n + 1)((n => n + 1)(0))))));
var Bosatsu_Rand$bool_Rand = (_a0 => [_a0])(s => (() => {
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
var Bosatsu_Rand$run_Rand = (rand, seed) => (() => {
  const fn = rand[0];
  return (() => {
    const _anon12 = fn(Bosatsu_Rand$state_from_Int(seed));
    return _anon12[1];
  })();
})();
var Bosatsu_Rand$sequence_Rand = rands => (() => {
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
var Bosatsu_Rand$uint64_Rand = (_a0 => [_a0])(Bosatsu_Rand$next);
var Bosatsu_Rand$bit_count = i => _int_loop(i, 0, (i_1, bits) => ((_a0, _a1) => [_a0,
    _a1])(i_1 >> 1, bits + 1));
var Bosatsu_Rand$to_big_Int = (us, acc) => (() => {
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
var Bosatsu_Rand$nat30 = Bosatsu_Nat$to_Nat(30);
var Bosatsu_Rand$resample = (rand_Int, high, uints) => (() => {
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
var Bosatsu_Rand$const0 = (_a0 => [_a0])(s => ((_a0, _a1) => [_a0, _a1])(s, 0));
var Bosatsu_Rand$int_range = high => (() => {
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
var Bosatsu_Rand$nat_range = high => (() => {
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
var Bosatsu_Rand$geometric = (depth, acc) => (depth === 0) ? (_a0 => [_a0])((_slots => s => ((_a0, _a1) => [_a0,
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
          })())([acc, Bosatsu_Rand$geometric, prev]));
      })();
    })();
  })();
var Bosatsu_Rand$geometric_Int = Bosatsu_Rand$geometric(Bosatsu_Rand$nat30, 0);
var Bosatsu_Rand$split_at = (list, idx) => (list[0] === 0) ? ((_a0, _a1) => [_a0, _a1])([0], [0]) : (() => {
    const h = list[1];
    return (() => {
      const t = list[2];
      return (idx[0] === 0) ? ((_a0, _a1) => [_a0, _a1])([0], list) : (() => {
          const _anon47 = Bosatsu_Rand$split_at(t, Bosatsu_BinNat$prev(idx));
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
var Bosatsu_Rand$from_pair = (left, right) => (_a0 => [_a0])((_slots => s => (() => {
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
var Bosatsu_Rand$one_of = (head, tail) => (() => {
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
var Euler_Four$max_opt = (o1, o2) => (o1[0] === 0) ? o2 : (() => {
    const s1 = o1;
    return (() => {
      const v1 = o1[1];
      return (o2[0] === 0) ? s1 : (() => {
          const s2 = o2;
          return (() => {
            const v2 = o2[1];
            return (() => {
              const _anon1 = (() => {
                const _anon0 = (v2 < v1) ? [0] : (v2 === v1) ? [1] : [2];
                return (_anon0[0] === 2) ? [1] : [0];
              })();
              return (_anon1[0] === 1) ? s2 : s1;
            })();
          })();
        })();
    })();
  })();
var Euler_Four$max_of = (n, fn) => (() => {
  const loop = (_slots => (nat, n_1, max) => (() => {
    let _anon3;
    return (() => {
      let _anon4;
      return (() => {
        let _anon6;
        return (() => {
          let _anon8;
          return (() => {
            let _anon10;
            return (() => {
              (() => {
                _anon6 = nat;
                return true;
              })();
              return (() => {
                (() => {
                  _anon8 = n_1;
                  return true;
                })();
                return (() => {
                  (() => {
                    _anon10 = max;
                    return true;
                  })();
                  return (() => {
                    (() => {
                      _anon3 = [1];
                      return true;
                    })();
                    return (() => {
                      while (_anon3[0] === 1) {
                        (_anon6 === 0) ? (() => {
                            (() => {
                              _anon3 = [0];
                              return true;
                            })();
                            return (() => {
                              (() => {
                                _anon4 = Euler_Four$max_opt(_anon10, _slots[0](_anon8));
                                return true;
                              })();
                              return [];
                            })();
                          })() : (() => {
                            let _anon2;
                            return (() => {
                              (() => {
                                _anon2 = _anon6 - 1;
                                return true;
                              })();
                              return (() => {
                                const prev_nat = _anon2;
                                return (() => {
                                  const _anon5 = prev_nat;
                                  return (() => {
                                    const _anon7 = (-1) + _anon8;
                                    return (() => {
                                      const _anon9 = Euler_Four$max_opt(_anon10, _slots[0](_anon8));
                                      return (() => {
                                        (() => {
                                          _anon6 = _anon5;
                                          return true;
                                        })();
                                        return (() => {
                                          (() => {
                                            _anon8 = _anon7;
                                            return true;
                                          })();
                                          return (() => {
                                            (() => {
                                              _anon10 = _anon9;
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
                      return _anon4;
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
  return loop(Bosatsu_Nat$to_Nat(n), n, [0]);
})();
var Euler_Four$first_of = (n, fn) => (() => {
  const loop = (_slots => (nat, n_1) => (() => {
    let _anon13;
    return (() => {
      let _anon14;
      return (() => {
        let _anon16;
        return (() => {
          let _anon18;
          return (() => {
            (() => {
              _anon16 = nat;
              return true;
            })();
            return (() => {
              (() => {
                _anon18 = n_1;
                return true;
              })();
              return (() => {
                (() => {
                  _anon13 = [1];
                  return true;
                })();
                return (() => {
                  while (_anon13[0] === 1) {
                    (_anon16 === 0) ? (() => {
                        (() => {
                          _anon13 = [0];
                          return true;
                        })();
                        return (() => {
                          (() => {
                            _anon14 = _slots[0](_anon18);
                            return true;
                          })();
                          return [];
                        })();
                      })() : (() => {
                        let _anon12;
                        return (() => {
                          (() => {
                            _anon12 = _anon16 - 1;
                            return true;
                          })();
                          return (() => {
                            const prev_nat = _anon12;
                            return (() => {
                              const _anon11 = _slots[0](_anon18);
                              return (_anon11[0] === 0) ? (() => {
                                  const _anon15 = prev_nat;
                                  return (() => {
                                    const _anon17 = (-1) + _anon18;
                                    return (() => {
                                      (() => {
                                        _anon16 = _anon15;
                                        return true;
                                      })();
                                      return (() => {
                                        (() => {
                                          _anon18 = _anon17;
                                          return true;
                                        })();
                                        return [];
                                      })();
                                    })();
                                  })();
                                })() : (() => {
                                  (() => {
                                    _anon13 = [0];
                                    return true;
                                  })();
                                  return (() => {
                                    (() => {
                                      _anon14 = _anon11;
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
                  return _anon14;
                })();
              })();
            })();
          })();
        })();
      })();
    })();
  })())([fn]);
  return loop(Bosatsu_Nat$to_Nat(n), n);
})();
var Euler_Four$digit_list = n => Bosatsu_Predef$reverse(_int_loop(n, [0], (n_1, acc) => ((_a0, _a1) => [_a0,
      _a1])(10 ? Math.trunc(n_1 / 10) : 0, ((_a0, _a1) => [1,
        _a0,
        _a1])(10 ? n_1 % 10 : n_1, acc))));
var Euler_Four$product_palindrome = (n1, n2) => (() => {
  const prod = n1 * n2;
  return (() => {
    const _anon19 = (() => {
      const digits = Euler_Four$digit_list(prod);
      return Bosatsu_List$eq_List((_a0, _a1) => (_a0 === _a1) ? [1] : [0])(digits, Bosatsu_Predef$reverse(digits));
    })();
    return (_anon19[0] === 1) ? (_a0 => [1, _a0])(prod) : [0];
  })();
})();
var Euler_Four$max_pal_opt = Euler_Four$max_of(99, n1 => Euler_Four$first_of(99, (_slots => n2 => Euler_Four$product_palindrome(_slots[0], n2))([n1])));
var Euler_Four$max_pal = (Euler_Four$max_pal_opt[0] === 1) ? Euler_Four$max_pal_opt[1] : 0;
var Euler_Four$test = ((_a0, _a1) => [0,
  _a0,
  _a1])((Euler_Four$max_pal === 9009) ? [1] : [0], _js_to_bosatsu_string("maximum palindrome"));
_tests["Euler/Four::test"] = Euler_Four$test;
var Euler_P5$factorial = n => _int_loop(n, 1, (i, p) => ((_a0, _a1) => [_a0, _a1])(i - 1, p * i));
var Euler_P5$max_candidate = Euler_P5$factorial(10);
var Euler_P5$int_loop_up = (top, res, fn) => _int_loop(top, res, (_slots => (i, res_1) => (() => {
    const _anon0 = _slots[0](_slots[1] - i, res_1);
    return (() => {
      const next_rev = _anon0[0];
      return (() => {
        const next_res = _anon0[1];
        return ((_a0, _a1) => [_a0, _a1])(_slots[1] - next_rev, next_res);
      })();
    })();
  })())([fn, top]));
var Euler_P5$bound = 1 + Euler_P5$max_candidate;
var Euler_P5$factors = range(10);
var Euler_P5$div_all = Euler_P5$int_loop_up(Euler_P5$bound, 0, (i, a) => (() => {
    const cand = 1 + i;
    return (() => {
      const _anon1 = Bosatsu_List$for_all(Euler_P5$factors, (_slots => f => (((1 + f) ? _slots[0] % (1 + f) : _slots[0]) === 0) ? [1] : [0])([cand]));
      return (_anon1[0] === 1) ? ((_a0, _a1) => [_a0,
          _a1])(Euler_P5$bound, cand) : ((_a0, _a1) => [_a0, _a1])(1 + i, 0);
    })();
  })());
var Euler_P5$test = ((_a0, _a1) => [0,
  _a0,
  _a1])((Euler_P5$div_all === 2520) ? [1] : [0], _js_to_bosatsu_string("test"));
_tests["Euler/P5::test"] = Euler_P5$test;
var Euler_P7$int_loop_up = (top, res, fn) => _int_loop(top, res, (_slots => (i, res_1) => (() => {
    const _anon0 = _slots[0](_slots[1] - i, res_1);
    return (() => {
      const next_rev = _anon0[0];
      return (() => {
        const next_res = _anon0[1];
        return ((_a0, _a1) => [_a0, _a1])(_slots[1] - next_rev, next_res);
      })();
    })();
  })())([fn, top]));
var Euler_P7$is_prime = x => Bosatsu_Bool$not(Euler_P7$int_loop_up(x, [0], (_slots => (i, div) => (() => {
      const candidate = 2 + i;
      return (() => {
        const _anon1 = (candidate < _slots[0]) ? [0] : (candidate === _slots[0]) ? [1] : [2];
        return (_anon1[0] === 0) ? (div[0] === 1) ? ((_a0, _a1) => [_a0,
              _a1])(_slots[0], [1]) : ((_a0, _a1) => [_a0,
              _a1])(1 + i, ((candidate ? _slots[0] % candidate : _slots[0]) === 0) ? [1] : [0]) : ((_a0, _a1) => [_a0,
            _a1])(_slots[0], [0]);
      })();
    })())([x])));
var Euler_P7$ith_prime = total => (() => {
  const max_size = range(total * total);
  return (() => {
    const _anon4 = foldl_List(max_size, ((_a0, _a1) => [_a0,
        _a1])([0], 0), (_slots => (prime_cnt, i) => (() => {
        const candidate = 2 + i;
        return (() => {
          const primes = prime_cnt[0];
          return (() => {
            const cnt = prime_cnt[1];
            return (() => {
              const _anon3 = (cnt < _slots[0]) ? [0] : (cnt === _slots[0]) ? [1] : [2];
              return (_anon3[0] === 0) ? (() => {
                  const _anon2 = Bosatsu_List$for_all(primes, (_slots => p => Bosatsu_Bool$not(((p ? _slots[0] % p : _slots[0]) === 0) ? [1] : [0]))([candidate]));
                  return (_anon2[0] === 1) ? ((_a0, _a1) => [_a0,
                      _a1])(((_a0, _a1) => [1,
                        _a0,
                        _a1])(candidate, primes), 1 + cnt) : prime_cnt;
                })() : prime_cnt;
            })();
          })();
        })();
      })())([total]));
    return (() => {
      const ps = _anon4[0];
      return (ps[0] === 1) ? ps[1] : (-1);
    })();
  })();
})();
var Euler_P7$test = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("euler 7"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0,
      _a0,
      _a1])(Euler_P7$is_prime(13), _js_to_bosatsu_string("6th prime is 13")), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1) => [0,
        _a0,
        _a1])((Euler_P7$ith_prime(6) === 13) ? [1] : [0], _js_to_bosatsu_string("6th prime is 13")), ((_a0, _a1) => [1,
        _a0,
        _a1])(((_a0, _a1) => [0,
          _a0,
          _a1])((Euler_P7$ith_prime(11) === 31) ? [1] : [0], _js_to_bosatsu_string("11th prime is 31")), [0]))));
_tests["Euler/P7::test"] = Euler_P7$test;
var Euler_Two$fib = n => foldl_List(range(n), [0], (revFib, a) => (revFib[0] === 0) ? ((_a0, _a1) => [1,
      _a0,
      _a1])(1, [0]) : (() => {
      let _anon0;
      return (revFib[0] === 1 && (() => {
        _anon0 = revFib[2];
        return true;
      })() && (_anon0[0] === 0)) ? (() => {
          const h = revFib[1];
          return ((_a0, _a1) => [1, _a0, _a1])(2, ((_a0, _a1) => [1,
              _a0,
              _a1])(h, [0]));
        })() : (() => {
          let _anon1;
          return (() => {
            (() => {
              _anon1 = revFib[2];
              return true;
            })();
            return (() => {
              const h1 = revFib[1];
              return (() => {
                const h2 = _anon1[1];
                return ((_a0, _a1) => [1, _a0, _a1])(h1 + h2, revFib);
              })();
            })();
          })();
        })();
    })());
var Euler_Two$computed = Bosatsu_List$sum(flat_map_List(Euler_Two$fib(35), f => (() => {
      const _anon3 = (() => {
        const _anon2 = (f < 4000000) ? [0] : (f === 4000000) ? [1] : [2];
        return (_anon2[0] === 2) ? [0] : ((2 ? f % 2 : f) === 0) ? [1] : [0];
      })();
      return (_anon3[0] === 1) ? ((_a0, _a1) => [1, _a0, _a1])(f, [0]) : [0];
    })()));
var Euler_Two$test = ((_a0, _a1) => [0,
  _a0,
  _a1])((Euler_Two$computed === 4613732) ? [1] : [0], _js_to_bosatsu_string("expected 4613732"));
_tests["Euler/Two::test"] = Euler_Two$test;
var GenDeps$main = ((_a0, _a1, _a2) => [_a0,
  _a1,
  _a2])(BazelDepsApi$default_options_with_scala, BazelDepsApi$merge_dep_List(Bosatsu_Predef$concat(Bosatsu_Predef$map_List(((_a0, _a1) => [1,
          _a0,
          _a1])(((_a0, _a1, _a2) => [_a0,
            _a1,
            _a2])(_js_to_bosatsu_string("com.lihaoyi"), _js_to_bosatsu_string("sourcecode"), _js_to_bosatsu_string("0.1.4")), ((_a0, _a1) => [1,
            _a0,
            _a1])(((_a0, _a1, _a2) => [_a0,
              _a1,
              _a2])(_js_to_bosatsu_string("com.monovore"), _js_to_bosatsu_string("decline"), _js_to_bosatsu_string("1.0.0")), ((_a0, _a1) => [1,
              _a0,
              _a1])(((_a0, _a1, _a2) => [_a0,
                _a1,
                _a2])(_js_to_bosatsu_string("org.bykn"), _js_to_bosatsu_string("fastparse-cats-core"), _js_to_bosatsu_string("0.1.0")), ((_a0, _a1) => [1,
                _a0,
                _a1])(((_a0, _a1, _a2) => [_a0,
                  _a1,
                  _a2])(_js_to_bosatsu_string("org.scala-lang.modules"), _js_to_bosatsu_string("scala-xml"), _js_to_bosatsu_string("1.0.6")), ((_a0, _a1) => [1,
                  _a0,
                  _a1])(((_a0, _a1, _a2) => [_a0,
                    _a1,
                    _a2])(_js_to_bosatsu_string("org.scalacheck"), _js_to_bosatsu_string("scalacheck"), _js_to_bosatsu_string("1.13.5")), ((_a0, _a1) => [1,
                    _a0,
                    _a1])(((_a0, _a1, _a2) => [_a0,
                      _a1,
                      _a2])(_js_to_bosatsu_string("org.scalactic"), _js_to_bosatsu_string("scalactic"), _js_to_bosatsu_string("3.0.1")), ((_a0, _a1) => [1,
                      _a0,
                      _a1])(((_a0, _a1, _a2) => [_a0,
                        _a1,
                        _a2])(_js_to_bosatsu_string("org.spire-math"), _js_to_bosatsu_string("kind-projector"), _js_to_bosatsu_string("0.9.4")), ((_a0, _a1) => [1,
                        _a0,
                        _a1])(((_a0, _a1, _a2) => [_a0,
                          _a1,
                          _a2])(_js_to_bosatsu_string("org.typelevel"), _js_to_bosatsu_string("alleycats-core"), _js_to_bosatsu_string("2.0.0")), ((_a0, _a1) => [1,
                          _a0,
                          _a1])(((_a0, _a1, _a2) => [_a0,
                            _a1,
                            _a2])(_js_to_bosatsu_string("org.typelevel"), _js_to_bosatsu_string("cats-effect"), _js_to_bosatsu_string("2.0.0")), ((_a0, _a1) => [1,
                            _a0,
                            _a1])(((_a0, _a1, _a2) => [_a0,
                              _a1,
                              _a2])(_js_to_bosatsu_string("org.typelevel"), _js_to_bosatsu_string("paiges-core"), _js_to_bosatsu_string("0.3.0")), [0])))))))))), b => (() => {
          const o = b[0];
          return (() => {
            const a = b[1];
            return (() => {
              const v = b[2];
              return BazelDepsApi$scala_dep(o, a, v);
            })();
          })();
        })()), ((_a0, _a1) => [1,
        _a0,
        _a1])(BazelDepsApi$scala_dep_modules(_js_to_bosatsu_string("org.scalatest"), _js_to_bosatsu_string("scalatest"), ((_a0, _a1) => [1,
            _a0,
            _a1])(_js_to_bosatsu_string(""), [0]), _js_to_bosatsu_string("3.0.1"), ((_a0, _a1) => [1,
            _a0,
            _a1])(_js_to_bosatsu_string("org.scalactic:scalactic"), [0])), ((_a0, _a1) => [1,
          _a0,
          _a1])(BazelDepsApi$scala_dep_modules(_js_to_bosatsu_string("org.typelevel"), _js_to_bosatsu_string("cats"), ((_a0, _a1) => [1,
              _a0,
              _a1])(_js_to_bosatsu_string("core"), ((_a0, _a1) => [1,
                _a0,
                _a1])(_js_to_bosatsu_string("free"), ((_a0, _a1) => [1,
                  _a0,
                  _a1])(_js_to_bosatsu_string("kernel"), ((_a0, _a1) => [1,
                    _a0,
                    _a1])(_js_to_bosatsu_string("macros"), [0])))), _js_to_bosatsu_string("2.0.0"), [0]), ((_a0, _a1) => [1,
            _a0,
            _a1])(BazelDepsApi$scala_dep_modules(_js_to_bosatsu_string("com.lihaoyi"), _js_to_bosatsu_string("fastparse"), ((_a0, _a1) => [1,
                _a0,
                _a1])(_js_to_bosatsu_string(""), ((_a0, _a1) => [1,
                  _a0,
                  _a1])(_js_to_bosatsu_string("utils"), [0])), _js_to_bosatsu_string("1.0.0"), ((_a0, _a1) => [1,
                _a0,
                _a1])(_js_to_bosatsu_string("com.lihaoyi:sourcecode"), [0])), [0]))))), BazelDepsApi$standard_scala_replacements);
var Bosatsu_Properties$forall_Prop = (rand, name, fn) => (_a0 => [_a0])((_slots => cnt => (() => {
    const rands = Bosatsu_Predef$replicate_List(_slots[0], cnt);
    return (() => {
      const seq = Bosatsu_Rand$sequence_Rand(rands);
      return Bosatsu_Rand$map_Rand(seq, (_slots => as => ((_a0, _a1) => [1,
          _a0,
          _a1])(_slots[0], Bosatsu_Predef$map_List(as, _slots[1])))([_slots[1],
            _slots[2]]));
    })();
  })())([rand, name, fn]));
var Bosatsu_Properties$suite_Prop = (name, props) => (_a0 => [_a0])((_slots => s => (() => {
    const s_1 = Bosatsu_Rand$sequence_Rand(Bosatsu_Predef$map_List(_slots[0], (_slots => a => (() => {
          const fn = a[0];
          return fn(_slots[0]);
        })())([s])));
    return Bosatsu_Rand$map_Rand(s_1, (_slots => as => ((_a0, _a1) => [1,
        _a0,
        _a1])(_slots[0], as))([_slots[1]]));
  })())([props, name]));
var Bosatsu_Properties$run_Prop = (prop, trials, seed) => (() => {
  const fn = prop[0];
  return Bosatsu_Rand$run_Rand(fn(trials), seed);
})();
var Bosatsu_Properties$signed64 = Bosatsu_Rand$map_Rand(Bosatsu_Rand$int_range(1 << 64), i => i - (1 << 63));
var Bosatsu_Properties$not_law = Bosatsu_Properties$forall_Prop(Bosatsu_Properties$signed64, _js_to_bosatsu_string("not_law"), i => (() => {
    const istr = _int_to_String(i);
    return ((_a0, _a1) => [0,
      _a0,
      _a1])(((~i) === (0 - (1 + i))) ? [1] : [0], _concat_String(((_a0, _a1) => [1,
          _a0,
          _a1])(_js_to_bosatsu_string("~"), ((_a0, _a1) => [1,
            _a0,
            _a1])(istr, ((_a0, _a1) => [1,
              _a0,
              _a1])(_js_to_bosatsu_string(" == (-1 - "), ((_a0, _a1) => [1,
                _a0,
                _a1])(istr, ((_a0, _a1) => [1,
                  _a0,
                  _a1])(_js_to_bosatsu_string(")"), [0])))))));
  })());
var Bosatsu_Properties$shift_unshift_law = Bosatsu_Properties$forall_Prop(Bosatsu_Rand$prod_Rand(Bosatsu_Properties$signed64, Bosatsu_Rand$int_range(32)), _js_to_bosatsu_string("shift_unshift_law"), a => (() => {
    const i = a[0];
    return (() => {
      const k = a[1];
      return (() => {
        const istr = _int_to_String(i);
        return (() => {
          const kstr = _int_to_String(k);
          return (() => {
            const result = i << k >> k;
            return ((_a0, _a1) => [0,
              _a0,
              _a1])((result === i) ? [1] : [0], _concat_String(((_a0, _a1) => [1,
                  _a0,
                  _a1])(_js_to_bosatsu_string("("), ((_a0, _a1) => [1,
                    _a0,
                    _a1])(istr, ((_a0, _a1) => [1,
                      _a0,
                      _a1])(_js_to_bosatsu_string(" << "), ((_a0, _a1) => [1,
                        _a0,
                        _a1])(kstr, ((_a0, _a1) => [1,
                          _a0,
                          _a1])(_js_to_bosatsu_string(") >> "), ((_a0, _a1) => [1,
                            _a0,
                            _a1])(kstr, ((_a0, _a1) => [1,
                              _a0,
                              _a1])(_js_to_bosatsu_string(" == "), ((_a0, _a1) => [1,
                                _a0,
                                _a1])(istr, ((_a0, _a1) => [1,
                                  _a0,
                                  _a1])(_js_to_bosatsu_string(", got: "), ((_a0, _a1) => [1,
                                    _a0,
                                    _a1])(_int_to_String(result), [0]))))))))))));
          })();
        })();
      })();
    })();
  })());
var Bosatsu_Properties$positive_and_law = Bosatsu_Properties$forall_Prop(Bosatsu_Rand$prod_Rand(Bosatsu_Properties$signed64, Bosatsu_Properties$signed64), _js_to_bosatsu_string("x & y is >= 0 implies x >= 0 or y >= 0"), a => (() => {
    const x = a[0];
    return (() => {
      const y = a[1];
      return (() => {
        const xs = _int_to_String(x);
        return (() => {
          const ys = _int_to_String(y);
          return ((_a0, _a1) => [0, _a0, _a1])((() => {
              const _anon4 = (() => {
                const _anon0 = (x & y < 0) ? [0] : (x & y === 0) ? [1] : [2];
                return (_anon0[0] === 2) ? [1] : (_anon0[0] === 1) ? [1] : [0];
              })();
              return (_anon4[0] === 1) ? (() => {
                  const _anon3 = (() => {
                    const _anon1 = (x < 0) ? [0] : (x === 0) ? [1] : [2];
                    return (_anon1[0] === 2) ? [1] : (_anon1[0] === 1) ? [1] : [0];
                  })();
                  return (_anon3[0] === 1) ? [1] : (() => {
                      const _anon2 = (y < 0) ? [0] : (y === 0) ? [1] : [2];
                      return (_anon2[0] === 2) ? [1] : (_anon2[0] === 1) ? [1] : [0];
                    })();
                })() : [1];
            })(), _concat_String(((_a0, _a1) => [1,
                _a0,
                _a1])(xs, ((_a0, _a1) => [1,
                  _a0,
                  _a1])(_js_to_bosatsu_string(" & "), ((_a0, _a1) => [1,
                    _a0,
                    _a1])(ys, ((_a0, _a1) => [1,
                      _a0,
                      _a1])(_js_to_bosatsu_string(" is >= 0 implies "), ((_a0, _a1) => [1,
                        _a0,
                        _a1])(xs, ((_a0, _a1) => [1,
                          _a0,
                          _a1])(_js_to_bosatsu_string(" >= 0 or "), ((_a0, _a1) => [1,
                            _a0,
                            _a1])(ys, ((_a0, _a1) => [1,
                              _a0,
                              _a1])(_js_to_bosatsu_string(" >= 0"), [0]))))))))));
        })();
      })();
    })();
  })());
var Bosatsu_Properties$all_props = Bosatsu_Properties$suite_Prop(_js_to_bosatsu_string("integer props"), ((_a0, _a1) => [1,
    _a0,
    _a1])(Bosatsu_Properties$not_law, ((_a0, _a1) => [1,
      _a0,
      _a1])(Bosatsu_Properties$shift_unshift_law, ((_a0, _a1) => [1,
        _a0,
        _a1])(Bosatsu_Properties$positive_and_law, [0]))));
var Bosatsu_Properties$test = (() => {
  const fn = Bosatsu_Properties$all_props[0];
  return Bosatsu_Rand$run_Rand(fn(100), 42);
})();
_tests["Bosatsu/Properties::test"] = Bosatsu_Properties$test;
var Bosatsu_NumberProps$rand_Int = Bosatsu_Rand$from_pair(Bosatsu_Rand$int_range(128), Bosatsu_Rand$geometric_Int);
var Bosatsu_NumberProps$rand_Nat = Bosatsu_Rand$map_Rand(Bosatsu_NumberProps$rand_Int, Bosatsu_Nat$to_Nat);
var Bosatsu_NumberProps$rand_BinNat = Bosatsu_Rand$map_Rand(Bosatsu_NumberProps$rand_Int, Bosatsu_BinNat$toBinNat);
var Bosatsu_NumberProps$rand_BinInt = (() => {
  const pos = Bosatsu_Rand$map_Rand(Bosatsu_NumberProps$rand_Int, Bosatsu_BinInt$int_to_BinInt);
  return Bosatsu_Rand$from_pair(pos, Bosatsu_Rand$map_Rand(pos, Bosatsu_BinInt$not));
})();
var Bosatsu_NumberProps$int_props = Bosatsu_Properties$suite_Prop(_js_to_bosatsu_string("Int props"), ((_a0, _a1) => [1,
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
var Bosatsu_NumberProps$cmp_Comparison = (c1, c2) => (c1[0] === 0) ? (c2[0] === 0) ? [1] : [0] : (c1[0] === 1) ? (c2[0] === 0) ? [2] : (c2[0] === 1) ? [1] : [0] : (c2[0] === 2) ? [1] : [2];
var Bosatsu_NumberProps$exp_Int = (base, power) => _int_loop(power, 1, (_slots => (p, acc) => ((_a0, _a1) => [_a0,
    _a1])((-1) + p, acc * _slots[0]))([base]));
var Bosatsu_NumberProps$small_rand_Nat = Bosatsu_Rand$map_Rand(Bosatsu_Rand$int_range(7), Bosatsu_Nat$to_Nat);
var Bosatsu_NumberProps$small_rand_BinNat = Bosatsu_Rand$map_Rand(Bosatsu_Rand$int_range(7), Bosatsu_BinNat$toBinNat);
var Bosatsu_NumberProps$nat_props = Bosatsu_Properties$suite_Prop(_js_to_bosatsu_string("Nat props"), ((_a0, _a1) => [1,
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
var Bosatsu_NumberProps$binnat_props = Bosatsu_Properties$suite_Prop(_js_to_bosatsu_string("BinNat props"), ((_a0, _a1) => [1,
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
var Bosatsu_NumberProps$binint_props = Bosatsu_Properties$suite_Prop(_js_to_bosatsu_string("BinInt props"), ((_a0, _a1) => [1,
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
var Bosatsu_NumberProps$all_props = ((_a0, _a1) => [1, _a0, _a1])(Bosatsu_NumberProps$int_props, ((_a0, _a1) => [1,
    _a0,
    _a1])(Bosatsu_NumberProps$nat_props, ((_a0, _a1) => [1,
      _a0,
      _a1])(Bosatsu_NumberProps$binnat_props, ((_a0, _a1) => [1,
        _a0,
        _a1])(Bosatsu_NumberProps$binint_props, [0]))));
var Bosatsu_NumberProps$test = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("properties"), Bosatsu_Predef$map_List(Bosatsu_NumberProps$all_props, p => Bosatsu_Properties$run_Prop(p, 100, 123456)));
_tests["Bosatsu/NumberProps::test"] = Bosatsu_NumberProps$test;
var Queue$empty_Queue = ((_a0, _a1) => [_a0, _a1])([0], [0]);
var Queue$from_List = list => ((_a0, _a1) => [_a0, _a1])(list, [0]);
var Queue$push = (a, item) => (() => {
  const f = a[0];
  return (() => {
    const b = a[1];
    return ((_a0, _a1) => [_a0, _a1])(f, ((_a0, _a1) => [1,
        _a0,
        _a1])(item, b));
  })();
})();
var Queue$unpush = queue => (() => {
  let _anon1;
  return ((() => {
    _anon1 = queue[0];
    return true;
  })() && (_anon1[0] === 1)) ? (() => {
      const h = _anon1[1];
      return (() => {
        const t = _anon1[2];
        return (() => {
          const b = queue[1];
          return (_a0 => [1, _a0])(((_a0, _a1) => [_a0,
              _a1])(h, ((_a0, _a1) => [_a0, _a1])(t, b)));
        })();
      })();
    })() : (() => {
      const b_1 = queue[1];
      return (() => {
        const _anon0 = Bosatsu_Predef$reverse(b_1);
        return (_anon0[0] === 0) ? [0] : (() => {
            const h_1 = _anon0[1];
            return (() => {
              const t_1 = _anon0[2];
              return (_a0 => [1, _a0])(((_a0, _a1) => [_a0,
                  _a1])(h_1, ((_a0, _a1) => [_a0, _a1])(t_1, [0])));
            })();
          })();
      })();
    })();
})();
var Queue$pop_value = queue => (() => {
  const _anon3 = Queue$unpush(queue);
  return (() => {
    let _anon4;
    return (_anon3[0] === 1 && (() => {
      _anon4 = _anon3[1];
      return true;
    })()) ? (() => {
        const a = _anon4[0];
        return (_a0 => [1, _a0])(a);
      })() : [0];
  })();
})();
var Queue$pop = queue => (() => {
  const _anon5 = Queue$unpush(queue);
  return (() => {
    let _anon6;
    return (_anon5[0] === 1 && (() => {
      _anon6 = _anon5[1];
      return true;
    })()) ? _anon6[1] : Queue$empty_Queue;
  })();
})();
var Queue$fold_Queue = (a, init, fold_fn) => (() => {
  const f = a[0];
  return (() => {
    const b = a[1];
    return foldl_List(Bosatsu_Predef$reverse(b), foldl_List(f, init, fold_fn), fold_fn);
  })();
})();
var Queue$reverse_Queue = a => (() => {
  const f = a[0];
  return (() => {
    const b = a[1];
    return ((_a0, _a1) => [_a0, _a1])(b, f);
  })();
})();
var Queue$eq_Queue = eq_fn => (_slots => (left, right) => (() => {
  const init = ((_a0, _a1) => [_a0, _a1])([1], right);
  return (() => {
    const _anon11 = (() => {
      const f = left[0];
      return (() => {
        const b = left[1];
        return foldl_List(Bosatsu_Predef$reverse(b), foldl_List(f, init, (_slots => (a, al) => (() => {
              const g = a[0];
              return (() => {
                const right_1 = a[1];
                return (g[0] === 1) ? (() => {
                    const _anon7 = Queue$unpush(right_1);
                    return (_anon7[0] === 0) ? ((_a0, _a1) => [_a0,
                        _a1])([0], Queue$empty_Queue) : (() => {
                        let _anon8;
                        return (() => {
                          (() => {
                            _anon8 = _anon7[1];
                            return true;
                          })();
                          return (() => {
                            const ar = _anon8[0];
                            return (() => {
                              const right_2 = _anon8[1];
                              return ((_a0, _a1) => [_a0,
                                _a1])(_slots[0](al, ar), right_2);
                            })();
                          })();
                        })();
                      })();
                  })() : ((_a0, _a1) => [_a0, _a1])([0], Queue$empty_Queue);
              })();
            })())([_slots[0]])), (_slots => (a_1, al_1) => (() => {
            const g_1 = a_1[0];
            return (() => {
              const right_3 = a_1[1];
              return (g_1[0] === 1) ? (() => {
                  const _anon9 = Queue$unpush(right_3);
                  return (_anon9[0] === 0) ? ((_a0, _a1) => [_a0,
                      _a1])([0], Queue$empty_Queue) : (() => {
                      let _anon10;
                      return (() => {
                        (() => {
                          _anon10 = _anon9[1];
                          return true;
                        })();
                        return (() => {
                          const ar_1 = _anon10[0];
                          return (() => {
                            const right_4 = _anon10[1];
                            return ((_a0, _a1) => [_a0,
                              _a1])(_slots[0](al_1, ar_1), right_4);
                          })();
                        })();
                      })();
                    })();
                })() : ((_a0, _a1) => [_a0, _a1])([0], Queue$empty_Queue);
            })();
          })())([_slots[0]]));
      })();
    })();
    return (() => {
      let _anon13;
      return (() => {
        let _anon12;
        return (() => {
          let _anon15;
          return (() => {
            let _anon14;
            return ((() => {
              _anon13 = _anon11[1];
              return true;
            })() && ((() => {
              _anon12 = _anon11[0];
              return true;
            })() && (_anon12[0] === 1 && ((() => {
              _anon15 = _anon13[1];
              return true;
            })() && ((() => {
              _anon14 = _anon13[0];
              return true;
            })() && (_anon14[0] === 0 && (_anon15[0] === 0))))))) ? [1] : [0];
          })();
        })();
      })();
    })();
  })();
})())([eq_fn]);
var Queue$to_List = a => (() => {
  const f = a[0];
  return (() => {
    const b = a[1];
    return Bosatsu_Predef$concat(f, Bosatsu_Predef$reverse(b));
  })();
})();
var Queue$eq_qi = Queue$eq_Queue((_a0, _a1) => (_a0 === _a1) ? [1] : [0]);
var Queue$eq_li = Bosatsu_List$eq_List((_a0, _a1) => (_a0 === _a1) ? [1] : [0]);
var Queue$q12 = (() => {
  const _anon16 = Queue$push(Queue$empty_Queue, 1);
  return (() => {
    const f = _anon16[0];
    return (() => {
      const b = _anon16[1];
      return ((_a0, _a1) => [_a0, _a1])(f, ((_a0, _a1) => [1, _a0, _a1])(2, b));
    })();
  })();
})();
var Queue$rand_int = Bosatsu_Rand$int_range(128);
var Queue$rand_geo_List_Int = Bosatsu_Rand$flat_map_Rand(Bosatsu_Rand$geometric_Int, len => Bosatsu_Rand$sequence_Rand(Bosatsu_Predef$replicate_List(Queue$rand_int, len)));
var Queue$queue_from_list = Bosatsu_Rand$map_Rand(Queue$rand_geo_List_Int, Queue$from_List);
var Queue$rand_Queue_depth = depth => (depth === 0) ? Queue$queue_from_list : (() => {
    let _anon17;
    return (() => {
      (() => {
        _anon17 = depth - 1;
        return true;
      })();
      return (() => {
        const n = _anon17;
        return (() => {
          const smaller = Queue$rand_Queue_depth(n);
          return (() => {
            const pop_rand = Bosatsu_Rand$map_Rand(smaller, Queue$pop);
            return Bosatsu_Rand$one_of(pop_rand, ((_a0, _a1) => [1,
                _a0,
                _a1])(Bosatsu_Rand$map_Rand(Bosatsu_Rand$prod_Rand(Queue$rand_int, smaller), a => (() => {
                    const h = a[0];
                    return (() => {
                      const q = a[1];
                      return (() => {
                        const f = q[0];
                        return (() => {
                          const b = q[1];
                          return ((_a0, _a1) => [_a0,
                            _a1])(f, ((_a0, _a1) => [1, _a0, _a1])(h, b));
                        })();
                      })();
                    })();
                  })()), ((_a0, _a1) => [1,
                  _a0,
                  _a1])(Bosatsu_Rand$map_Rand(smaller, Queue$reverse_Queue), [0])));
          })();
        })();
      })();
    })();
  })();
var Queue$rand_Queue_Int = Queue$rand_Queue_depth(Bosatsu_Nat$to_Nat(50));
var Queue$show_List = (lst, showa) => (() => {
  const inner = (_slots => lst_1 => (lst_1[0] === 0) ? _js_to_bosatsu_string("") : (() => {
      let _anon18;
      return (lst_1[0] === 1 && (() => {
        _anon18 = lst_1[2];
        return true;
      })() && (_anon18[0] === 0)) ? (() => {
          const a = lst_1[1];
          return _slots[0](a);
        })() : (() => {
          const a_1 = lst_1[1];
          return (() => {
            const t = lst_1[2];
            return _concat_String(((_a0, _a1) => [1,
                _a0,
                _a1])(_slots[0](a_1), ((_a0, _a1) => [1,
                  _a0,
                  _a1])(_js_to_bosatsu_string(", "), ((_a0, _a1) => [1,
                    _a0,
                    _a1])(inner(t), [0]))));
          })();
        })();
    })())([showa]);
  return _concat_String(((_a0, _a1) => [1,
      _a0,
      _a1])(_js_to_bosatsu_string("["), ((_a0, _a1) => [1,
        _a0,
        _a1])(inner(lst), ((_a0, _a1) => [1,
          _a0,
          _a1])(_js_to_bosatsu_string("]"), [0]))));
})();
var Queue$show_Queue = (q, showa) => (() => {
  const f = q[0];
  return (() => {
    const b = q[1];
    return _concat_String(((_a0, _a1) => [1,
        _a0,
        _a1])(_js_to_bosatsu_string("Queue("), ((_a0, _a1) => [1,
          _a0,
          _a1])(Queue$show_List(f, showa), ((_a0, _a1) => [1,
            _a0,
            _a1])(_js_to_bosatsu_string(", "), ((_a0, _a1) => [1,
              _a0,
              _a1])(Queue$show_List(b, showa), ((_a0, _a1) => [1,
                _a0,
                _a1])(_js_to_bosatsu_string(")"), [0]))))));
  })();
})();
var Queue$queue_laws = Bosatsu_Properties$suite_Prop(_js_to_bosatsu_string("queue properties"), ((_a0, _a1) => [1,
    _a0,
    _a1])(Bosatsu_Properties$forall_Prop(Queue$rand_Queue_Int, _js_to_bosatsu_string("pop-law/toList"), q => ((_a0, _a1) => [0,
        _a0,
        _a1])((() => {
          const _anon23 = (() => {
            const _anon19 = Queue$unpush(q);
            return (() => {
              let _anon20;
              return (_anon19[0] === 1 && (() => {
                _anon20 = _anon19[1];
                return true;
              })()) ? (() => {
                  const a = _anon20[0];
                  return (_a0 => [1, _a0])(a);
                })() : [0];
            })();
          })();
          return (_anon23[0] === 0) ? (() => {
              const _anon21 = (() => {
                const f = q[0];
                return (() => {
                  const b = q[1];
                  return Bosatsu_Predef$concat(f, Bosatsu_Predef$reverse(b));
                })();
              })();
              return (_anon21[0] === 0) ? [1] : [0];
            })() : (() => {
              const i = _anon23[1];
              return (() => {
                const _anon22 = (() => {
                  const f_1 = q[0];
                  return (() => {
                    const b_1 = q[1];
                    return Bosatsu_Predef$concat(f_1, Bosatsu_Predef$reverse(b_1));
                  })();
                })();
                return (_anon22[0] === 1) ? (() => {
                    const h = _anon22[1];
                    return (h === i) ? [1] : [0];
                  })() : [0];
              })();
            })();
        })(), _js_to_bosatsu_string("check head"))), ((_a0, _a1) => [1,
      _a0,
      _a1])(Bosatsu_Properties$forall_Prop(Queue$rand_Queue_Int, _js_to_bosatsu_string("reverse isomorphism"), q_1 => (() => {
          const rev_tl = (() => {
            const _anon24 = Queue$reverse_Queue(q_1);
            return (() => {
              const f_2 = _anon24[0];
              return (() => {
                const b_2 = _anon24[1];
                return Bosatsu_Predef$concat(f_2, Bosatsu_Predef$reverse(b_2));
              })();
            })();
          })();
          return (() => {
            const tl_rev = Bosatsu_Predef$reverse(Queue$to_List(q_1));
            return ((_a0, _a1) => [0,
              _a0,
              _a1])(Queue$eq_li(rev_tl, tl_rev), _concat_String(((_a0, _a1) => [1,
                  _a0,
                  _a1])(_js_to_bosatsu_string("rev_tl = "), ((_a0, _a1) => [1,
                    _a0,
                    _a1])(Queue$show_List(rev_tl, _a0 => _int_to_String(_a0)), ((_a0, _a1) => [1,
                      _a0,
                      _a1])(_js_to_bosatsu_string(" tl_rev = "), ((_a0, _a1) => [1,
                        _a0,
                        _a1])(Queue$show_List(tl_rev, _a0 => _int_to_String(_a0)), ((_a0, _a1) => [1,
                          _a0,
                          _a1])(_js_to_bosatsu_string(": "), ((_a0, _a1) => [1,
                            _a0,
                            _a1])(Queue$show_Queue(q_1, _a0 => _int_to_String(_a0)), [0]))))))));
          })();
        })()), ((_a0, _a1) => [1,
        _a0,
        _a1])(Bosatsu_Properties$forall_Prop(Bosatsu_Rand$prod_Rand(Queue$rand_int, Queue$rand_Queue_Int), _js_to_bosatsu_string("push is the same as reverse prepend reverse"), a_1 => (() => {
            const h_1 = a_1[0];
            return (() => {
              const q_2 = a_1[1];
              return ((_a0, _a1) => [0, _a0, _a1])(Queue$eq_li((() => {
                    const _anon25 = (() => {
                      const f_3 = q_2[0];
                      return (() => {
                        const b_3 = q_2[1];
                        return ((_a0, _a1) => [_a0,
                          _a1])(f_3, ((_a0, _a1) => [1, _a0, _a1])(h_1, b_3));
                      })();
                    })();
                    return (() => {
                      const f_4 = _anon25[0];
                      return (() => {
                        const b_4 = _anon25[1];
                        return Bosatsu_Predef$concat(f_4, Bosatsu_Predef$reverse(b_4));
                      })();
                    })();
                  })(), Bosatsu_Predef$reverse(((_a0, _a1) => [1,
                      _a0,
                      _a1])(h_1, Bosatsu_Predef$reverse((() => {
                          const f_5 = q_2[0];
                          return (() => {
                            const b_5 = q_2[1];
                            return Bosatsu_Predef$concat(f_5, Bosatsu_Predef$reverse(b_5));
                          })();
                        })())))), _js_to_bosatsu_string("push isomorphism"));
            })();
          })()), ((_a0, _a1) => [1,
          _a0,
          _a1])(Bosatsu_Properties$forall_Prop(Queue$rand_Queue_Int, _js_to_bosatsu_string("pop isomorphism"), q_3 => (() => {
              const _anon27 = Queue$unpush(q_3);
              return (() => {
                let _anon28;
                return (_anon27[0] === 1 && (() => {
                  _anon28 = _anon27[1];
                  return true;
                })()) ? (() => {
                    const h_2 = _anon28[0];
                    return (() => {
                      const t = _anon28[1];
                      return ((_a0, _a1) => [0,
                        _a0,
                        _a1])(Queue$eq_li(((_a0, _a1) => [1,
                            _a0,
                            _a1])(h_2, (() => {
                              const f_6 = t[0];
                              return (() => {
                                const b_6 = t[1];
                                return Bosatsu_Predef$concat(f_6, Bosatsu_Predef$reverse(b_6));
                              })();
                            })()), (() => {
                            const f_7 = q_3[0];
                            return (() => {
                              const b_7 = q_3[1];
                              return Bosatsu_Predef$concat(f_7, Bosatsu_Predef$reverse(b_7));
                            })();
                          })()), _js_to_bosatsu_string("pop non-empty"));
                    })();
                  })() : ((_a0, _a1) => [0, _a0, _a1])((() => {
                      const _anon26 = (() => {
                        const f_8 = q_3[0];
                        return (() => {
                          const b_8 = q_3[1];
                          return Bosatsu_Predef$concat(f_8, Bosatsu_Predef$reverse(b_8));
                        })();
                      })();
                      return (_anon26[0] === 0) ? [1] : [0];
                    })(), _js_to_bosatsu_string("empty is only unpush"));
              })();
            })()), [0])))));
var Queue$tests = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("queue tests"), ((_a0, _a1) => [1,
    _a0,
    _a1])(((_a0, _a1) => [0, _a0, _a1])((() => {
        const _anon31 = ((_a0, _a1) => [_a0, _a1])((() => {
            const _anon29 = Queue$unpush(Queue$q12);
            return (() => {
              let _anon30;
              return (_anon29[0] === 1 && (() => {
                _anon30 = _anon29[1];
                return true;
              })()) ? (() => {
                  const a = _anon30[0];
                  return (_a0 => [1, _a0])(a);
                })() : [0];
            })();
          })(), (_a0 => [1, _a0])(1));
        return (() => {
          let _anon33;
          return (() => {
            let _anon32;
            return ((() => {
              _anon33 = _anon31[1];
              return true;
            })() && ((() => {
              _anon32 = _anon31[0];
              return true;
            })() && (_anon32[0] === 1 && (_anon33[0] === 1)))) ? (() => {
                const a_1 = _anon32[1];
                return (() => {
                  const b = _anon33[1];
                  return (a_1 === b) ? [1] : [0];
                })();
              })() : (() => {
                let _anon35;
                return (() => {
                  let _anon34;
                  return ((() => {
                    _anon35 = _anon31[1];
                    return true;
                  })() && ((() => {
                    _anon34 = _anon31[0];
                    return true;
                  })() && (_anon34[0] === 0 && (_anon35[0] === 0)))) ? [1] : [0];
                })();
              })();
          })();
        })();
      })(), _js_to_bosatsu_string("1")), ((_a0, _a1) => [1,
      _a0,
      _a1])(((_a0, _a1) => [0, _a0, _a1])(((() => {
          const f = Queue$q12[0];
          return (() => {
            const b_1 = Queue$q12[1];
            return foldl_List(Bosatsu_Predef$reverse(b_1), foldl_List(f, 0, (_a0, _a1) => _a0 + _a1), (_a0, _a1) => _a0 + _a1);
          })();
        })() === 3) ? [1] : [0], _js_to_bosatsu_string("fold_Queue add")), ((_a0, _a1) => [1,
        _a0,
        _a1])(((_a0, _a1) => [0, _a0, _a1])(((() => {
            const f_1 = Queue$q12[0];
            return (() => {
              const b_2 = Queue$q12[1];
              return foldl_List(Bosatsu_Predef$reverse(b_2), foldl_List(f_1, 0, (a_2, x) => x), (a_3, x_1) => x_1);
            })();
          })() === 2) ? [1] : [0], _js_to_bosatsu_string("take the second")), ((_a0, _a1) => [1,
          _a0,
          _a1])(((_a0, _a1) => [0, _a0, _a1])(((() => {
              const f_2 = Queue$q12[0];
              return (() => {
                const b_3 = Queue$q12[1];
                return foldl_List(Bosatsu_Predef$reverse(b_3), foldl_List(f_2, 0, (x_2, a_4) => x_2), (x_3, a_5) => x_3);
              })();
            })() === 0) ? [1] : [0], _js_to_bosatsu_string("take the first")), ((_a0, _a1) => [1,
            _a0,
            _a1])(((_a0, _a1) => [0,
              _a0,
              _a1])(Queue$eq_qi(Queue$reverse_Queue((() => {
                    const f_3 = Queue$q12[0];
                    return (() => {
                      const b_4 = Queue$q12[1];
                      return ((_a0, _a1) => [_a0, _a1])(b_4, f_3);
                    })();
                  })()), Queue$q12), _js_to_bosatsu_string("reverse is idempotent")), ((_a0, _a1) => [1,
              _a0,
              _a1])(((_a0, _a1) => [0,
                _a0,
                _a1])(Queue$eq_qi(Queue$q12, Queue$from_List(((_a0, _a1) => [1,
                      _a0,
                      _a1])(1, ((_a0, _a1) => [1,
                        _a0,
                        _a1])(2, [0])))), _js_to_bosatsu_string("from list [1, 2]")), ((_a0, _a1) => [1,
                _a0,
                _a1])(((_a0, _a1) => [0, _a0, _a1])(Queue$eq_qi((() => {
                      const f_4 = Queue$q12[0];
                      return (() => {
                        const b_5 = Queue$q12[1];
                        return ((_a0, _a1) => [_a0,
                          _a1])(f_4, ((_a0, _a1) => [1, _a0, _a1])(3, b_5));
                      })();
                    })(), Queue$from_List(((_a0, _a1) => [1,
                        _a0,
                        _a1])(1, ((_a0, _a1) => [1,
                          _a0,
                          _a1])(2, ((_a0, _a1) => [1,
                            _a0,
                            _a1])(3, [0]))))), _js_to_bosatsu_string("from list [1, 2, 3]")), ((_a0, _a1) => [1,
                  _a0,
                  _a1])(((_a0, _a1) => [0,
                    _a0,
                    _a1])(Queue$eq_qi(Queue$empty_Queue, ((_a0, _a1) => [_a0,
                        _a1])([0], [0])), _js_to_bosatsu_string("empty_Queue == from_List([])")), ((_a0, _a1) => [1,
                    _a0,
                    _a1])(((_a0, _a1) => [0,
                      _a0,
                      _a1])(Queue$eq_qi(Queue$q12, Queue$from_List(((_a0, _a1) => [1,
                            _a0,
                            _a1])(1, ((_a0, _a1) => [1,
                              _a0,
                              _a1])(2, [0])))), _js_to_bosatsu_string("from list [1, 2]")), ((_a0, _a1) => [1,
                      _a0,
                      _a1])(((_a0, _a1) => [0,
                        _a0,
                        _a1])(Queue$eq_qi(Queue$pop(Queue$pop(Queue$pop(Queue$from_List(((_a0, _a1) => [1,
                                    _a0,
                                    _a1])(1, ((_a0, _a1) => [1,
                                      _a0,
                                      _a1])(2, ((_a0, _a1) => [1,
                                        _a0,
                                        _a1])(3, [0]))))))), Queue$empty_Queue), _js_to_bosatsu_string("pop to empty")), ((_a0, _a1) => [1,
                        _a0,
                        _a1])(((_a0, _a1) => [0, _a0, _a1])(Queue$eq_qi((() => {
                              const _anon36 = Queue$unpush(Queue$empty_Queue);
                              return (() => {
                                let _anon37;
                                return (_anon36[0] === 1 && (() => {
                                  _anon37 = _anon36[1];
                                  return true;
                                })()) ? _anon37[1] : Queue$empty_Queue;
                              })();
                            })(), Queue$empty_Queue), _js_to_bosatsu_string("pop empty is okay")), ((_a0, _a1) => [1,
                          _a0,
                          _a1])(((_a0, _a1) => [0,
                            _a0,
                            _a1])(Queue$eq_li(Queue$to_List(Queue$from_List(((_a0, _a1) => [1,
                                    _a0,
                                    _a1])(1, ((_a0, _a1) => [1,
                                      _a0,
                                      _a1])(1, ((_a0, _a1) => [1,
                                        _a0,
                                        _a1])(2, ((_a0, _a1) => [1,
                                          _a0,
                                          _a1])(2, ((_a0, _a1) => [1,
                                            _a0,
                                            _a1])(3, ((_a0, _a1) => [1,
                                              _a0,
                                              _a1])(3, [0])))))))), ((_a0, _a1) => [1,
                                _a0,
                                _a1])(1, ((_a0, _a1) => [1,
                                  _a0,
                                  _a1])(1, ((_a0, _a1) => [1,
                                    _a0,
                                    _a1])(2, ((_a0, _a1) => [1,
                                      _a0,
                                      _a1])(2, ((_a0, _a1) => [1,
                                        _a0,
                                        _a1])(3, ((_a0, _a1) => [1,
                                          _a0,
                                          _a1])(3, [0]))))))), _js_to_bosatsu_string("to/from List")), ((_a0, _a1) => [1,
                            _a0,
                            _a1])(Bosatsu_Properties$run_Prop(Queue$queue_laws, 100, 4242), [0]))))))))))))));
_tests["Queue::tests"] = Queue$tests;