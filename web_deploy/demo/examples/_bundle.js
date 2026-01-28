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

// char_to_String - char is already a single-char string (identity function)
var _char_to_String = (c) => c;

// trace - log message and return value
var _trace = (msg, value) => {
  console.log(_bosatsu_to_js_string(msg));
  return value;
};

// cmp_String - compare two Bosatsu strings, return [0] (LT), [1] (EQ), or [2] (GT)
// Returns boxed values for pattern matching consistency with cmp_Int
var _cmp_String = (a, b) => {
  const sa = _bosatsu_to_js_string(a);
  const sb = _bosatsu_to_js_string(b);
  return sa < sb ? [0] : (sa === sb ? [1] : [2]);
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
globalThis._tests = {};

// Generated code
globalThis.Bosatsu_Predef$build_List = fn => fn((_a0, _a1) => [1, _a0, _a1], [0]);
globalThis.Bosatsu_Predef$foldr_List = (list, fn, acc) => (() => {
  const loop_1 = (_slots => function loop(list_1) {
    return (list_1[0] === 0) ? _slots[0] : (() => {
        const h = list_1[1];
        return (() => {
          const t = list_1[2];
          return _slots[1](h, loop(t));
        })();
      })();
  })([acc, fn]);
  return loop_1(list);
})();
globalThis.Bosatsu_Predef$reverse_concat = (front, back) => foldl_List(front, back, (tail, h) => ((_a0, _a1) => [1,
    _a0,
    _a1])(h, tail));
globalThis.Bosatsu_Predef$reverse = as => Bosatsu_Predef$reverse_concat(as, [0]);
globalThis.Bosatsu_Predef$concat = (front, back) => (back[0] === 0) ? front : Bosatsu_Predef$reverse_concat(Bosatsu_Predef$reverse_concat(front, [0]), back);
globalThis.Bosatsu_Predef$map_List = (lst, fn) => Bosatsu_Predef$reverse(foldl_List(lst, [0], (_slots => (t, a) => ((_a0, _a1) => [1,
      _a0,
      _a1])(_slots[0](a), t))([fn])));
globalThis.Bosatsu_Predef$replicate_List = (item, cnt) => _int_loop(cnt, [0], (_slots => (i, acc) => ((_a0, _a1) => [_a0,
    _a1])((-1) + i, ((_a0, _a1) => [1, _a0, _a1])(_slots[0], acc)))([item]));
globalThis.Bosatsu_Predef$uncurry2 = f => (_slots => (x1, x2) => _slots[0](x1)(x2))([f]);
globalThis.Bosatsu_Predef$uncurry3 = f => (_slots => (x1, x2, x3) => _slots[0](x1)(x2)(x3))([f]);
globalThis.Bosatsu_Predef$range_fold = (inclusiveLower, exclusiveUpper, init, fn) => (() => {
  const diff = exclusiveUpper - inclusiveLower;
  return _int_loop(diff, init, (_slots => (diff0, a) => ((_a0, _a1) => [_a0,
      _a1])((-1) + diff0, _slots[0](a, _slots[1] - diff0)))([fn,
        exclusiveUpper]));
})();
globalThis.Bosatsu_Predef$string_Order = (_a0 => [_a0])((_a0, _a1) => _cmp_String(_a0, _a1));
globalThis.Bosatsu_Predef$rotation = (left, right, max_diff) => (() => {
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
globalThis.Bosatsu_Predef$branch = (sz, item, left, right) => (() => {
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
globalThis.Bosatsu_Predef$branch_s = (item, left, right) => Bosatsu_Predef$branch(1 + (((left[0] === 0) ? 0 : left[1]) + ((right[0] === 0) ? 0 : right[1])), item, left, right);
globalThis.Bosatsu_Predef$balance = t => (t[0] === 0) ? [0] : (() => {
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
globalThis.Bosatsu_Predef$add_item = (ord, tree, item) => (() => {
  const fn = ord[0];
  return (() => {
    const loop_1 = (_slots => function loop(tree_1) {
      return (tree_1[0] === 0) ? ((_a0, _a1, _a2, _a3, _a4) => [1,
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
        })();
    })([fn, item]);
    return loop_1(tree);
  })();
})();
globalThis.Bosatsu_Predef$contains = (ord, tree, item) => (() => {
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
globalThis.Bosatsu_Predef$remove_item = (ord, tree, item) => (() => {
  const fn = ord[0];
  return (() => {
    const loop_1 = (_slots => function loop(tree_1) {
      return (tree_1[0] === 0) ? [0] : (() => {
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
        })();
    })([fn, item]);
    return loop_1(tree);
  })();
})();
globalThis.Bosatsu_Predef$fold_right_Tree = function fold_right_Tree(t, right_v, fn) {
  return (t[0] === 0) ? right_v : (() => {
      const key = t[3];
      return (() => {
        const left = t[4];
        return (() => {
          const right = t[5];
          return fold_right_Tree(left, fn(key, fold_right_Tree(right, right_v, fn)), fn);
        })();
      })();
    })();
};
globalThis.Bosatsu_Predef$empty_Dict = comp => (() => {
  const fn = comp[0];
  return ((_a0, _a1) => [_a0, _a1])((_a0 => [_a0])((_slots => (a, b) => (() => {
        const k1 = a[0];
        return (() => {
          const k2 = b[0];
          return _slots[0](k1, k2);
        })();
      })())([fn])), [0]);
})();
globalThis.Bosatsu_Predef$add_key = (dict, key, value) => (() => {
  const ord = dict[0];
  return (() => {
    const tree = dict[1];
    return ((_a0, _a1) => [_a0,
      _a1])(ord, Bosatsu_Predef$add_item(ord, tree, ((_a0, _a1) => [_a0,
          _a1])(key, value)));
  })();
})();
globalThis.Bosatsu_Predef$get_key = (dict, key) => (() => {
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
globalThis.Bosatsu_Predef$remove_key = (dict, key) => (() => {
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
globalThis.Bosatsu_Predef$items = dict => (() => {
  const tree = dict[1];
  return Bosatsu_Predef$fold_right_Tree(tree, [0], (_a0, _a1) => [1, _a0, _a1]);
})();
globalThis.Bosatsu_Predef$clear_Dict = dict => (() => {
  const ord = dict[0];
  return ((_a0, _a1) => [_a0, _a1])(ord, [0]);
})();
globalThis.Bosatsu_Prog$map = (prog, fn) => Bosatsu_Prog$flat_map(prog, (_slots => res => Bosatsu_Prog$pure(_slots[0](res)))([fn]));
globalThis.Bosatsu_Prog$map_err = (prog, fn) => Bosatsu_Prog$recover(prog, (_slots => res => Bosatsu_Prog$raise_error(_slots[0](res)))([fn]));
globalThis.Bosatsu_Prog$with_env = (p, env) => Bosatsu_Prog$remap_env(p, (_slots => a => _slots[0])([env]));
globalThis.Bosatsu_Prog$ignore_env = p => Bosatsu_Prog$remap_env(p, a => []);
globalThis.Bosatsu_Prog$_await = p => (_slots => fn => Bosatsu_Prog$flat_map(_slots[0], fn))([p]);
globalThis.Bosatsu_Prog$recursive = fn => (_slots => a => Bosatsu_Prog$apply_fix(a, _slots[0]))([fn]);
globalThis.Bosatsu_Prog$unit = Bosatsu_Prog$pure([]);
globalThis.Bosatsu_Prog$count_down = a => Bosatsu_Prog$apply_fix(a, loop => (_slots => i => (() => {
    const _anon0 = (i < 0) ? [0] : (i === 0) ? [1] : [2];
    return (_anon0[0] === 1) ? Bosatsu_Prog$println(_js_to_bosatsu_string("\ndone")) : (_anon0[0] === 0) ? Bosatsu_Prog$println(_js_to_bosatsu_string("\ndone")) : Bosatsu_Prog$flat_map(Bosatsu_Prog$print(_concat_String(((_a0, _a1) => [1,
                _a0,
                _a1])(_int_to_String(i), ((_a0, _a1) => [1,
                  _a0,
                  _a1])(_js_to_bosatsu_string(", "), [0])))), (_slots => a_1 => _slots[0]((-1) + _slots[1]))([_slots[0],
              i]));
  })())([loop]));
globalThis.Bosatsu_Prog$to_run = (_a0 => [_a0])(Bosatsu_Prog$flat_map(Bosatsu_Prog$read_env, args => Bosatsu_Prog$ignore_env((() => {
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
globalThis.Demo_Compute$fib = n => (() => {
  const _anon0 = _int_loop(n, ((_a0, _a1) => [_a0,
      _a1])(0, 1), (i, acc) => (() => {
      const a = acc[0];
      return (() => {
        const b = acc[1];
        return ((_a0, _a1) => [_a0, _a1])((-1) + i, ((_a0, _a1) => [_a0,
            _a1])(b, a + b));
      })();
    })());
  return _anon0[0];
})();
globalThis.Demo_Compute$factorial = n => _int_loop(n, 1, (i, acc) => ((_a0, _a1) => [_a0, _a1])((-1) + i, acc * i));
globalThis.Demo_Compute$collatz_single = n => (() => {
  const _anon3 = _int_loop(1000, ((_a0, _a1) => [_a0,
      _a1])(n, 0), (remaining, state) => (() => {
      const value = state[0];
      return (() => {
        const steps = state[1];
        return (() => {
          const _anon2 = (value < 1) ? [0] : (value === 1) ? [1] : [2];
          return (_anon2[0] === 2) ? ((_a0, _a1) => [_a0,
              _a1])((-1) + remaining, ((_a0, _a1) => [_a0, _a1])((() => {
                  const _anon1 = 2 ? value % 2 : value;
                  return (_anon1 === 0) ? 2 ? Math.trunc(value / 2) : 0 : 1 + (value + (value + value));
                })(), 1 + steps)) : ((_a0, _a1) => [_a0,
              _a1])((-1) + remaining, ((_a0, _a1) => [_a0, _a1])(value, steps));
        })();
      })();
    })());
  return _anon3[1];
})();
globalThis.Demo_Compute$collatz_steps = n => _int_loop(n, 0, (_slots => (i, max_steps) => (() => {
    const steps = Demo_Compute$collatz_single(1 + (_slots[0] - i));
    return ((_a0, _a1) => [_a0, _a1])((-1) + i, (() => {
        const _anon4 = (steps < max_steps) ? [0] : (steps === max_steps) ? [1] : [2];
        return (_anon4[0] === 2) ? steps : max_steps;
      })());
  })())([n]));
globalThis.Demo_Compute$main = (_a0 => [_a0])(Bosatsu_Prog$ignore_env(Bosatsu_Prog$pure(0)));
globalThis.Demo_Orchestrator$validate_fib_input = n => (() => {
  const _anon1 = (n < 0) ? [0] : (n === 0) ? [1] : [2];
  return (_anon1[0] === 0) ? [0] : (() => {
      const _anon0 = (n < 40) ? [0] : (n === 40) ? [1] : [2];
      return (_anon0[0] === 2) ? [0] : [1];
    })();
})();
globalThis.Demo_Orchestrator$validate_fact_input = n => (() => {
  const _anon3 = (n < 0) ? [0] : (n === 0) ? [1] : [2];
  return (_anon3[0] === 0) ? [0] : (() => {
      const _anon2 = (n < 20) ? [0] : (n === 20) ? [1] : [2];
      return (_anon2[0] === 2) ? [0] : [1];
    })();
})();
globalThis.Demo_Orchestrator$compute_fib_sequence = n => Bosatsu_Predef$map_List(range(1 + n), i => ((_a0, _a1) => [_a0,
    _a1])(i, Demo_Compute$fib(i)));
globalThis.Demo_Orchestrator$compute_factorial_table = n => Bosatsu_Predef$map_List(range(1 + n), i => ((_a0, _a1) => [_a0,
    _a1])(i, Demo_Compute$factorial(i)));