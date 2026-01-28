// Import runtime
// require("./../_runtime.js") or import from "./../_runtime.js"

const empty_Queue = ((_a0, _a1) => [_a0, _a1])([0], [0]);
const from_List = list => ((_a0, _a1) => [_a0, _a1])(list, [0]);
const push = (a, item) => (() => {
  const f = a[0];
  return (() => {
    const b = a[1];
    return ((_a0, _a1) => [_a0, _a1])(f, ((_a0, _a1) => [1,
        _a0,
        _a1])(item, b));
  })();
})();
const unpush = queue => (() => {
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
const pop_value = queue => (() => {
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
const pop = queue => (() => {
  const _anon5 = Queue$unpush(queue);
  return (() => {
    let _anon6;
    return (_anon5[0] === 1 && (() => {
      _anon6 = _anon5[1];
      return true;
    })()) ? _anon6[1] : Queue$empty_Queue;
  })();
})();
const fold_Queue = (a, init, fold_fn) => (() => {
  const f = a[0];
  return (() => {
    const b = a[1];
    return foldl_List(Bosatsu_Predef$reverse(b), foldl_List(f, init, fold_fn), fold_fn);
  })();
})();
const reverse_Queue = a => (() => {
  const f = a[0];
  return (() => {
    const b = a[1];
    return ((_a0, _a1) => [_a0, _a1])(b, f);
  })();
})();
const eq_Queue = eq_fn => (_slots => (left, right) => (() => {
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
const to_List = a => (() => {
  const f = a[0];
  return (() => {
    const b = a[1];
    return Bosatsu_Predef$concat(f, Bosatsu_Predef$reverse(b));
  })();
})();
const eq_qi = Queue$eq_Queue((_a0, _a1) => (_a0 === _a1) ? [1] : [0]);
const eq_li = Bosatsu_List$eq_List((_a0, _a1) => (_a0 === _a1) ? [1] : [0]);
const q12 = (() => {
  const _anon16 = Queue$push(Queue$empty_Queue, 1);
  return (() => {
    const f = _anon16[0];
    return (() => {
      const b = _anon16[1];
      return ((_a0, _a1) => [_a0, _a1])(f, ((_a0, _a1) => [1, _a0, _a1])(2, b));
    })();
  })();
})();
const rand_int = Bosatsu_Rand$int_range(128);
const rand_geo_List_Int = Bosatsu_Rand$flat_map_Rand(Bosatsu_Rand$geometric_Int, len => Bosatsu_Rand$sequence_Rand(Bosatsu_Predef$replicate_List(Queue$rand_int, len)));
const queue_from_list = Bosatsu_Rand$map_Rand(Queue$rand_geo_List_Int, Queue$from_List);
const rand_Queue_depth = depth => (depth === 0) ? Queue$queue_from_list : (() => {
    let _anon17;
    return (() => {
      (() => {
        _anon17 = depth - 1;
        return true;
      })();
      return (() => {
        const n = _anon17;
        return (() => {
          const smaller = rand_Queue_depth(n);
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
const rand_Queue_Int = Queue$rand_Queue_depth(Bosatsu_Nat$to_Nat(50));
const show_List = (lst, showa) => (() => {
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
const show_Queue = (q, showa) => (() => {
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
const queue_laws = Bosatsu_Properties$suite_Prop(_js_to_bosatsu_string("queue properties"), ((_a0, _a1) => [1,
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
const tests = ((_a0, _a1) => [1,
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
export {empty_Queue};