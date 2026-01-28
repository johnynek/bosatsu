// Import runtime
// require("./../../../_runtime.js") or import from "./../../../_runtime.js"

const applicative_from_pure_map_product = (pure, map, product) => ((_a0, _a1, _a2, _a3, _a4) => [_a0,
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
const applicative_from_pure_ap = (pure, ap) => ((_a0, _a1, _a2, _a3, _a4) => [_a0,
  _a1,
  _a2,
  _a3,
  _a4])(pure, (_slots => (fn, fa) => _slots[0](_slots[1](fn), fa))([ap,
      pure]), ap, (_slots => (fa_1, fb, fn_1) => _slots[0](_slots[0](_slots[1]((_slots => a => (_slots => b => _slots[0](_slots[1], b))([_slots[0],
            a]))([fn_1])), fa_1), fb))([ap,
      pure]), (_slots => (fa_2, fb_1) => _slots[0](_slots[0](_slots[1](a_1 => (_slots => b_1 => ((_a0, _a1) => [_a0,
          _a1])(_slots[0], b_1))([a_1])), fa_2), fb_1))([ap, pure]));
const applicative_Option = Bosatsu_Example_ApplicativeTraverse$applicative_from_pure_map_product(_a0 => [1,
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
const trav_l = (app, fn, lst) => (() => {
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
const traverse_List = (_a0 => [_a0])(Bosatsu_Example_ApplicativeTraverse$trav_l);
const eq_opt_list_int = Bosatsu_Option$eq_Option(Bosatsu_List$eq_List((_a0, _a1) => (_a0 === _a1) ? [1] : [0]));
const test = ((_a0, _a1) => [1,
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
export {applicative_from_pure_map_product};