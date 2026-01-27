// Import runtime
// require("./../../_runtime.js") or import from "./../../_runtime.js"

const str_to_char_tests = ((_a0, _a1) => [1,
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
const length_String = s => (() => {
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
const len_test = ((_a0, _a1) => [1,
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
const last_tests = ((_a0, _a1) => [1,
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
const partition_tests = ((_a0, _a1) => [1,
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
const match_tests = ((_a0, _a1) => [1,
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
const glob_match_tests = ((_a0, _a1) => [1,
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
const tests = ((_a0, _a1) => [1,
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
export {str_to_char_tests};