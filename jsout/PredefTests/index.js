// Import runtime
// require("./../_runtime.js") or import from "./../_runtime.js"

const oi = opt => (opt[0] === 0) ? _js_to_bosatsu_string("None") : (() => {
    const v = opt[1];
    return _concat_String(((_a0, _a1) => [1,
        _a0,
        _a1])(_js_to_bosatsu_string("Some("), ((_a0, _a1) => [1,
          _a0,
          _a1])(_int_to_String(v), ((_a0, _a1) => [1,
            _a0,
            _a1])(_js_to_bosatsu_string(")"), [0]))));
  })();
const test_int = ((_a0, _a1) => [1,
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
const test_string = ((_a0, _a1) => [1,
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
const test = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("Predef tests"), ((_a0, _a1) => [1,
    _a0,
    _a1])(PredefTests$test_int, ((_a0, _a1) => [1,
      _a0,
      _a1])(PredefTests$test_string, [0])));
export {oi};