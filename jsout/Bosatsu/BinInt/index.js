// Import runtime
// require("./../../_runtime.js") or import from "./../../_runtime.js"

const cmp = (a, b) => (a[0] === 0) ? (() => {
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
const eq = (a, b) => (a[0] === 0) ? (() => {
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
const binNat_to_BinInt = _a0 => [0, _a0];
const not = bi => (bi[0] === 0) ? (() => {
    const b = bi[1];
    return (_a0 => [1, _a0])(b);
  })() : (() => {
    const b_1 = bi[1];
    return (_a0 => [0, _a0])(b_1);
  })();
const int_to_BinInt = i => (() => {
  const _anon0 = (i < 0) ? [0] : (i === 0) ? [1] : [2];
  return (_anon0[0] === 0) ? (_a0 => [1,
      _a0])(Bosatsu_BinNat$toBinNat(~i)) : (_a0 => [0,
      _a0])(Bosatsu_BinNat$toBinNat(i));
})();
const binInt_to_Int = bi => (bi[0] === 0) ? (() => {
    const bn = bi[1];
    return Bosatsu_BinNat$toInt(bn);
  })() : (() => {
    const x = bi[1];
    return ~Bosatsu_BinNat$toInt(x);
  })();
const negate = bi => (() => {
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
const abs = bi => (bi[0] === 0) ? bi[1] : (() => {
    const x = bi[1];
    return Bosatsu_BinNat$next(x);
  })();
const add = (x, y) => (() => {
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
const sub = (a, b) => Bosatsu_BinInt$add(a, (() => {
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
export {cmp};