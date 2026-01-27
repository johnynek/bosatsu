// Import runtime
// require("./../_runtime.js") or import from "./../_runtime.js"

const combine = _concat_String(((_a0, _a1) => [1,
    _a0,
    _a1])(_js_to_bosatsu_string("foo: "), ((_a0, _a1) => [1,
      _a0,
      _a1])(_js_to_bosatsu_string("this is foo"), ((_a0, _a1) => [1,
        _a0,
        _a1])(_js_to_bosatsu_string(" bar: "), ((_a0, _a1) => [1,
          _a0,
          _a1])(_js_to_bosatsu_string("this is bar"), [0])))));
const fb = (() => {
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
const test0 = ((_a0, _a1) => [0,
  _a0,
  _a1])(PatternExamples$fb, _js_to_bosatsu_string("foo-bar match"));
const get_foos = s => (() => {
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
          return ((_a0, _a1) => [1, _a0, _a1])(foo, get_foos(rest));
        })();
      })() : [0];
  })();
})();
const test1 = (() => {
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
const test2 = ((_a0, _a1) => [0,
  _a0,
  _a1])([1], _js_to_bosatsu_string("test unnamed match"));
const tests = ((_a0, _a1) => [1,
  _a0,
  _a1])(_js_to_bosatsu_string("PatternExamples"), ((_a0, _a1) => [1,
    _a0,
    _a1])(PatternExamples$test0, ((_a0, _a1) => [1,
      _a0,
      _a1])(PatternExamples$test1, ((_a0, _a1) => [1,
        _a0,
        _a1])(PatternExamples$test2, [0]))));
export {combine};