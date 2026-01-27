// Import runtime
// require("./../_runtime.js") or import from "./../_runtime.js"

const res0 = (() => {
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
const res1 = (() => {
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
const res3 = (() => {
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
const res4 = (() => {
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
const test = ((_a0, _a1) => [1,
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
export {res0};