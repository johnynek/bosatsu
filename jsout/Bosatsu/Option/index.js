// Import runtime
// require("./../../_runtime.js") or import from "./../../_runtime.js"

const eq_Option = eq => (_slots => (left, right) => (() => {
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
export {eq_Option};