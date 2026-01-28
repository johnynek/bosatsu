// Import runtime
// require("./../../_runtime.js") or import from "./../../_runtime.js"

const fib = n => foldl_List(range(n), [0], (revFib, a) => (revFib[0] === 0) ? ((_a0, _a1) => [1,
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
const computed = Bosatsu_List$sum(flat_map_List(Euler_Two$fib(35), f => (() => {
      const _anon3 = (() => {
        const _anon2 = (f < 4000000) ? [0] : (f === 4000000) ? [1] : [2];
        return (_anon2[0] === 2) ? [0] : ((2 ? f % 2 : f) === 0) ? [1] : [0];
      })();
      return (_anon3[0] === 1) ? ((_a0, _a1) => [1, _a0, _a1])(f, [0]) : [0];
    })()));
const test = ((_a0, _a1) => [0,
  _a0,
  _a1])((Euler_Two$computed === 4613732) ? [1] : [0], _js_to_bosatsu_string("expected 4613732"));
export {fib};