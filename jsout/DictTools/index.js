// Import runtime
// require("./../_runtime.js") or import from "./../_runtime.js"

const merge = (left, right, fn) => foldl_List(Bosatsu_Predef$items(right), left, (_slots => (d, a) => (() => {
    const k = a[0];
    return (() => {
      const v = a[1];
      return (() => {
        const _anon0 = Bosatsu_Predef$get_key(d, k);
        return (_anon0[0] === 0) ? Bosatsu_Predef$add_key(d, k, v) : (() => {
            const v0 = _anon0[1];
            return Bosatsu_Predef$add_key(d, k, _slots[0](v0, v));
          })();
      })();
    })();
  })())([fn]));
export {merge};