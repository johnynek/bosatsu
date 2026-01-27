// Import runtime
// require("./../../_runtime.js") or import from "./../../_runtime.js"

const eq_Pair = (eq_a, eq_b) => (_slots => (a, b) => (() => {
  const l1 = a[0];
  return (() => {
    const l2 = a[1];
    return (() => {
      const r1 = b[0];
      return (() => {
        const r2 = b[1];
        return (() => {
          const _anon0 = _slots[0](l1, r1);
          return (_anon0[0] === 1) ? _slots[1](l2, r2) : [0];
        })();
      })();
    })();
  })();
})())([eq_a, eq_b]);
const eq_Dict = (eq_key, eq_value) => (_slots => (left, right) => Bosatsu_List$eq_List((_slots => (a, b) => (() => {
    const l1 = a[0];
    return (() => {
      const l2 = a[1];
      return (() => {
        const r1 = b[0];
        return (() => {
          const r2 = b[1];
          return (() => {
            const _anon1 = _slots[0](l1, r1);
            return (_anon1[0] === 1) ? _slots[1](l2, r2) : [0];
          })();
        })();
      })();
    })();
  })())([_slots[0],
      _slots[1]]))(Bosatsu_Predef$items(left), Bosatsu_Predef$items(right)))([eq_key,
    eq_value]);
export {eq_Pair};