// Import runtime
// require("./../_runtime.js") or import from "./../_runtime.js"

const refl = (_a0 => [_a0])(x => x);
const map_Build = (b, fn) => (_a0 => [2, _a0])(((_a0, _a1) => [_a0,
    _a1])(b, fn));
const map2_Build = (ba, bb, fn) => (_a0 => [3, _a0])(((_a0, _a1, _a2) => [_a0,
    _a1,
    _a2])(ba, bb, fn));
const file = s => ((_a0, _a1) => [0, _a0, _a1])(s, BuildLibrary$refl);
const empty = (_a0 => [1, _a0])([0]);
const build_all = items => (items[0] === 0) ? BuildLibrary$empty : (() => {
    const h = items[1];
    return (() => {
      const t = items[2];
      return (_a0 => [3, _a0])(((_a0, _a1, _a2) => [_a0,
          _a1,
          _a2])(h, build_all(t), (_a0, _a1) => [1, _a0, _a1]));
    })();
  })();
const files = fs => BuildLibrary$build_all(Bosatsu_Predef$map_List(fs, f => ((_a0, _a1) => [0,
      _a0,
      _a1])(f, BuildLibrary$refl)));
const library = (sources, deps) => ((_a0, _a1, _a2) => [4,
  _a0,
  _a1,
  _a2])(sources, deps, BuildLibrary$refl);
const build = args => (() => {
  const srcs = args[0];
  return (() => {
    const deps = args[1];
    return ((_a0, _a1, _a2) => [4,
      _a0,
      _a1,
      _a2])(srcs, deps, BuildLibrary$refl);
  })();
})();
export {refl};