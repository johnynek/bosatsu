// Import runtime
// require("./../_runtime.js") or import from "./../_runtime.js"

const lib1 = BuildLibrary$library(BuildLibrary$files(((_a0, _a1) => [1,
      _a0,
      _a1])(_js_to_bosatsu_string("file1.bosatsu"), ((_a0, _a1) => [1,
        _a0,
        _a1])(_js_to_bosatsu_string("file2.bosatsu"), [0]))), BuildLibrary$empty);
const lib2 = BuildLibrary$build(((_a0, _a1) => [_a0,
    _a1])(BuildLibrary$files([0]), BuildLibrary$build_all(((_a0, _a1) => [1,
        _a0,
        _a1])(BuildExample$lib1, [0]))));
export {lib1};