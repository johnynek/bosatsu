// Import runtime
// require("./../_runtime.js") or import from "./../_runtime.js"

const maven_central = ((_a0, _a1, _a2) => [_a0,
  _a1,
  _a2])(_js_to_bosatsu_string("mavencentral"), _js_to_bosatsu_string("default"), _js_to_bosatsu_string("https://repo.maven.apache.org/maven2/"));
const default_options_with_scala = ((_a0, _a1, _a2, _a3, _a4, _a5) => [_a0,
  _a1,
  _a2,
  _a3,
  _a4,
  _a5])(((_a0, _a1) => [1,
    _a0,
    _a1])(_js_to_bosatsu_string("load(\"@io_bazel_rules_scala//scala:scala_import.bzl\", \"scala_import\")"), [0]), ((_a0, _a1) => [1,
    _a0,
    _a1])(_js_to_bosatsu_string("java"), ((_a0, _a1) => [1,
      _a0,
      _a1])(_js_to_bosatsu_string("scala:2.11.11"), [0])), ((_a0, _a1) => [1,
    _a0,
    _a1])(BazelDepsApi$maven_central, [0]), _js_to_bosatsu_string("runtime_deps"), _js_to_bosatsu_string("coursier"), _js_to_bosatsu_string("highest"));
const scala_dep = (orgname, artifact, version) => Bosatsu_Predef$add_key(Bosatsu_Predef$empty_Dict(Bosatsu_Predef$string_Order), orgname, Bosatsu_Predef$add_key(Bosatsu_Predef$empty_Dict(Bosatsu_Predef$string_Order), artifact, ((_a0, _a1, _a2, _a3) => [_a0,
      _a1,
      _a2,
      _a3])([0], _js_to_bosatsu_string("scala"), version, [0])));
const scala_dep_modules = (orgname, artifact, modules, version, _exports) => Bosatsu_Predef$add_key(Bosatsu_Predef$empty_Dict(Bosatsu_Predef$string_Order), orgname, Bosatsu_Predef$add_key(Bosatsu_Predef$empty_Dict(Bosatsu_Predef$string_Order), artifact, ((_a0, _a1, _a2, _a3) => [_a0,
      _a1,
      _a2,
      _a3])(modules, _js_to_bosatsu_string("scala"), version, _exports)));
const merge_artifact = (left, right) => (() => {
  const lm = left[0];
  return (() => {
    const le = left[3];
    return (() => {
      const rm = right[0];
      return (() => {
        const rr = right[1];
        return (() => {
          const rv = right[2];
          return (() => {
            const re = right[3];
            return ((_a0, _a1, _a2, _a3) => [_a0,
              _a1,
              _a2,
              _a3])(Bosatsu_Predef$concat(lm, rm), rr, rv, Bosatsu_Predef$concat(le, re));
          })();
        })();
      })();
    })();
  })();
})();
const merge_deps = (left, right) => DictTools$merge(left, right, (l, r) => DictTools$merge(l, r, BazelDepsApi$merge_artifact));
const merge_dep_List = deps => foldl_List(deps, Bosatsu_Predef$empty_Dict(Bosatsu_Predef$string_Order), BazelDepsApi$merge_deps);
const standard_scala_replacements = Bosatsu_Predef$add_key(Bosatsu_Predef$add_key(Bosatsu_Predef$empty_Dict(Bosatsu_Predef$string_Order), _js_to_bosatsu_string("org.scala-lang.modules"), Bosatsu_Predef$add_key(Bosatsu_Predef$add_key(Bosatsu_Predef$empty_Dict(Bosatsu_Predef$string_Order), _js_to_bosatsu_string("scala-xml"), ((_a0, _a1) => [_a0,
          _a1])(_js_to_bosatsu_string("scala"), _js_to_bosatsu_string("@io_bazel_rules_scala_scala_xml//:io_bazel_rules_scala_scala_xml"))), _js_to_bosatsu_string("scala-parser-combinators"), ((_a0, _a1) => [_a0,
        _a1])(_js_to_bosatsu_string("scala"), _js_to_bosatsu_string("@io_bazel_rules_scala_scala_parser_combinators//:io_bazel_rules_scala_scala_parser_combinators")))), _js_to_bosatsu_string("org.scala-lang"), Bosatsu_Predef$add_key(Bosatsu_Predef$add_key(Bosatsu_Predef$add_key(Bosatsu_Predef$empty_Dict(Bosatsu_Predef$string_Order), _js_to_bosatsu_string("scala-reflect"), ((_a0, _a1) => [_a0,
          _a1])(_js_to_bosatsu_string("scala/unmangled"), _js_to_bosatsu_string("@io_bazel_rules_scala_scala_reflect//:io_bazel_rules_scala_scala_reflect"))), _js_to_bosatsu_string("scala-library"), ((_a0, _a1) => [_a0,
        _a1])(_js_to_bosatsu_string("scala/unmangled"), _js_to_bosatsu_string("@io_bazel_rules_scala_scala_library//:io_bazel_rules_scala_scala_library"))), _js_to_bosatsu_string("scala-compiler"), ((_a0, _a1) => [_a0,
      _a1])(_js_to_bosatsu_string("scala/unmangled"), _js_to_bosatsu_string("@io_bazel_rules_scala_scala_compiler//:io_bazel_rules_scala_scala_compiler"))));
export {maven_central};