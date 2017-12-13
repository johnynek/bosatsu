load("@io_bazel_rules_scala//scala:scala.bzl", upstream_scala_library = "scala_library", "scala_binary", "scala_test", "scala_repl")

_plugins = ["@org_spire_math_kind_projector_2_11//jar:file"]

def strict_scalacopts():
  return [
    "-Xfatal-warnings",
    "-Xfuture",
    "-Xlint",
    "-Xlint:unsound-match",            
    "-Yno-adapted-args",                
    "-Ypartial-unification",            
    "-Ywarn-unused-import",  # macros can cause this to kill the compiler, see: https://github.com/scala/pickling/issues/370
    "-Ywarn-value-discard",
    "-language:existentials",            # Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros",     # Allow macro definition (besides implementation and application)
    "-language:higherKinds",             # Allow higher-kinded types
    "-deprecation",
    "-feature",
    "-unchecked",
  ]

def scala_library(name, srcs = [], deps = [], runtime_deps = [], data = [], resources = [], resource_strip_prefix = "",
                  scalacopts = strict_scalacopts(), jvm_flags = [], main_class = "", javacopts=[],exports = [], visibility = None):
    upstream_scala_library(name = name, srcs = srcs, deps = deps, runtime_deps = runtime_deps,
                  plugins = _plugins,
                  resources = resources, resource_strip_prefix = resource_strip_prefix, scalacopts = scalacopts,javacopts=javacopts,
                  jvm_flags = jvm_flags, main_class = main_class, exports = exports, visibility = visibility)

def _name_to_target(s):
    res_array = []
    for c in s:
        if c.isalnum() or c == ".":
            res_array.append(c)
        else:
            res_array.append("_")
    return "_target_" + "".join(res_array)

def scala_module(name, all_srcs, srcs_deps = {}, deps = [], runtime_deps = [], data = [], resources = [], resource_strip_prefix = "",
                  scalacopts = strict_scalacopts(), jvm_flags = [], main_class = "", javacopts=[],exports = [], visibility = None):
    ts = []
    for (src, src_ds) in srcs_deps.items():
      these_deps = [":" + _name_to_target(d) for d in src_ds] + deps
      n = _name_to_target(src)
      upstream_scala_library(name = n, srcs = [src], deps = these_deps, runtime_deps = [],
                    plugins = _plugins, resources = [], scalacopts = scalacopts, javacopts=javacopts,
                    jvm_flags = jvm_flags, exports = these_deps)

      ts.append(":" + n)

    top_srcs = [s for s in all_srcs if s not in srcs_deps]
    upstream_scala_library(name = name, srcs = top_srcs, deps = deps + ts, runtime_deps = runtime_deps,
                  plugins = _plugins,
                  resources = resources, resource_strip_prefix = resource_strip_prefix, scalacopts = scalacopts, javacopts=javacopts,
                  jvm_flags = jvm_flags, main_class = main_class, exports = exports + ts)

