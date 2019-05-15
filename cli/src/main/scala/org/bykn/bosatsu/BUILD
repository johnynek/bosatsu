load("@io_bazel_rules_scala//scala:scala.bzl", "scala_binary", "scala_test", "scala_repl")
load("@org_bykn_bosatsu//tools:scala.bzl", "scala_library", "scala_module", "strict_scalacopts")

package(default_visibility = ["//visibility:public"])

common_deps = [
      "//core/src/main/scala/org/bykn/bosatsu:bosatsu",
      "//3rdparty/jvm/org/bykn:fastparse_cats_core",
      "//3rdparty/jvm/org/typelevel:alleycats_core",
      "//3rdparty/jvm/org/typelevel:cats_core",
      "//3rdparty/jvm/org/typelevel:cats_kernel",
      "//3rdparty/jvm/org/typelevel:cats_macros",
      "//3rdparty/jvm/org/typelevel:paiges_core",
      "//3rdparty/jvm/com/lihaoyi:fastparse",
      "//3rdparty/jvm/com/lihaoyi:fastparse_utils",
      "//3rdparty/jvm/com/monovore:decline",
      "//3rdparty/jvm/com/stripe:dagon_core",
  ]

scala_library(
    name = "bosatsu",
    srcs = glob(["**/*.scala"], exclude=["Main.scala"]),
    deps = common_deps + ["//cli/src/main/protobuf/bosatsu"])

scala_binary(
    name = "bosatsu_main",
    srcs = ["Main.scala"],
    deps = [":bosatsu"] + common_deps,
    runtime_deps = [],
    main_class = "org.bykn.bosatsu.Main",
    scalacopts = strict_scalacopts(),
    visibility = ["//visibility:public"],
  )
