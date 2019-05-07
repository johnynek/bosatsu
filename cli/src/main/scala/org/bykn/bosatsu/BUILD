load("@io_bazel_rules_scala//scala:scala.bzl", "scala_binary", "scala_test", "scala_repl")
load("@org_bykn_bosatsu//tools:scala.bzl", "scala_library", "scala_module", "strict_scalacopts")

package(default_visibility = ["//visibility:public"])

scala_binary(
    name = "bosatsu_main",
    srcs = ["Main.scala"],
    deps = ["//base/src/main/scala/org/bykn/bosatsu"],
    runtime_deps = [],
    main_class = "org.bykn.bosatsu.Main",
    scalacopts = strict_scalacopts(),
    visibility = ["//visibility:public"],
  )
