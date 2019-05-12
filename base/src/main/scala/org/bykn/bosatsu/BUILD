load("@io_bazel_rules_scala//scala:scala.bzl", "scala_binary", "scala_test", "scala_repl", "scala_macro_library")
load("@org_bykn_bosatsu//tools:scala.bzl", "scala_library", "scala_module", "strict_scalacopts")

package(default_visibility = ["//visibility:public"])

scala_macro_library(
    name = "bosatsu",
    srcs = glob(["**/*.scala"]),
    )
