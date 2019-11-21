load("@org_bykn_bosatsu//tools:scala.bzl", "scala_library")

package(default_visibility = ["//visibility:public"])

scala_library(
    name = "par",
    srcs = [".jvm/src/main/scala/org/bykn/bosatsu/Par.scala"],
    deps = [
      "//3rdparty/jvm/org/typelevel:cats_core",
      "//3rdparty/jvm/org/typelevel:cats_kernel",
    ],
)
