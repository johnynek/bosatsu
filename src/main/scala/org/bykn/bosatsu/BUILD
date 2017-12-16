load("@io_bazel_rules_scala//scala:scala.bzl", "scala_binary", "scala_test", "scala_repl")
load("@org_bykn_bosatsu//tools:scala.bzl", "scala_library", "scala_module", "strict_scalacopts")

common_deps = [
      "//src/main/java/org/bykn/bosatsu",
      "//3rdparty/jvm/org/typelevel:cats_core",
      "//3rdparty/jvm/org/typelevel:cats_kernel",
      "//3rdparty/jvm/org/typelevel:cats_macros",
      "//3rdparty/jvm/org/typelevel:machinist",
      "//3rdparty/jvm/org/typelevel:paiges_core",
      "//3rdparty/jvm/com/lihaoyi:fastparse",
      "//3rdparty/jvm/com/lihaoyi:fastparse_utils",
      "//3rdparty/jvm/com/monovore:decline",
      "//3rdparty/jvm/com/stripe:dagon_core",
  ]

scala_module(
    name = "bosatsu",
    all_srcs = [f for f in glob(["*.scala"]) if f != "Main.scala"],
    srcs_deps = {
        "DefinedType.scala": ["Type.scala", "Substitutable.scala", "TypeError.scala"],
        "EditDistance.scala": [],
        "Fix.scala": [],
        "LocationMap.scala": [],
        "PackageName.scala": ["Parser.scala"],
        "Parser.scala": ["Region.scala", "LocationMap.scala"],
        "Region.scala": [],
        "Substitutable.scala": ["Type.scala"],
        "Type.scala": ["Region.scala", "PackageName.scala", "LocationMap.scala"],
        "TypeEnv.scala": ["Type.scala", "Substitutable.scala", "TypeError.scala", "DefinedType.scala"],
        "TypeError.scala": ["Type.scala"],
        "TypeRef.scala": ["Type.scala"],
        },
    deps = common_deps,
    visibility = ["//visibility:public"],
)

scala_binary(
    name = "bosatsu_main",
    srcs = ["Main.scala"],
    deps = [":bosatsu"] + common_deps,
    runtime_deps = ["//src/main:resources"],
    main_class = "org.bykn.bosatsu.Main",
    scalacopts = strict_scalacopts(),
    visibility = ["//visibility:public"],
  )
