load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library")
scala_library(
    name = "dagon_core",
    exports = [
        "//external:jar/com/stripe/dagon_core_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library"
    ],
    visibility = [
        "//visibility:public"
    ]
)


