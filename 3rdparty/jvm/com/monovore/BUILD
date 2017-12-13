load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library")
scala_library(
    name = "decline",
    exports = [
        "//external:jar/com/monovore/decline_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        "//3rdparty/jvm/org/typelevel:cats_core"
    ],
    visibility = [
        "//visibility:public"
    ]
)


