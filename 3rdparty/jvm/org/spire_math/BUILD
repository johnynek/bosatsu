load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library")
scala_library(
    name = "kind_projector",
    exports = [
        "//external:jar/org/spire_math/kind_projector_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_compiler",
        "//3rdparty/jvm/org/scala_lang:scala_library"
    ],
    visibility = [
        "//visibility:public"
    ]
)


