load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library")
scala_library(
    name = "fastparse",
    exports = [
        "//external:jar/com/lihaoyi/fastparse_2_11",
        ":sourcecode"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        ":fastparse_utils"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_library(
    name = "fastparse_utils",
    exports = [
        "//external:jar/com/lihaoyi/fastparse_utils_2_11",
        ":sourcecode"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_library(
    name = "sourcecode",
    exports = [
        "//external:jar/com/lihaoyi/sourcecode_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library"
    ],
    visibility = [
        "//visibility:public"
    ]
)


