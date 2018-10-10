load("@io_bazel_rules_scala//scala:scala_import.bzl", "scala_import")
scala_import(
    name = "fastparse",
    exports = [
        ":sourcecode"
    ],
    jars = [
        "//external:jar/com/lihaoyi/fastparse_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        ":fastparse_utils"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_import(
    name = "fastparse_utils",
    exports = [
        ":sourcecode"
    ],
    jars = [
        "//external:jar/com/lihaoyi/fastparse_utils_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_import(
    name = "sourcecode",
    jars = [
        "//external:jar/com/lihaoyi/sourcecode_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library"
    ],
    visibility = [
        "//visibility:public"
    ]
)


