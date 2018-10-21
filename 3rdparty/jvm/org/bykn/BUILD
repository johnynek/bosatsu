load("@io_bazel_rules_scala//scala:scala_import.bzl", "scala_import")
scala_import(
    name = "fastparse_cats_core",
    jars = [
        "//external:jar/org/bykn/fastparse_cats_core_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/com/lihaoyi:fastparse",
        "//3rdparty/jvm/org/scala_lang:scala_library",
        "//3rdparty/jvm/org/scalacheck:scalacheck",
        "//3rdparty/jvm/org/scalatest:scalatest",
        "//3rdparty/jvm/org/typelevel:cats_core",
        "//3rdparty/jvm/org/typelevel:cats_laws",
        "//3rdparty/jvm/org/typelevel:cats_testkit"
    ],
    visibility = [
        "//visibility:public"
    ]
)


