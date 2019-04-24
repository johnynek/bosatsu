load("@org_bykn_bosatsu//tools:bosatsu.bzl", "bosatsu_library", "bosatsu_json", "bosatsu_test")

bosatsu_library(
    name = "test",
    srcs = ["test.bosatsu"])

bosatsu_library(
    name = "test2",
    deps = [":test"],
    srcs = ["test2.bosatsu"])

bosatsu_json(
    name = "testjson",
    package = "Foo/Bar",
    deps = [":test"])

bosatsu_json(
    name = "test2json",
    deps = [":test", ":test2"],
    package = "Foo/Bar2")

bosatsu_test(
    name = "test_rule",
    srcs = ["bo_test.bosatsu"],
    size = "small")

bosatsu_test(
    name = "euler1",
    srcs = ["euler1.bosatsu"],
    size = "small")

bosatsu_test(
    name = "euler2",
    srcs = ["euler2.bosatsu"],
    size = "medium")

bosatsu_test(
    name = "euler3",
    srcs = ["euler3.bosatsu"],
    size = "small")

bosatsu_test(
    name = "euler4",
    srcs = ["euler4.bosatsu"],
    size = "small")

bosatsu_test(
    name = "euler5",
    srcs = ["euler5.bosatsu"],
    size = "small")

bosatsu_test(
    name = "euler6",
    srcs = ["euler6.bosatsu"],
    size = "small")

bosatsu_test(
    name = "euler7",
    srcs = ["euler7.bosatsu"],
    size = "medium")

bosatsu_library(
    name = "dict_tools",
    srcs = ["dicttools.bosatsu"])

bosatsu_library(
    name = "bazel_deps_api",
    deps = ["dict_tools"],
    srcs = ["bazel_deps_api.bosatsu"],
    )

bosatsu_json(
    name ="gen_deps",
    srcs = ["gen_deps.bosatsu"],
    deps = [":bazel_deps_api", ":dict_tools"],
    package = "GenDeps")

bosatsu_library(
    name = "queue",
    srcs = ["Queue.bosatsu"],
    )

# todo we need to be able to test a library without recompiling it
bosatsu_test(
    name = "queue_test",
    srcs = ["Queue.bosatsu"],
    )

bosatsu_library(
    name = "treelist",
    srcs = ["TreeList.bosatsu"],
    )

# todo we need to be able to test a library without recompiling it
bosatsu_test(
    name = "treelist_test",
    srcs = ["TreeList.bosatsu"],
    )
