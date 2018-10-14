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
