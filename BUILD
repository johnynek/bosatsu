load("@org_bykn_bosatsu//tools:bosatsu.bzl", "bosatsu_library", "bosatsu_json")

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
    srcs = ["test.bosatsu"])

bosatsu_json(
    name = "test2json",
    deps = [":test"],
    package = "Foo/Bar2",
    srcs = ["test2.bosatsu"])
