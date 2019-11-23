load("@org_bykn_bosatsu//tools:bosatsu.bzl", "bosatsu_library", "bosatsu_json", "bosatsu_test")

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
    deps = [":list"],
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
    deps = [":list"],
    size = "small")

bosatsu_test(
    name = "euler6",
    srcs = ["euler6.bosatsu"],
    size = "small")

bosatsu_test(
    name = "euler7",
    srcs = ["euler7.bosatsu"],
    deps = [":bool", ":list"],
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
    name = "bool",
    srcs = ["Bool.bosatsu"])

bosatsu_library(
    name = "list",
    srcs = ["List.bosatsu"],
    deps = [":bool", ":nat"],
    )

bosatsu_library(
    name = "dict",
    srcs = ["Dict.bosatsu"],
    deps = [":list"]
    )

bosatsu_test(
    name = "list_test",
    deps = [":list"],
    packages = ["Bosatsu/List"],
    )

bosatsu_library(
    name = "queue",
    deps = [":list"],
    srcs = ["Queue.bosatsu"],
    )

bosatsu_test(
    name = "queue_test",
    deps = [":queue"],
    packages = ["Queue"],
   )

bosatsu_library(
    name = "treelist",
    deps = [":list"],
    srcs = ["TreeList.bosatsu"],
    )

bosatsu_test(
    name = "treelist_test",
    deps = [":treelist"],
    packages = ["TreeList"],
    )

bosatsu_test(
    name = "recordset",
    deps = [],
    srcs = ["recordset.bosatsu"],
    packages = ["RecordSet/Library"],
    )

bosatsu_library(
    name = "nothing",
    srcs = ["Nothing.bosatsu"],
    )

bosatsu_library(
    name = "option",
    srcs = ["Option.bosatsu"],
    )

bosatsu_library(
    name = "applicative_traverse",
    srcs = ["ApplicativeTraverse.bosatsu"],
    deps = [":list", ":option"]
)

bosatsu_test(
    name = "applicative_traverse_test",
    deps = [":applicative_traverse"],
    packages = ["Bosatsu/Example/ApplicativeTraverse"]
    )

bosatsu_library(
    name = "nat",
    srcs = ["Nat.bosatsu"],
)

bosatsu_test(
    name = "nat_test",
    deps = [":nat"],
    packages = ["Bosatsu/Nat"]
    )

bosatsu_library(
    name = "binnat",
    srcs = ["BinNat.bosatsu"],
    deps = [":nat"],
)

bosatsu_test(
    name = "binnat_test",
    deps = [":binnat"],
    packages = ["Bosatsu/BinNat"]
    )

bosatsu_library(
    name = "BuildLibrary",
    srcs = ["BuildLibrary.bosatsu", "BuildExample.bosatsu"],
)
