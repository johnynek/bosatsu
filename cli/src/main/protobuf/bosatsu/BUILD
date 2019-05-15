load("@io_bazel_rules_scala//scala_proto:scala_proto.bzl", "scalapb_proto_library")
load("@io_bazel_rules_scala//scala_proto:scala_proto_toolchain.bzl", "scala_proto_toolchain")

package(default_visibility = ["//visibility:public"])

scala_proto_toolchain(
    name = "scala_proto_toolchain_configuration",
    with_grpc = False,
)

toolchain(
    name = "scalapb_toolchain",
    toolchain = ":scala_proto_toolchain_configuration",
    toolchain_type = "@io_bazel_rules_scala//scala_proto:toolchain_type",
    visibility = ["//visibility:public"],
)

proto_library(
    name = "bosatsu_proto",
    srcs = ["TypedAst.proto"],
    )

scalapb_proto_library(
    name = "bosatsu",
    deps = [":bosatsu_proto"],
    )
