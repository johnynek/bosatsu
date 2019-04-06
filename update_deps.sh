#!/bin/sh

bazel build test_workspace:gen_deps
cp bazel-bin/test_workspace/gen_deps.json dependencies.yaml
cd ../bazel-deps
bazel run :parse -- generate -r ~/oss/bosatsu/ --deps dependencies.yaml -s 3rdparty/workspace.bzl
cd ../bosatsu
