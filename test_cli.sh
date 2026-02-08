#!/bin/sh

sbt cli/assembly
time ./bosatsuj tool test --input_dir test_workspace --input test_workspace/Bosatsu/IO/Error.bosatsu --input test_workspace/Bosatsu/IO/Std.bosatsu --package_root test_workspace
