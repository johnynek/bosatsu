#!/bin/sh

sbt cli/assembly
time ./bosatsuj tool test --input_dir test_workspace --package_root test_workspace
