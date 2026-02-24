#!/bin/sh

sbt cli/assembly
time ./bosatsuj tool test \
  --input_dir test_workspace \
  --input test_workspace/Bosatsu/IO/Error.bosatsu \
  --input test_workspace/Bosatsu/IO/Core.bosatsu \
  --input test_workspace/Bosatsu/IO/Std.bosatsu \
  --package_root test_workspace

time ./bosatsuj lib fetch \
  --repo_root . \
  --name core_alpha

time ./bosatsuj lib eval \
  --repo_root . \
  --name core_alpha \
  --main Bosatsu/LsExample \
  --color none > /dev/null
