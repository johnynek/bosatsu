#!/bin/sh
tmp_dir=$(mktemp -d -t bosatsupy-XXXXXXXXXX)
./bosatsuj transpile --input_dir test_workspace/ --package_root test_workspace/ --lang python --outdir $tmp_dir
#./bosatsu transpile --input_dir test_workspace/ --package_root test_workspace/ --lang python --outdir $tmp_dir
python3 -m unittest discover $tmp_dir -v --pattern "*.py"
rm -rf $tmp_dir
#echo $tmp_dir
