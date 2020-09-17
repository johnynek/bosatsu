# Compiling to Python

Bosatsu can currently be interpretted using the `eval` subcommand, or it can be
compiled to Python using the `transpile` subcommand.

For instance, to compile all our test code into Python:
```
./bosatsuj transpile --input_dir test_workspace/ --package_root test_workspace/ --lang Python --outdir pyout
```
Then in the pyout directory we will find one Python file for each Bosatsu package.
