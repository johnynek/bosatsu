# Compiling to Python

Bosatsu can currently be interpreted using the `eval` subcommand, or it can be
compiled to Python using the `transpile` subcommand. This assumes you have
installed the Bosatsu CLI; see [Getting started](getting_started.md).

For instance, to compile all our test code into Python:
```
./bosatsu transpile --input_dir test_workspace/ --package_root test_workspace/ --lang python --outdir pyout
```
Then in the pyout directory we will find one Python file for each Bosatsu
package.
