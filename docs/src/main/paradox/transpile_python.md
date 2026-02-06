# Compiling to Python

Bosatsu can currently be interpreted using `tool eval`, or it can be
compiled to Python using `tool transpile`. This assumes you have
installed the Bosatsu CLI; see [Getting started](getting_started.html).

For instance, to compile all our test code into Python:
```
./bosatsu tool transpile --input_dir test_workspace/ --package_root test_workspace/ python --outdir pyout
```
Then in the pyout directory we will find one Python file for each Bosatsu
package.
