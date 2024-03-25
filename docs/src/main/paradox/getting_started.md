# Getting started
You can try basic expressions using this [in-browser Bosatsu compiler](https://johnynek.github.io/bosatsu/compiler/).

To work locally, clone the [bosatsu repo](https://github.com/johnynek/bosatsu), [install sbt](https://www.scala-sbt.org/1.x/docs/Setup.html), then build the compiler:
```
sbt cli/assembly
```
If that completes successfully, you can run `./bosatsuj` if you have java installed. Running with no
arguments prints the help message. You should see something like:
```
Missing expected command (eval or check or test or json or transpile)!

Usage:
    bosatsu eval
    bosatsu check
    bosatsu test
    bosatsu json
    bosatsu transpile

a total and functional programming language

version: 0.1.0-SNAPSHOT
scala-version: 2.12.11
git-sha: 1d068c4f3c16807aeb42565d4fc7c45b401e96a4

Options and flags:
    --help
        Display this help text.

Subcommands:
    eval
        evaluate an expression and print the output
    check
        type check a set of packages
    test
        test a set of bosatsu modules
    json
        json writing and transformation tools
    transpile
        transpile bosatsu into another language
```

Now try running an example program:
```
./bosatsuj eval --main Euler/One::computed --input test_workspace/euler1.bosatsu
```
The command `./bosatsuj` has pretty complete help if you run it with no arguments. If you have [graal native-image](https://www.graalvm.org/reference-manual/native-image/) on your path, after building with `sbt cli/assembly` you can build the native image:
```
./build_native.sh
```
Now you should have `./bosatsu` available which is *MUCH* faster to use than `./bosatsuj` which has to pay the JVM startup cost on each call, e.g.:
```
oscar@oscar-XPS-13-7390:~/oss/bosatsu$ time bosatsu eval --main Euler/One::computed --input test_workspace/euler1.bosatsu 
233168: Bosatsu/Predef::Int

real    0m0.161s
user    0m0.085s
sys     0m0.051s
oscar@oscar-XPS-13-7390:~/oss/bosatsu$ time ./bosatsuj eval --main Euler/One::computed --input test_workspace/euler1.bosatsu 
233168: Bosatsu/Predef::Int

real    0m2.260s
user    0m4.621s
sys     0m0.155s
```

