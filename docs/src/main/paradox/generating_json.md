# Generating JSON from Bosatsu Values

Like [Dhall](https://dhall-lang.org/) Bosatsu can be used as a configuration language.

As an example, in the project repo, after running `sbt cli/assembly` try the following:
```
./bosatuj json write --main_file test_workspace/Foo.bosatsu --package_root test_workspace/
```
you should see: `"this is Foo"`. The json subcommand gives you the ability to render
values as JSON.

For a more interesting example see:

```
./bosatsuj json write --main_file test_workspace/gen_deps.bosatsu --input_dir test_workspace/ --package_root test_workspace/ | head -30
```
which should render:
```
{
  "options": {
      "buildHeader": [
          "load(\"@io_bazel_rules_scala//scala:scala_import.bzl\", \"scala_import\")" ],
      "languages": [ "java", "scala:2.11.11" ],
      "resolvers": [
          {
            "id": "mavencentral",
            "type": "default",
            "url": "https://repo.maven.apache.org/maven2/"
          } ],
      "transitivity": "runtime_deps",
      "resolverType": "coursier",
      "versionConflictPolicy": "highest"
    },
  "dependencies": {
      "com.lihaoyi": {
          "fastparse": {
              "modules": [ "", "utils" ],
              "lang": "scala",
              "version": "1.0.0",
              "exports": [ "com.lihaoyi:sourcecode" ]
            },
          "sourcecode": {
              "modules": [ ],
              "lang": "scala",
              "version": "0.1.4",
              "exports": [ ]
            }
        },
```
