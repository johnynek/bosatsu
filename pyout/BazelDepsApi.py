import Bosatsu.Predef as ___iPredef0
import DictTools as ___iDictTools1

maven_central = ("mavencentral", "default", "https://repo.maven.apache.org/maven2/")

default_options_with_scala = ((1,
        "load(\"@io_bazel_rules_scala//scala:scala_import.bzl\", \"scala_import\")",
        (0,)),
    (1, "java", (1, "scala:2.11.11", (0,))),
    (1, maven_central, (0,)),
    "runtime_deps",
    "coursier",
    "highest")

def scala_dep(___borgname0, ___bartifact0, ___bversion0):
    return ___iPredef0.add_key(___iPredef0.empty_Dict(___iPredef0.string_Order),
        ___borgname0,
        ___iPredef0.add_key(___iPredef0.empty_Dict(___iPredef0.string_Order),
            ___bartifact0,
            ((0,), "scala", ___bversion0, (0,))))

def scala_dep_modules(___borgname1, ___bartifact1, ___bmodules0, ___bversion1, ___bexports0):
    return ___iPredef0.add_key(___iPredef0.empty_Dict(___iPredef0.string_Order),
        ___borgname1,
        ___iPredef0.add_key(___iPredef0.empty_Dict(___iPredef0.string_Order),
            ___bartifact1,
            (___bmodules0, "scala", ___bversion1, ___bexports0)))

def merge_artifact(___bleft0, ___bright0):
    return (___iPredef0.concat(___bleft0[0], ___bright0[0]),
        ___bright0[1],
        ___bright0[2],
        ___iPredef0.concat(___bleft0[3], ___bright0[3]))

def merge_deps(___bleft1, ___bright1):
    return ___iDictTools1.merge(___bleft1,
        ___bright1,
        lambda ___bl0, ___br0: ___iDictTools1.merge(___bl0, ___br0, merge_artifact))

def merge_dep_List(___bdeps0):
    return ___iPredef0.foldl_List(___bdeps0,
        ___iPredef0.empty_Dict(___iPredef0.string_Order),
        merge_deps)

standard_scala_replacements = ___iPredef0.add_key(___iPredef0.add_key(___iPredef0.empty_Dict(___iPredef0.string_Order),
        "org.scala-lang.modules",
        ___iPredef0.add_key(___iPredef0.add_key(___iPredef0.empty_Dict(___iPredef0.string_Order),
                "scala-xml",
                ("scala", "@io_bazel_rules_scala_scala_xml//:io_bazel_rules_scala_scala_xml")),
            "scala-parser-combinators",
            ("scala",
                "@io_bazel_rules_scala_scala_parser_combinators//:io_bazel_rules_scala_scala_parser_combinators"))),
    "org.scala-lang",
    ___iPredef0.add_key(___iPredef0.add_key(___iPredef0.add_key(___iPredef0.empty_Dict(___iPredef0.string_Order),
                "scala-reflect",
                ("scala/unmangled",
                    "@io_bazel_rules_scala_scala_reflect//:io_bazel_rules_scala_scala_reflect")),
            "scala-library",
            ("scala/unmangled",
                "@io_bazel_rules_scala_scala_library//:io_bazel_rules_scala_scala_library")),
        "scala-compiler",
        ("scala/unmangled",
            "@io_bazel_rules_scala_scala_compiler//:io_bazel_rules_scala_scala_compiler")))