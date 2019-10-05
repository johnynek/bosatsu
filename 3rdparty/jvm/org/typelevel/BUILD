load("@io_bazel_rules_scala//scala:scala_import.bzl", "scala_import")
scala_import(
    name = "alleycats_core",
    jars = [
        "//external:jar/org/typelevel/alleycats_core_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        ":cats_core"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_import(
    name = "catalysts_macros",
    jars = [
        "//external:jar/org/typelevel/catalysts_macros_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        ":macro_compat"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



scala_import(
    name = "catalysts_platform",
    jars = [
        "//external:jar/org/typelevel/catalysts_platform_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        ":catalysts_macros"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



scala_import(
    name = "cats_core",
    jars = [
        "//external:jar/org/typelevel/cats_core_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        ":cats_kernel",
        ":cats_macros"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_import(
    name = "cats_effect",
    jars = [
        "//external:jar/org/typelevel/cats_effect_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        ":cats_core"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_import(
    name = "cats_free",
    jars = [
        "//external:jar/org/typelevel/cats_free_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        ":cats_core",
        ":cats_macros"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_import(
    name = "cats_kernel",
    jars = [
        "//external:jar/org/typelevel/cats_kernel_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_import(
    name = "cats_kernel_laws",
    jars = [
        "//external:jar/org/typelevel/cats_kernel_laws_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        "//3rdparty/jvm/org/scalacheck:scalacheck",
        ":catalysts_platform",
        ":cats_kernel",
        ":discipline"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



scala_import(
    name = "cats_laws",
    jars = [
        "//external:jar/org/typelevel/cats_laws_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        "//3rdparty/jvm/org/scalacheck:scalacheck",
        ":catalysts_platform",
        ":cats_core",
        ":cats_kernel",
        ":cats_kernel_laws",
        ":cats_macros",
        ":discipline",
        ":machinist"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



scala_import(
    name = "cats_macros",
    jars = [
        "//external:jar/org/typelevel/cats_macros_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_import(
    name = "cats_testkit",
    jars = [
        "//external:jar/org/typelevel/cats_testkit_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        "//3rdparty/jvm/org/scalacheck:scalacheck",
        "//3rdparty/jvm/org/scalatest:scalatest",
        ":cats_core",
        ":cats_laws",
        ":cats_macros",
        ":discipline",
        ":machinist"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



scala_import(
    name = "discipline",
    jars = [
        "//external:jar/org/typelevel/discipline_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        "//3rdparty/jvm/org/scalacheck:scalacheck"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



scala_import(
    name = "machinist",
    jars = [
        "//external:jar/org/typelevel/machinist_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        "//3rdparty/jvm/org/scala_lang:scala_reflect"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



scala_import(
    name = "macro_compat",
    jars = [
        "//external:jar/org/typelevel/macro_compat_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



scala_import(
    name = "paiges_core",
    jars = [
        "//external:jar/org/typelevel/paiges_core_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library"
    ],
    visibility = [
        "//visibility:public"
    ]
)


