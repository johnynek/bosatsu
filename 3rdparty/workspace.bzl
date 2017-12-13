def declare_maven(hash):
    native.maven_jar(
        name = hash["name"],
        artifact = hash["artifact"],
        sha1 = hash["sha1"],
        repository = hash["repository"]
    )
    native.bind(
        name = hash["bind"],
        actual = hash["actual"]
    )

def maven_dependencies(callback = declare_maven):
    callback({"artifact": "com.lihaoyi:fastparse-utils_2.11:1.0.0", "lang": "scala", "sha1": "98716ae2093a51449f41485009ce1bb1cefd3336", "repository": "https://repo.maven.apache.org/maven2/", "name": "com_lihaoyi_fastparse_utils_2_11", "actual": "@com_lihaoyi_fastparse_utils_2_11//jar:file", "bind": "jar/com/lihaoyi/fastparse_utils_2_11"})
    callback({"artifact": "com.lihaoyi:fastparse_2.11:1.0.0", "lang": "scala", "sha1": "334cc8841a7f72a16c258252232fd1db8c0e9791", "repository": "https://repo.maven.apache.org/maven2/", "name": "com_lihaoyi_fastparse_2_11", "actual": "@com_lihaoyi_fastparse_2_11//jar:file", "bind": "jar/com/lihaoyi/fastparse_2_11"})
    callback({"artifact": "com.lihaoyi:sourcecode_2.11:0.1.4", "lang": "scala", "sha1": "78369535832ebb91fb1c4054c5662b2c9a0d2c88", "repository": "https://repo.maven.apache.org/maven2/", "name": "com_lihaoyi_sourcecode_2_11", "actual": "@com_lihaoyi_sourcecode_2_11//jar:file", "bind": "jar/com/lihaoyi/sourcecode_2_11"})
    callback({"artifact": "com.monovore:decline_2.11:0.4.0-M2", "lang": "scala", "sha1": "7436ca297799dd761a1d21e5fd66d72ed54186b1", "repository": "https://repo.maven.apache.org/maven2/", "name": "com_monovore_decline_2_11", "actual": "@com_monovore_decline_2_11//jar:file", "bind": "jar/com/monovore/decline_2_11"})
    callback({"artifact": "com.stripe:dagon-core_2.11:0.2.2", "lang": "scala", "sha1": "03d2ca9d27bbb3f494b2eb9414e121ede364f50a", "repository": "https://repo.maven.apache.org/maven2/", "name": "com_stripe_dagon_core_2_11", "actual": "@com_stripe_dagon_core_2_11//jar:file", "bind": "jar/com/stripe/dagon_core_2_11"})
    callback({"artifact": "org.scala-sbt:test-interface:1.0", "lang": "java", "sha1": "0a3f14d010c4cb32071f863d97291df31603b521", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_scala_sbt_test_interface", "actual": "@org_scala_sbt_test_interface//jar", "bind": "jar/org/scala_sbt/test_interface"})
    callback({"artifact": "org.scalacheck:scalacheck_2.11:1.13.5", "lang": "scala", "sha1": "4800dfc0e73bd9af55a89ba7c8ec44c46b6f034f", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_scalacheck_scalacheck_2_11", "actual": "@org_scalacheck_scalacheck_2_11//jar:file", "bind": "jar/org/scalacheck/scalacheck_2_11"})
    callback({"artifact": "org.scalactic:scalactic_2.11:3.0.1", "lang": "scala", "sha1": "3c444d143879dc172fa555cea08fd0de6fa2f34f", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_scalactic_scalactic_2_11", "actual": "@org_scalactic_scalactic_2_11//jar:file", "bind": "jar/org/scalactic/scalactic_2_11"})
    callback({"artifact": "org.scalatest:scalatest_2.11:3.0.1", "lang": "scala", "sha1": "40a1842e7f0b915d87de1cb69f9c6962a65ee1fd", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_scalatest_scalatest_2_11", "actual": "@org_scalatest_scalatest_2_11//jar:file", "bind": "jar/org/scalatest/scalatest_2_11"})
    callback({"artifact": "org.spire-math:kind-projector_2.11:0.9.4", "lang": "scala", "sha1": "d8872b2c067d3c9b57bf4809d0d0ca77ed9f5435", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_spire_math_kind_projector_2_11", "actual": "@org_spire_math_kind_projector_2_11//jar:file", "bind": "jar/org/spire_math/kind_projector_2_11"})
    callback({"artifact": "org.typelevel:cats-core_2.11:1.0.0-MF", "lang": "scala", "sha1": "3bd1f05e18bec5521948fd7a56f0594ad764557c", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_typelevel_cats_core_2_11", "actual": "@org_typelevel_cats_core_2_11//jar:file", "bind": "jar/org/typelevel/cats_core_2_11"})
    callback({"artifact": "org.typelevel:cats-free_2.11:1.0.0-MF", "lang": "scala", "sha1": "8133c0b9d18f1f253a646b672797c67505c658bb", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_typelevel_cats_free_2_11", "actual": "@org_typelevel_cats_free_2_11//jar:file", "bind": "jar/org/typelevel/cats_free_2_11"})
    callback({"artifact": "org.typelevel:cats-kernel_2.11:1.0.0-MF", "lang": "scala", "sha1": "7f1ea90fefda409d3004aab05e158a62937a1353", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_typelevel_cats_kernel_2_11", "actual": "@org_typelevel_cats_kernel_2_11//jar:file", "bind": "jar/org/typelevel/cats_kernel_2_11"})
    callback({"artifact": "org.typelevel:cats-macros_2.11:1.0.0-MF", "lang": "scala", "sha1": "c9e3310d7f62e4ec9ed3ce1e843a26a71ba10798", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_typelevel_cats_macros_2_11", "actual": "@org_typelevel_cats_macros_2_11//jar:file", "bind": "jar/org/typelevel/cats_macros_2_11"})
    callback({"artifact": "org.typelevel:machinist_2.11:0.6.1", "lang": "scala", "sha1": "239a56280d1cf730048f552a1a18f415bfcbf270", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_typelevel_machinist_2_11", "actual": "@org_typelevel_machinist_2_11//jar:file", "bind": "jar/org/typelevel/machinist_2_11"})
    callback({"artifact": "org.typelevel:paiges-core_2.11:0.2.0", "lang": "scala", "sha1": "ddb340684f1a95bca4665abf23745d0e9e9371bf", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_typelevel_paiges_core_2_11", "actual": "@org_typelevel_paiges_core_2_11//jar:file", "bind": "jar/org/typelevel/paiges_core_2_11"})
