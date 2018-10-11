# Do not edit. bazel-deps autogenerates this file from dependencies.yaml.
def _jar_artifact_impl(ctx):
    jar_name = "%s.jar" % ctx.name
    ctx.download(
        output=ctx.path("jar/%s" % jar_name),
        url=ctx.attr.urls,
        sha256=ctx.attr.sha256,
        executable=False
    )
    src_name="%s-sources.jar" % ctx.name
    srcjar_attr=""
    has_sources = len(ctx.attr.src_urls) != 0
    if has_sources:
        ctx.download(
            output=ctx.path("jar/%s" % src_name),
            url=ctx.attr.src_urls,
            sha256=ctx.attr.src_sha256,
            executable=False
        )
        srcjar_attr ='\n    srcjar = ":%s",' % src_name

    build_file_contents = """
package(default_visibility = ['//visibility:public'])
java_import(
    name = 'jar',
    tags = ['maven_coordinates={artifact}'],
    jars = ['{jar_name}'],{srcjar_attr}
)
filegroup(
    name = 'file',
    srcs = [
        '{jar_name}',
        '{src_name}'
    ],
    visibility = ['//visibility:public']
)\n""".format(artifact = ctx.attr.artifact, jar_name = jar_name, src_name = src_name, srcjar_attr = srcjar_attr)
    ctx.file(ctx.path("jar/BUILD"), build_file_contents, False)
    return None

jar_artifact = repository_rule(
    attrs = {
        "artifact": attr.string(mandatory = True),
        "sha256": attr.string(mandatory = True),
        "urls": attr.string_list(mandatory = True),
        "src_sha256": attr.string(mandatory = False, default=""),
        "src_urls": attr.string_list(mandatory = False, default=[]),
    },
    implementation = _jar_artifact_impl
)

def jar_artifact_callback(hash):
    src_urls = []
    src_sha256 = ""
    source=hash.get("source", None)
    if source != None:
        src_urls = [source["url"]]
        src_sha256 = source["sha256"]
    jar_artifact(
        artifact = hash["artifact"],
        name = hash["name"],
        urls = [hash["url"]],
        sha256 = hash["sha256"],
        src_urls = src_urls,
        src_sha256 = src_sha256
    )
    native.bind(name = hash["bind"], actual = hash["actual"])


def list_dependencies():
    return [
    {"artifact": "com.lihaoyi:fastparse-utils_2.11:1.0.0", "lang": "scala", "sha1": "98716ae2093a51449f41485009ce1bb1cefd3336", "sha256": "90bc5d8979d6b1b95f636d5d5751033884a5cb500cf812b152ab6fe5c972e7bf", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/com/lihaoyi/fastparse-utils_2.11/1.0.0/fastparse-utils_2.11-1.0.0.jar", "source": {"sha1": "e2735bfc5fbf85a21f7335ac672e27b4ec55358c", "sha256": "6a092add91bab7bf903cf74d18a8d7c3023e832fb00ac0cf176db7b35dfd9e48", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/com/lihaoyi/fastparse-utils_2.11/1.0.0/fastparse-utils_2.11-1.0.0-sources.jar"} , "name": "com_lihaoyi_fastparse_utils_2_11", "actual": "@com_lihaoyi_fastparse_utils_2_11//jar:file", "bind": "jar/com/lihaoyi/fastparse_utils_2_11"},
    {"artifact": "com.lihaoyi:fastparse_2.11:1.0.0", "lang": "scala", "sha1": "334cc8841a7f72a16c258252232fd1db8c0e9791", "sha256": "1b6d9fc75ca8a62abe0dd7a71e62aa445f2d3198c86aab5088e1f90a96ade30b", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/com/lihaoyi/fastparse_2.11/1.0.0/fastparse_2.11-1.0.0.jar", "source": {"sha1": "32f0ce22bbe0407737ac5346952a3556062400cf", "sha256": "f8792af99935264e1d23d882012b6d89aacbe74d820f63af866b5b162cb5d034", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/com/lihaoyi/fastparse_2.11/1.0.0/fastparse_2.11-1.0.0-sources.jar"} , "name": "com_lihaoyi_fastparse_2_11", "actual": "@com_lihaoyi_fastparse_2_11//jar:file", "bind": "jar/com/lihaoyi/fastparse_2_11"},
    {"artifact": "com.lihaoyi:sourcecode_2.11:0.1.4", "lang": "scala", "sha1": "78369535832ebb91fb1c4054c5662b2c9a0d2c88", "sha256": "e0edffec93ddef29c40b7c65580960062a3fa9d781eddb8c64e19e707c4a8e7c", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/com/lihaoyi/sourcecode_2.11/0.1.4/sourcecode_2.11-0.1.4.jar", "source": {"sha1": "c910950e1170659ba1671b24c2c4f8f2b174b336", "sha256": "b6a282beaca27092692197c017cbd349dccf526100af1bbd7f78cf462219f7f9", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/com/lihaoyi/sourcecode_2.11/0.1.4/sourcecode_2.11-0.1.4-sources.jar"} , "name": "com_lihaoyi_sourcecode_2_11", "actual": "@com_lihaoyi_sourcecode_2_11//jar:file", "bind": "jar/com/lihaoyi/sourcecode_2_11"},
    {"artifact": "com.monovore:decline_2.11:0.4.2", "lang": "scala", "sha1": "6ba2ad6bb271bfe1df1a1ada1725a262d0cad437", "sha256": "c3c732cf50b616d7c23b1410ab605c33a630a4daf36ce9882b665b4e6d6ea361", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/com/monovore/decline_2.11/0.4.2/decline_2.11-0.4.2.jar", "source": {"sha1": "6859ade0f9d3a293086765bca0440ce01ade0280", "sha256": "bbab8cc31cd043c0cd77d96cfa8894c7609cdadf4913901c9dd7746ccd37edc6", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/com/monovore/decline_2.11/0.4.2/decline_2.11-0.4.2-sources.jar"} , "name": "com_monovore_decline_2_11", "actual": "@com_monovore_decline_2_11//jar:file", "bind": "jar/com/monovore/decline_2_11"},
    {"artifact": "com.stripe:dagon-core_2.11:0.2.2", "lang": "scala", "sha1": "03d2ca9d27bbb3f494b2eb9414e121ede364f50a", "sha256": "d3ae2014a607859861b273046717b6a283dc140d7c3e945fc3b4eca812ea3fc4", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/com/stripe/dagon-core_2.11/0.2.2/dagon-core_2.11-0.2.2.jar", "source": {"sha1": "e622cf39dc59644d2ab0e7bc00a5fbcc5ef4db5e", "sha256": "1f6e315f39ec5090e3fdd8a24f3f82740032e377d249eea9f63b216027b46d83", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/com/stripe/dagon-core_2.11/0.2.2/dagon-core_2.11-0.2.2-sources.jar"} , "name": "com_stripe_dagon_core_2_11", "actual": "@com_stripe_dagon_core_2_11//jar:file", "bind": "jar/com/stripe/dagon_core_2_11"},
    {"artifact": "org.scala-sbt:test-interface:1.0", "lang": "java", "sha1": "0a3f14d010c4cb32071f863d97291df31603b521", "sha256": "15f70b38bb95f3002fec9aea54030f19bb4ecfbad64c67424b5e5fea09cd749e", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/org/scala-sbt/test-interface/1.0/test-interface-1.0.jar", "source": {"sha1": "d44b23e9e3419ad0e00b91bba764a48d43075000", "sha256": "c314491c9df4f0bd9dd125ef1d51228d70bd466ee57848df1cd1b96aea18a5ad", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/org/scala-sbt/test-interface/1.0/test-interface-1.0-sources.jar"} , "name": "org_scala_sbt_test_interface", "actual": "@org_scala_sbt_test_interface//jar", "bind": "jar/org/scala_sbt/test_interface"},
    {"artifact": "org.scalacheck:scalacheck_2.11:1.13.5", "lang": "scala", "sha1": "4800dfc0e73bd9af55a89ba7c8ec44c46b6f034f", "sha256": "7e55593585376e799b5c93561ee97b8c9e2a6e479205377e7bb9a77d5bd1f854", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/org/scalacheck/scalacheck_2.11/1.13.5/scalacheck_2.11-1.13.5.jar", "source": {"sha1": "0ed27a94e5d447b9a23cc169eb424092ed8d259a", "sha256": "d7ab366a782c957ba116aa47e7a86d4e74850c351875b0a347a235a1fe22c269", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/org/scalacheck/scalacheck_2.11/1.13.5/scalacheck_2.11-1.13.5-sources.jar"} , "name": "org_scalacheck_scalacheck_2_11", "actual": "@org_scalacheck_scalacheck_2_11//jar:file", "bind": "jar/org/scalacheck/scalacheck_2_11"},
    {"artifact": "org.scalactic:scalactic_2.11:3.0.1", "lang": "scala", "sha1": "3c444d143879dc172fa555cea08fd0de6fa2f34f", "sha256": "d5586d4aa060aebbf0ccb85be62208ca85ccc8c4220a342c22783adb04b1ded1", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/org/scalactic/scalactic_2.11/3.0.1/scalactic_2.11-3.0.1.jar", "source": {"sha1": "3de4d4b57f2cfc20c916a9444dd713235bf626ac", "sha256": "119b51c8a98623d259395d5688e814d3b46d4a8f5da9a9f0842ff988f8f03a1c", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/org/scalactic/scalactic_2.11/3.0.1/scalactic_2.11-3.0.1-sources.jar"} , "name": "org_scalactic_scalactic_2_11", "actual": "@org_scalactic_scalactic_2_11//jar:file", "bind": "jar/org/scalactic/scalactic_2_11"},
    {"artifact": "org.scalatest:scalatest_2.11:3.0.1", "lang": "scala", "sha1": "40a1842e7f0b915d87de1cb69f9c6962a65ee1fd", "sha256": "3788679b5c8762997b819989e5ec12847df3fa8dcb9d4a787c63188bd953ae2a", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/org/scalatest/scalatest_2.11/3.0.1/scalatest_2.11-3.0.1.jar", "source": {"sha1": "a419220815f884a36f461e6b05fc9303976ed8cb", "sha256": "a7532b9f0963060ce292b3d6705a7efa238960d38af8a1cc7da6fb72f6d54982", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/org/scalatest/scalatest_2.11/3.0.1/scalatest_2.11-3.0.1-sources.jar"} , "name": "org_scalatest_scalatest_2_11", "actual": "@org_scalatest_scalatest_2_11//jar:file", "bind": "jar/org/scalatest/scalatest_2_11"},
    {"artifact": "org.spire-math:kind-projector_2.11:0.9.4", "lang": "scala", "sha1": "d8872b2c067d3c9b57bf4809d0d0ca77ed9f5435", "sha256": "081f2c09b886b634f8613739bfb8d64d8d52020e5fe92a918fd55b70f4a27897", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/org/spire-math/kind-projector_2.11/0.9.4/kind-projector_2.11-0.9.4.jar", "source": {"sha1": "8ca363a721eb00f3812c7f1b3d56f9776e653ba6", "sha256": "7dc6a86e2aa26d38f80759bf117535c3114e906b0ba383ea47c1f90fc876a5c6", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/org/spire-math/kind-projector_2.11/0.9.4/kind-projector_2.11-0.9.4-sources.jar"} , "name": "org_spire_math_kind_projector_2_11", "actual": "@org_spire_math_kind_projector_2_11//jar:file", "bind": "jar/org/spire_math/kind_projector_2_11"},
    {"artifact": "org.typelevel:alleycats-core_2.11:1.4.0", "lang": "scala", "sha1": "17897e6bed201552dab0b98b19c03d1367982c6f", "sha256": "dacc767bcfe48b1898a999ba7d5292b589d746050cb62efa541fc2950f1e315d", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/org/typelevel/alleycats-core_2.11/1.4.0/alleycats-core_2.11-1.4.0.jar", "source": {"sha1": "bdfb64384c48c1d7336e8712b1e6c6850010d753", "sha256": "a0cd8c8c817dab2db0439707366258b8f7e4361a30e45b647bbc3125445ca51f", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/org/typelevel/alleycats-core_2.11/1.4.0/alleycats-core_2.11-1.4.0-sources.jar"} , "name": "org_typelevel_alleycats_core_2_11", "actual": "@org_typelevel_alleycats_core_2_11//jar:file", "bind": "jar/org/typelevel/alleycats_core_2_11"},
# duplicates in org.typelevel:cats-core_2.11 fixed to 1.4.0
# - com.monovore:decline_2.11:0.4.2 wanted version 1.1.0
# - org.typelevel:alleycats-core_2.11:1.4.0 wanted version 1.4.0
# - org.typelevel:cats-free_2.11:1.4.0 wanted version 1.4.0
    {"artifact": "org.typelevel:cats-core_2.11:1.4.0", "lang": "scala", "sha1": "af99bb2ca9936bef8e97c25f8f3fe4e39a0935ae", "sha256": "0de89db8f63e12e65f3444a0136c32621b820d1ea56a13495d60bfe054485fdf", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/org/typelevel/cats-core_2.11/1.4.0/cats-core_2.11-1.4.0.jar", "source": {"sha1": "183fa5ab7f13311d7daf5205d12f78513b2d4a6c", "sha256": "b45b9ce985f3646ec5fa0b455290f834ca2ba16402c63ac0cfe87ea8dc898555", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/org/typelevel/cats-core_2.11/1.4.0/cats-core_2.11-1.4.0-sources.jar"} , "name": "org_typelevel_cats_core_2_11", "actual": "@org_typelevel_cats_core_2_11//jar:file", "bind": "jar/org/typelevel/cats_core_2_11"},
    {"artifact": "org.typelevel:cats-free_2.11:1.4.0", "lang": "scala", "sha1": "24b07303afc5787c6b1c52a229d7d9e6431fb679", "sha256": "cbd73b6f29281292ff5d715b2064398ff499adb87818d8653f27cd35f2f56a30", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/org/typelevel/cats-free_2.11/1.4.0/cats-free_2.11-1.4.0.jar", "source": {"sha1": "192e6edfa4fcd64a4dffffae91b7bbc0d0171e0d", "sha256": "d8947e712bb8021c2e1bc0247824a84487689b0df86ae04d75e2e493564afa0f", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/org/typelevel/cats-free_2.11/1.4.0/cats-free_2.11-1.4.0-sources.jar"} , "name": "org_typelevel_cats_free_2_11", "actual": "@org_typelevel_cats_free_2_11//jar:file", "bind": "jar/org/typelevel/cats_free_2_11"},
    {"artifact": "org.typelevel:cats-kernel_2.11:1.4.0", "lang": "scala", "sha1": "a0f900b9fa2d6c45c4f0619ff695ecb66c067210", "sha256": "a79dd2c4fa7ead20ffc0050f2af2bb65b24678a001fa1e4a97c4428ad89bf914", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/org/typelevel/cats-kernel_2.11/1.4.0/cats-kernel_2.11-1.4.0.jar", "source": {"sha1": "e2ca06ba2f64ad6dd0d704440de59cd9647a98b3", "sha256": "00ae8977cc930db84720a7f529b0bf38b1aa4ad45179f51df4ac507061659bfb", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/org/typelevel/cats-kernel_2.11/1.4.0/cats-kernel_2.11-1.4.0-sources.jar"} , "name": "org_typelevel_cats_kernel_2_11", "actual": "@org_typelevel_cats_kernel_2_11//jar:file", "bind": "jar/org/typelevel/cats_kernel_2_11"},
    {"artifact": "org.typelevel:cats-macros_2.11:1.4.0", "lang": "scala", "sha1": "f5a6ce316441f5244747bdec35c0a983cd95aab3", "sha256": "53673f6d280fdfd258bdb72db1bfc9759b22d68953e2d775858394f21750a024", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/org/typelevel/cats-macros_2.11/1.4.0/cats-macros_2.11-1.4.0.jar", "source": {"sha1": "8d725b9fd4b34d8d65a17568034c8105347ff4ba", "sha256": "ac508ba2354c78d4a8985081a2f7e16a7200b82b00bebaada48f23f4dfa45f97", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/org/typelevel/cats-macros_2.11/1.4.0/cats-macros_2.11-1.4.0-sources.jar"} , "name": "org_typelevel_cats_macros_2_11", "actual": "@org_typelevel_cats_macros_2_11//jar:file", "bind": "jar/org/typelevel/cats_macros_2_11"},
    {"artifact": "org.typelevel:export-hook_2.11:1.2.0", "lang": "scala", "sha1": "d8b073515d14b53b4ff52a3d1387103efa5f5d6e", "sha256": "74441dc09e10a75ce416295081106db28080941a37b06842ca9b443393ca5f69", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/org/typelevel/export-hook_2.11/1.2.0/export-hook_2.11-1.2.0.jar", "source": {"sha1": "c7c75fd232b934b7f3c1dd03b22ee5aa78fd54ad", "sha256": "7477c02188dfbec207cda896b6e7e42221eebb73d3f665cc1701e6350b18dec0", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/org/typelevel/export-hook_2.11/1.2.0/export-hook_2.11-1.2.0-sources.jar"} , "name": "org_typelevel_export_hook_2_11", "actual": "@org_typelevel_export_hook_2_11//jar:file", "bind": "jar/org/typelevel/export_hook_2_11"},
    {"artifact": "org.typelevel:machinist_2.11:0.6.5", "lang": "scala", "sha1": "ef16711a92750ad5ee3131eb27e14f2b55ecfe4d", "sha256": "aa0ba2e98642d499ca26c4062a0397ef81c439fff625ec71ebc755ae70f7c759", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/org/typelevel/machinist_2.11/0.6.5/machinist_2.11-0.6.5.jar", "source": {"sha1": "5124285222bb34f4f8d3b8c393ab4b46a0d7f67a", "sha256": "f92c8346d0c8a56f27c1dc48ea658e15e78197904cefeedf4c89cf4b2c15e22a", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/org/typelevel/machinist_2.11/0.6.5/machinist_2.11-0.6.5-sources.jar"} , "name": "org_typelevel_machinist_2_11", "actual": "@org_typelevel_machinist_2_11//jar:file", "bind": "jar/org/typelevel/machinist_2_11"},
    {"artifact": "org.typelevel:macro-compat_2.11:1.1.1", "lang": "scala", "sha1": "0cb87cb74fd5fb118fede3f98075c2044616b35d", "sha256": "5200a80ad392f0b882021d6de2efb17b874cc179ff8539f9bcedabc100b7890b", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/org/typelevel/macro-compat_2.11/1.1.1/macro-compat_2.11-1.1.1.jar", "source": {"sha1": "363f86f631e1e95fc7989f73a0cea3ee18107cea", "sha256": "4e3438277b20cd64bce0ba31ffc7b8a74da914551c9dea46297508f879a6f220", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/org/typelevel/macro-compat_2.11/1.1.1/macro-compat_2.11-1.1.1-sources.jar"} , "name": "org_typelevel_macro_compat_2_11", "actual": "@org_typelevel_macro_compat_2_11//jar:file", "bind": "jar/org/typelevel/macro_compat_2_11"},
    {"artifact": "org.typelevel:paiges-core_2.11:0.2.1", "lang": "scala", "sha1": "299e599df09e6ab3182eff433f84c93ab458601e", "sha256": "c176a5d76b33e09d92d192854a5d45abed625fb886845b864c94520b20b97635", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/org/typelevel/paiges-core_2.11/0.2.1/paiges-core_2.11-0.2.1.jar", "source": {"sha1": "194f571af2419c157783149817f0e64a29d6eb17", "sha256": "5288df95d67acbf770afe555fa3aa4fae34a21ed648546a4b87b1174db17e0eb", "repository": "https://repo.maven.apache.org/maven2/", "url": "https://repo.maven.apache.org/maven2/org/typelevel/paiges-core_2.11/0.2.1/paiges-core_2.11-0.2.1-sources.jar"} , "name": "org_typelevel_paiges_core_2_11", "actual": "@org_typelevel_paiges_core_2_11//jar:file", "bind": "jar/org/typelevel/paiges_core_2_11"},
    ]

def maven_dependencies(callback = jar_artifact_callback):
    for hash in list_dependencies():
        callback(hash)
