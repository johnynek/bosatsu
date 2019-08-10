BosatsuProvider = provider(fields = ["transitive_deps", "transitive_sigs"])

def _collect_deps(ctx):

  deps = [dep_target[BosatsuProvider].transitive_deps for dep_target in ctx.attr.deps]
  sigs = [dep_target[BosatsuProvider].transitive_sigs for dep_target in ctx.attr.deps]

  return BosatsuProvider(transitive_deps = depset(transitive = deps),
                         transitive_sigs = depset(transitive = sigs))

def _bosatsu_library_impl(ctx):
  provider = _collect_deps(ctx)

  args = ["type-check"]
  for f in ctx.files.srcs:
    args += ["--input", f.path]
  for f in provider.transitive_sigs.to_list():
    args += ["--interface", f.path]

  args += ["--interface_out", ctx.outputs.interface.path]
  args += ["--output", ctx.outputs.output.path]

  ctx.actions.run(
      inputs = depset(ctx.files.srcs, transitive=[provider.transitive_sigs]),
      outputs = [ctx.outputs.interface, ctx.outputs.output],
      executable = ctx.executable._bosatsu_main,
      mnemonic = "Bosatsu",
      progress_message = "bosatsu %s (%s files)" % (ctx.label, len(ctx.files.srcs)),
      arguments = args,
      )

  result = BosatsuProvider(
      transitive_deps = depset([ctx.outputs.output], transitive = [provider.transitive_deps]),
      transitive_sigs = depset([ctx.outputs.interface], transitive = [provider.transitive_sigs]))

  return [result]

bosatsu_library = rule(
    implementation = _bosatsu_library_impl,
    attrs = {
        "srcs": attr.label_list(mandatory=False, allow_files=[".bosatsu"]),
        "deps": attr.label_list(),
        "_bosatsu_main": attr.label(executable=True, cfg="host", default=Label("//cli/src/main/scala/org/bykn/bosatsu:bosatsu_main")),
    },
    outputs = {
      "interface": "%{name}.bosatsig",
      "output": "%{name}.bosatsu_package",
    },
)

def _bosatsu_json_impl(ctx):
  provider = _collect_deps(ctx)

  args = ["write-json"]
  for f in ctx.files.srcs:
    args += ["--input", f.path]
  for f in provider.transitive_deps.to_list():
    args += ["--include", f.path]

  args += ["--output", ctx.outputs.json.path]
  args += ["--main", ctx.attr.package]

  ctx.actions.run(
      inputs = depset(ctx.files.srcs, transitive=[provider.transitive_deps]),
      outputs = [ctx.outputs.json],
      executable = ctx.executable._bosatsu_main,
      mnemonic = "Bosatsu",
      progress_message = "bosatsu %s (%s files)" % (ctx.label, len(ctx.files.srcs)),
      arguments = args,
      )

  return []

bosatsu_json = rule(
    implementation = _bosatsu_json_impl,
    attrs = {
        "srcs": attr.label_list(mandatory=False, allow_files=[".bosatsu"]),
        "deps": attr.label_list(),
        "package": attr.string(),
        "_bosatsu_main": attr.label(executable=True, cfg="host", default=Label("//cli/src/main/scala/org/bykn/bosatsu:bosatsu_main")),
    },
    outputs = {
      "json": "%{name}.json",
    })

def _bosatsu_test_impl(ctx):
  provider = _collect_deps(ctx)

  all_inputs = depset(ctx.files.srcs + [ctx.executable._bosatsu_main], transitive = [provider.transitive_deps])
  rfs = ctx.runfiles(transitive_files = all_inputs, collect_default = True)

  args = ["test"]
  for f in ctx.files.srcs:
    args += ["--input", f.short_path]
  for f in provider.transitive_deps.to_list():
    args += ["--include", f.short_path]
  for p in ctx.attr.packages:
    args += ["--test_package", p]

  ctx.actions.write(
      output = ctx.outputs.executable,
      content = """#!/bin/sh
{path} {arg}
""".format(path = ctx.executable._bosatsu_main.short_path,
           arg = " ".join(args)))

  return [DefaultInfo(runfiles = rfs)]

bosatsu_test = rule(
    implementation = _bosatsu_test_impl,
    attrs = {
        "srcs": attr.label_list(mandatory=False, allow_files=[".bosatsu"]),
        "deps": attr.label_list(),
        "packages": attr.string_list(),
        "data": attr.label_list(default=[Label("//cli/src/main/scala/org/bykn/bosatsu:bosatsu_main")]),
        "_bosatsu_main": attr.label(executable=True, cfg="host", default=Label("//cli/src/main/scala/org/bykn/bosatsu:bosatsu_main")),
    },
    executable=True,
    test=True)
