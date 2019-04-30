BosatsuProvider = provider(fields = ["transitive_deps", "transitive_sigs", "runfiles"])

def _collect_deps(ctx):
  transitive_deps = depset()
  transitive_sigs = depset()

  for dep_target in ctx.attr.deps:
    if BosatsuProvider in dep_target:
        bosatsu_provider = dep_target[BosatsuProvider]
        transitive_deps += bosatsu_provider.transitive_deps
        transitive_sigs += bosatsu_provider.transitive_sigs
    else:
      fail("expected a Bosatsu provider but missing in: %s" % dep_target)

  return BosatsuProvider(transitive_deps = transitive_deps,
                         transitive_sigs = transitive_sigs,
                         runfiles = None)

def _add_self(ctx, prov):
  outs = []
  if hasattr(ctx.outputs, "signature"):
    outs += [ctx.outputs.signature]
  if hasattr(ctx.outputs, "json"):
    outs += [ctx.outputs.json]

  return BosatsuProvider(transitive_deps = prov.transitive_deps + ctx.files.srcs,
                  transitive_sigs = prov.transitive_sigs + outs)

def _bosatsu_library_impl(ctx):
  provider = _collect_deps(ctx)

  all_inputs = provider.transitive_deps + ctx.files.srcs
  args = ["type-check"]
  for f in all_inputs:
    args += ["--input", f.path]
  for f in provider.transitive_sigs:
    args += ["--signature", f.path]

  args += ["--output", ctx.outputs.signature.path]

  ctx.action(
      inputs = all_inputs + provider.transitive_sigs, # TODO only use signatures of dependencies
      outputs = [ctx.outputs.signature],
      executable = ctx.executable._bosatsu_main,
      mnemonic = "Bosatsu",
      progress_message = "bosatsu %s (%s files)" % (ctx.label, len(ctx.files.srcs)),
      arguments = args,
      )

  return struct(providers = [_add_self(ctx, provider)])

bosatsu_library = rule(
    implementation = _bosatsu_library_impl,
    attrs = {
        "srcs": attr.label_list(mandatory=False, allow_files=FileType([".bosatsu"])),
        "deps": attr.label_list(),
        "_bosatsu_main": attr.label(executable=True, cfg="host", default=Label("//core/src/main/scala/org/bykn/bosatsu:bosatsu_main")),
    },
    outputs = {
      "signature": "%{name}.bosatsig",
    },
)

def _bosatsu_json_impl(ctx):
  provider = _collect_deps(ctx)

  all_inputs = provider.transitive_deps + ctx.files.srcs
  args = ["write-json"]
  for f in all_inputs:
    args += ["--input", f.path]
  for f in provider.transitive_sigs:
    args += ["--signature", f.path]

  args += ["--output", ctx.outputs.json.path]
  args += ["--main", ctx.attr.package]

  ctx.action(
      inputs = all_inputs + provider.transitive_sigs, # TODO only use signatures of dependencies
      outputs = [ctx.outputs.json],
      executable = ctx.executable._bosatsu_main,
      mnemonic = "Bosatsu",
      progress_message = "bosatsu %s (%s files)" % (ctx.label, len(ctx.files.srcs)),
      arguments = args,
      )

  return struct(providers = [_add_self(ctx, provider)])

bosatsu_json = rule(
    implementation = _bosatsu_json_impl,
    attrs = {
        "srcs": attr.label_list(mandatory=False, allow_files=FileType([".bosatsu"])),
        "deps": attr.label_list(),
        "package": attr.string(),
        "_bosatsu_main": attr.label(executable=True, cfg="host", default=Label("//core/src/main/scala/org/bykn/bosatsu:bosatsu_main")),
    },
    outputs = {
      "json": "%{name}.json",
    })

def _bosatsu_test_impl(ctx):
  provider = _collect_deps(ctx)

  all_inputs = provider.transitive_deps + ctx.files.srcs + [ctx.executable._bosatsu_main]
  rfs = ctx.runfiles(transitive_files = all_inputs, collect_default = True)

  all_inputs = provider.transitive_deps + ctx.files.srcs
  args = ["test"]
  for f in ctx.files.srcs:
    args += ["--input", f.short_path]
  for f in provider.transitive_deps:
    args += ["--test_deps", f.short_path]
  for p in ctx.attr.packages:
    args += ["--test_package", p]

  ctx.file_action(
      output = ctx.outputs.executable,
      content = """#!/bin/sh
{path} {arg}
""".format(path = ctx.executable._bosatsu_main.short_path,
           arg = " ".join(args)))

  return [DefaultInfo(runfiles = rfs)]

bosatsu_test = rule(
    implementation = _bosatsu_test_impl,
    attrs = {
        "srcs": attr.label_list(mandatory=False, allow_files=FileType([".bosatsu"])),
        "deps": attr.label_list(),
        "packages": attr.string_list(),
        "data": attr.label_list(cfg="data",default=[Label("//core/src/main/scala/org/bykn/bosatsu:bosatsu_main")]),
        "_bosatsu_main": attr.label(executable=True, cfg="host", default=Label("//core/src/main/scala/org/bykn/bosatsu:bosatsu_main")),
    },
    executable=True,
    test=True)
