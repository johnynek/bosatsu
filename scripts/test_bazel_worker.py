#!/usr/bin/env python3
"""Compare one-shot Bazel parameter-file requests with direct JVM commands."""
import argparse
import hashlib
import io
import json
from pathlib import Path
import shutil
import subprocess
import tarfile
import tempfile
import zipfile


ROOT = Path(__file__).resolve().parents[1]
SOURCE = "source files 雪/Suite API.bosatsu"
BUNDLE = "generated files 雪/test suite.pkg"
SIGNATURE = "generated files 雪/test suite.sig"
RUNTIME = "manifests 雪/runtime manifest.json"
ROOT_GRAPH = "graphs 雪/root group.json"


def invoke(java: str, jar: Path, cwd: Path, *args: str) -> subprocess.CompletedProcess[bytes]:
    return subprocess.run(
        [java, "-jar", str(jar), *args], cwd=cwd, capture_output=True, check=False
    )


def check_result(
    label: str,
    direct: subprocess.CompletedProcess[bytes],
    worker: subprocess.CompletedProcess[bytes],
) -> None:
    for field in ("returncode", "stdout", "stderr"):
        expected = getattr(direct, field)
        actual = getattr(worker, field)
        if expected != actual:
            if field in ("stdout", "stderr"):
                expected_text = expected.decode("utf-8", errors="replace")
                actual_text = actual.decode("utf-8", errors="replace")
                detail = f"direct {field}:\n{expected_text}\nworker {field}:\n{actual_text}"
            else:
                detail = f"direct {field}: {expected}\nworker {field}: {actual}"
            raise AssertionError(f"{label} differs between direct and worker invocation\n{detail}")


def remove_outputs(cwd: Path, outputs: list[str]) -> None:
    for name in outputs:
        path = cwd / name
        if path.is_file() or path.is_symlink():
            path.unlink()


def output_bytes(cwd: Path, outputs: list[str]) -> dict[str, bytes]:
    result: dict[str, bytes] = {}
    for name in outputs:
        path = cwd / name
        if not path.is_file():
            raise AssertionError(f"command did not create expected output {name!r}")
        result[name] = path.read_bytes()
    return result


def workspace_snapshot(cwd: Path) -> dict[str, bytes]:
    result: dict[str, bytes] = {}
    for path in cwd.rglob("*"):
        relative = path.relative_to(cwd).as_posix()
        if path.is_symlink():
            result[relative] = b"L\0" + path.readlink().as_posix().encode("utf-8")
        elif path.is_file():
            result[relative] = b"F\0" + path.read_bytes()
        elif path.is_dir():
            result[relative] = b"D\0"
    return result


def check_startup_failures(java: str, jar: Path, cwd: Path) -> None:
    params_dir = Path("parameter files 雪")
    params_root = cwd / params_dir
    params_root.mkdir(parents=True, exist_ok=True)
    (params_root / "empty.params").write_bytes(b"")
    (params_root / "empty-operation.params").write_bytes(b"\nheaders")
    (params_root / "invalid-utf8.params").write_bytes(b"headers\n\xc3(")

    cases = (
        ("no parameter argument", (), "requires exactly one @parameter-file argument"),
        (
            "bare filename",
            (str(params_dir / "empty.params"),),
            "requires exactly one @parameter-file argument",
        ),
        ("bare @", ("@",), "requires exactly one @parameter-file argument"),
        (
            "missing file",
            ("@" + (params_dir / "missing.params").as_posix(),),
            "cannot read parameter file",
        ),
        (
            "multiple parameter arguments",
            (
                "@" + (params_dir / "empty.params").as_posix(),
                "@" + (params_dir / "empty-operation.params").as_posix(),
            ),
            "requires exactly one @parameter-file argument",
        ),
        (
            "persistent worker startup",
            ("--persistent_worker",),
            "requires exactly one @parameter-file argument",
        ),
        (
            "empty file",
            ("@" + (params_dir / "empty.params").as_posix(),),
            "is empty",
        ),
        (
            "empty operation line",
            ("@" + (params_dir / "empty-operation.params").as_posix(),),
            "empty worker operation",
        ),
        (
            "invalid UTF-8",
            ("@" + (params_dir / "invalid-utf8.params").as_posix(),),
            "not valid UTF-8",
        ),
    )
    before = workspace_snapshot(cwd)
    for label, args, expected in cases:
        result = invoke(java, jar, cwd, "tool", "bazel-worker", *args)
        stderr = result.stderr.decode("utf-8", errors="replace")
        if result.returncode == 0 or expected not in stderr:
            raise AssertionError(
                f"worker did not reject {label} with a useful diagnostic:\n{stderr}"
            )
        if result.stdout:
            raise AssertionError(f"worker wrote stdout while rejecting {label}: {result.stdout!r}")
        after = workspace_snapshot(cwd)
        if after != before:
            raise AssertionError(f"worker changed or published workspace files while rejecting {label}")


def compare_command(
    java: str,
    jar: Path,
    cwd: Path,
    name: str,
    command: list[str],
    outputs: list[str],
    *,
    line_ending: str = "\n",
    final_newline: bool = True,
    expect_success: bool = True,
    compare_diagnostics: bool = True,
    preserve_outputs_on_failure: bool = False,
) -> subprocess.CompletedProcess[bytes]:
    parameter_path = Path("parameter files 雪") / f"{name}.params"
    parameter_file = cwd / parameter_path
    parameter_file.parent.mkdir(parents=True, exist_ok=True)
    content = line_ending.join(command)
    if final_newline:
        content += line_ending
    parameter_file.write_bytes(content.encode("utf-8"))

    preserved_outputs: dict[str, bytes] = {}
    if expect_success or not preserve_outputs_on_failure:
        remove_outputs(cwd, outputs)
    else:
        for output in outputs:
            path = cwd / output
            path.parent.mkdir(parents=True, exist_ok=True)
            preserved_outputs[output] = b"previous successful toolchain stamp\n"
            path.write_bytes(preserved_outputs[output])

    direct = invoke(java, jar, cwd, "tool", *command)
    direct_outputs = output_bytes(cwd, outputs) if direct.returncode == 0 else {}
    if expect_success and direct.returncode != 0:
        raise AssertionError(
            f"direct {name} command failed ({direct.returncode}):\n"
            f"{direct.stderr.decode('utf-8', errors='replace')}"
        )
    if not expect_success and direct.returncode == 0:
        raise AssertionError(f"direct {name} command unexpectedly succeeded")
    if direct.returncode != 0:
        if preserve_outputs_on_failure:
            actual = {output: (cwd / output).read_bytes() for output in outputs}
            if actual != preserved_outputs:
                raise AssertionError(f"direct {name} changed a previously published output after failure")
        elif any((cwd / output).exists() for output in outputs):
            raise AssertionError(f"direct {name} left an output behind after failure")

    if preserve_outputs_on_failure and not expect_success:
        for output, content in preserved_outputs.items():
            (cwd / output).write_bytes(content)
    else:
        remove_outputs(cwd, outputs)
    worker = invoke(java, jar, cwd, "tool", "bazel-worker", "@" + parameter_path.as_posix())
    if compare_diagnostics:
        check_result(name, direct, worker)
    else:
        if (direct.returncode, direct.stdout) != (worker.returncode, worker.stdout):
            raise AssertionError(f"{name} status or stdout differs between direct and worker invocation")
        # The JVM adds effect trace frames from the two different entry paths.
        # Compare the reported exception and message while ignoring those frames.
        direct_diagnostic = direct.stderr.partition(b"\n\tat ")[0]
        worker_diagnostic = worker.stderr.partition(b"\n\tat ")[0]
        if direct_diagnostic != worker_diagnostic or b"NotDirectoryException" not in direct_diagnostic:
            raise AssertionError(f"{name} diagnostics differ between direct and worker invocation")
    if worker.returncode == 0:
        worker_outputs = output_bytes(cwd, outputs)
        if direct_outputs != worker_outputs:
            raise AssertionError(f"{name} output bytes differ between direct and worker invocation")
    elif preserve_outputs_on_failure:
        actual = {output: (cwd / output).read_bytes() for output in outputs}
        if actual != preserved_outputs:
            raise AssertionError(f"worker {name} changed a previously published output after failure")
    elif any((cwd / output).exists() for output in outputs):
        raise AssertionError(f"{name} left an output behind after failure")
    if expect_success != (worker.returncode == 0):
        raise AssertionError(f"{name} returned unexpected status {worker.returncode}")
    return worker


TOOLCHAIN_MANIFEST = "manifests 雪/toolchain.json"
TOOLCHAIN_STAMP = "generated files 雪/toolchain stamp"
TOOLCHAIN_CORE = "components 雪/core.bosatsu_lib"
TOOLCHAIN_SOURCE = "sources 雪/Prog.bosatsu"
TOOLCHAIN_RUNTIME = "components 雪/c-runtime.tar.gz"


def sha256(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def protobuf_varint(value: int) -> bytes:
    encoded = bytearray()
    while value > 0x7F:
        encoded.append((value & 0x7F) | 0x80)
        value >>= 7
    encoded.append(value)
    return bytes(encoded)


def protobuf_bytes(field_number: int, value: bytes) -> bytes:
    return protobuf_varint((field_number << 3) | 2) + protobuf_varint(len(value)) + value


def core_library_fixture(
    name: str = "core_alpha", repo: str = "https://example.invalid/core"
) -> bytes:
    # Minimal bosatsu.Library protobuf: name (field 1), repo_uri (field 4),
    # and descriptor.version (fields 5 and 1) for version 1.2.3.
    version = b"\x08\x01\x10\x02\x18\x03"
    descriptor = protobuf_bytes(1, version)
    return (
        protobuf_bytes(1, name.encode("utf-8"))
        + protobuf_bytes(4, repo.encode("utf-8"))
        + protobuf_bytes(5, descriptor)
    )


def runtime_archive_fixture() -> bytes:
    archive = io.BytesIO()
    content = b"local runtime fixture\n"
    with tarfile.open(fileobj=archive, mode="w:gz") as tar:
        info = tarfile.TarInfo("c_runtime/fixture.txt")
        info.size = len(content)
        info.mtime = 0
        tar.addfile(info, io.BytesIO(content))
    return archive.getvalue()


def write_toolchain_manifest(
    cwd: Path,
    *,
    compiler_bytes: bytes,
    core_bytes: bytes = b"",
    runtime_bytes: bytes = b"",
    core_sources: dict[str, bytes] | None = None,
    core_name: str = "core_alpha",
) -> None:
    manifest = {
        "schema_version": 1,
        "compiler": {
            "uri": "https://unused.invalid/compiler.jar",
            "sha256": sha256(compiler_bytes),
        },
        "core": {
            "library_name": core_name,
            "version": "1.2.3",
            "uri": "https://unused.invalid/core.bosatsu_lib",
            "sha256": sha256(core_bytes),
        },
        "c_runtime": {
            "uri": "https://unused.invalid/c-runtime.tar.gz",
            "sha256": sha256(runtime_bytes),
        },
    }
    if core_sources is not None:
        manifest["core_sources"] = {
            package: sha256(source) for package, source in core_sources.items()
        }
    path = cwd / TOOLCHAIN_MANIFEST
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(manifest, sort_keys=True), encoding="utf-8")


def toolchain_command(
    compiler_jar: Path,
    *,
    core_library: str | None = None,
    core_sources: list[str] | None = None,
    runtime_archive: str | None = None,
) -> list[str]:
    command = [
        "verify-toolchain",
        "--manifest",
        TOOLCHAIN_MANIFEST,
        "--compiler-jar",
        compiler_jar.as_posix(),
        "--out",
        TOOLCHAIN_STAMP,
    ]
    if core_library is not None:
        command.extend(["--core-library", core_library])
    for source in core_sources or []:
        command.extend(["--core-source", source])
    if runtime_archive is not None:
        command.extend(["--runtime-archive", runtime_archive])
    return command


def test_toolchain_verification(java: str, candidate_jar: Path, work: Path) -> None:
    cwd = work / "toolchain verification workspace"
    cwd.mkdir()
    candidate_name = Path("candidate compiler 雪.jar")
    shutil.copyfile(candidate_jar, cwd / candidate_name)
    candidate_bytes = (cwd / candidate_name).read_bytes()
    write_toolchain_manifest(cwd, compiler_bytes=candidate_bytes)

    # The selected candidate jar is also the exact jar launched for verification.
    compare_command(
        java,
        candidate_name,
        cwd,
        "toolchain-compiler-only",
        toolchain_command(candidate_name),
        [TOOLCHAIN_STAMP],
    )

    core = core_library_fixture()
    (cwd / TOOLCHAIN_CORE).parent.mkdir(parents=True, exist_ok=True)
    (cwd / TOOLCHAIN_CORE).write_bytes(core)
    write_toolchain_manifest(cwd, compiler_bytes=candidate_bytes, core_bytes=core)
    compare_command(
        java,
        candidate_name,
        cwd,
        "toolchain-binary-core",
        toolchain_command(candidate_name, core_library=TOOLCHAIN_CORE),
        [TOOLCHAIN_STAMP],
    )

    wrong_core = core_library_fixture(repo="https://example.invalid/altered-core")
    (cwd / TOOLCHAIN_CORE).write_bytes(wrong_core)
    compare_command(
        java,
        candidate_name,
        cwd,
        "toolchain-substituted-core",
        toolchain_command(candidate_name, core_library=TOOLCHAIN_CORE),
        [TOOLCHAIN_STAMP],
        expect_success=False,
        preserve_outputs_on_failure=True,
    )

    wrong_identity = core_library_fixture(name="other_core")
    (cwd / TOOLCHAIN_CORE).write_bytes(wrong_identity)
    write_toolchain_manifest(
        cwd,
        compiler_bytes=candidate_bytes,
        core_bytes=wrong_identity,
        core_name="core_alpha",
    )
    compare_command(
        java,
        candidate_name,
        cwd,
        "toolchain-core-identity",
        toolchain_command(candidate_name, core_library=TOOLCHAIN_CORE),
        [TOOLCHAIN_STAMP],
        expect_success=False,
    )

    source = b"package Bosatsu/Prog\n\nmain = 1\n"
    (cwd / TOOLCHAIN_SOURCE).parent.mkdir(parents=True, exist_ok=True)
    (cwd / TOOLCHAIN_SOURCE).write_bytes(source)
    write_toolchain_manifest(
        cwd,
        compiler_bytes=candidate_bytes,
        core_sources={"Bosatsu/Prog": source},
    )
    compare_command(
        java,
        candidate_name,
        cwd,
        "toolchain-core-source",
        toolchain_command(candidate_name, core_sources=[TOOLCHAIN_SOURCE]),
        [TOOLCHAIN_STAMP],
    )

    changed_source = b"package Bosatsu/Prog\n\nmain = 2\n"
    (cwd / TOOLCHAIN_SOURCE).write_bytes(changed_source)
    compare_command(
        java,
        candidate_name,
        cwd,
        "toolchain-modified-core-source",
        toolchain_command(candidate_name, core_sources=[TOOLCHAIN_SOURCE]),
        [TOOLCHAIN_STAMP],
        expect_success=False,
        preserve_outputs_on_failure=True,
    )
    wrong_package = b"package Bosatsu/Changed\n\nmain = 1\n"
    (cwd / TOOLCHAIN_SOURCE).write_bytes(wrong_package)
    compare_command(
        java,
        candidate_name,
        cwd,
        "toolchain-unlisted-core-source",
        toolchain_command(candidate_name, core_sources=[TOOLCHAIN_SOURCE]),
        [TOOLCHAIN_STAMP],
        expect_success=False,
    )

    runtime = runtime_archive_fixture()
    (cwd / TOOLCHAIN_RUNTIME).parent.mkdir(parents=True, exist_ok=True)
    (cwd / TOOLCHAIN_RUNTIME).write_bytes(runtime)
    write_toolchain_manifest(cwd, compiler_bytes=candidate_bytes, runtime_bytes=runtime)
    compare_command(
        java,
        candidate_name,
        cwd,
        "toolchain-native-runtime",
        toolchain_command(candidate_name, runtime_archive=TOOLCHAIN_RUNTIME),
        [TOOLCHAIN_STAMP],
    )

    changed_runtime = runtime_archive_fixture() + b"independently hashed substitute"
    (cwd / TOOLCHAIN_RUNTIME).write_bytes(changed_runtime)
    compare_command(
        java,
        candidate_name,
        cwd,
        "toolchain-substituted-runtime",
        toolchain_command(candidate_name, runtime_archive=TOOLCHAIN_RUNTIME),
        [TOOLCHAIN_STAMP],
        expect_success=False,
    )

    altered_name = Path("substituted compiler 雪.jar")
    shutil.copyfile(cwd / candidate_name, cwd / altered_name)
    with zipfile.ZipFile(cwd / altered_name, mode="a") as jar_file:
        jar_file.writestr("toolchain-fixture-marker.txt", b"different candidate bytes\n")
    compare_command(
        java,
        altered_name,
        cwd,
        "toolchain-substituted-compiler",
        toolchain_command(altered_name),
        [TOOLCHAIN_STAMP],
        expect_success=False,
    )


def write_fixture(cwd: Path) -> None:
    (cwd / SOURCE).parent.mkdir(parents=True, exist_ok=True)
    (cwd / SOURCE).write_text(
        'package Demo/Suite\nchecks = Assertion(True, "candidate 雪")\n', encoding="utf-8"
    )
    (cwd / ROOT_GRAPH).parent.mkdir(parents=True, exist_ok=True)
    graph = {
        "schema_version": 1,
        "kind": "group",
        "label": "//demo:suite",
        "members": [{"package": "Demo/Suite", "source": SOURCE}],
        "deps": [],
    }
    (cwd / ROOT_GRAPH).write_text(json.dumps(graph), encoding="utf-8")
    runtime_path = cwd / RUNTIME
    runtime_path.parent.mkdir(parents=True, exist_ok=True)
    runtime = {
        "schema_version": 1,
        "kind": "local",
        "packages": [BUNDLE],
        "externals": [],
    }
    runtime_path.write_text(json.dumps(runtime), encoding="utf-8")


def test_worker(java: str, jar: Path, work: Path) -> None:
    cwd = work / "candidate workspace"
    cwd.mkdir()
    write_fixture(cwd)
    (cwd / "generated files 雪").mkdir()

    headers_result = compare_command(
        java,
        jar,
        cwd,
        "headers",
        ["headers", "--src", SOURCE, "--out", "generated files 雪/headers.json"],
        ["generated files 雪/headers.json"],
    )
    if headers_result.stdout or headers_result.stderr:
        raise AssertionError("successful headers worker invocation must have empty stdout and stderr")
    compare_command(
        java,
        jar,
        cwd,
        "compile-packages",
        [
            "compile-packages",
            "--src",
            SOURCE,
            "--packages-out",
            BUNDLE,
            "--sig-out",
            SIGNATURE,
        ],
        [BUNDLE, SIGNATURE],
        line_ending="\r\n",
    )
    compare_command(
        java,
        jar,
        cwd,
        "validate-graph",
        [
            "validate-graph",
            "--root",
            ROOT_GRAPH,
            "--out",
            "generated files 雪/validated stamp",
        ],
        ["generated files 雪/validated stamp"],
        final_newline=False,
    )
    compare_command(
        java,
        jar,
        cwd,
        "emit-c",
        [
            "emit-c",
            "--runtime-manifest",
            RUNTIME,
            "--test-packages",
            BUNDLE,
            "--out",
            "generated files 雪/test suite.c",
        ],
        ["generated files 雪/test suite.c"],
        line_ending="\r\n",
    )

    compare_command(
        java,
        jar,
        cwd,
        "missing-source",
        [
            "headers",
            "--src",
            "missing input 雪.bosatsu",
            "--out",
            "generated files 雪/missing.json",
        ],
        ["generated files 雪/missing.json"],
        expect_success=False,
    )
    compare_command(
        java,
        jar,
        cwd,
        "invalid-option",
        [
            "compile-packages",
            "--src",
            SOURCE,
            "--packages-out",
            "generated files 雪/invalid.pkg",
            "--sig-out",
            "generated files 雪/invalid.sig",
            "--cache_dir",
            "cache",
        ],
        ["generated files 雪/invalid.pkg", "generated files 雪/invalid.sig"],
        expect_success=False,
    )
    compare_command(
        java,
        jar,
        cwd,
        "empty-argument",
        ["headers", "--src", "", "--out", "generated files 雪/empty-argument.json"],
        ["generated files 雪/empty-argument.json"],
        expect_success=False,
    )

    for operation in ("check", "fetch", "transpile", "publish-artifacts"):
        params = cwd / "parameter files 雪" / f"reject-{operation}.params"
        params.write_text(operation + "\n", encoding="utf-8")
        result = invoke(java, jar, cwd, "tool", "bazel-worker", "@" + params.relative_to(cwd).as_posix())
        message = result.stderr.decode("utf-8", errors="replace")
        if result.returncode == 0 or f"unsupported bazel worker operation: {operation}" not in message:
            raise AssertionError(f"worker accepted or misdiagnosed blocked operation {operation}:\n{message}")

    blocked_parent = cwd / "blocked output 雪"
    blocked_parent.write_text("not a directory", encoding="utf-8")
    blocked_output = "blocked output 雪/retry.json"
    compare_command(
        java,
        jar,
        cwd,
        "blocked-write",
        ["headers", "--src", SOURCE, "--out", blocked_output],
        [blocked_output],
        expect_success=False,
        compare_diagnostics=False,
    )
    blocked_parent.unlink()
    blocked_parent.mkdir()
    compare_command(
        java,
        jar,
        cwd,
        "retry-after-blocked-write",
        ["headers", "--src", SOURCE, "--out", blocked_output],
        [blocked_output],
    )
    check_startup_failures(java, jar, cwd)


def test_bosatsuj(java: str, jar: Path, work: Path) -> None:
    repo = work / "bosatsuj fixture"
    assembly_dir = repo / "cli" / "target" / jar.parent.name
    assembly_dir.mkdir(parents=True)
    shutil.copyfile(jar, assembly_dir / jar.name)
    shutil.copyfile(ROOT / "bosatsuj", repo / "bosatsuj")
    (repo / "bosatsuj").chmod(0o755)

    cwd = work / "bosatsuj workspace"
    cwd.mkdir()
    write_fixture(cwd)
    output = "generated files 雪/launcher headers.json"
    (cwd / "generated files 雪").mkdir()
    direct = invoke(java, jar, cwd, "tool", "headers", "--src", SOURCE, "--out", output)
    direct_output = (cwd / output).read_bytes() if direct.returncode == 0 else b""
    (cwd / output).unlink(missing_ok=True)

    params = cwd / "parameter files 雪" / "launcher.params"
    params.parent.mkdir(parents=True, exist_ok=True)
    params.write_text("headers\n--src\n" + SOURCE + "\n--out\n" + output + "\n", encoding="utf-8")
    launched = subprocess.run(
        [str(repo / "bosatsuj"), "tool", "bazel-worker", "@parameter files 雪/launcher.params"],
        cwd=cwd,
        capture_output=True,
        check=False,
    )
    check_result("bosatsuj launcher", direct, launched)
    if launched.returncode != 0 or (cwd / output).read_bytes() != direct_output:
        raise AssertionError("bosatsuj did not execute the candidate jar with the worker arguments")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--jar", type=Path, required=True)
    args = parser.parse_args()
    jar = args.jar.resolve()
    if not jar.is_file():
        parser.error(f"candidate jar does not exist: {jar}")
    java = shutil.which("java")
    if java is None:
        raise RuntimeError("java is required")
    with tempfile.TemporaryDirectory(prefix="bosatsu-bazel-worker-") as tmp:
        work = Path(tmp)
        test_toolchain_verification(java, jar, work)
        test_worker(java, jar, work)
        test_bosatsuj(java, jar, work)
    print("candidate jar Bazel worker acceptance passed")


if __name__ == "__main__":
    main()
