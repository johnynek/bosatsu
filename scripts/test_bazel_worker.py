#!/usr/bin/env python3
"""Compare direct, one-shot, and persistent JVM file-command invocations."""
import argparse
import json
import os
from pathlib import Path
import select
import shutil
import subprocess
import tempfile
import time


ROOT = Path(__file__).resolve().parents[1]
SOURCE = "source files 雪/Suite API.bosatsu"
BUNDLE = "generated files 雪/test suite.pkg"
SIGNATURE = "generated files 雪/test suite.sig"
RUNTIME = "manifests 雪/runtime manifest.json"
ROOT_GRAPH = "graphs 雪/root group.json"

STRICT_SOURCES = "strict sources 雪"
STRICT_OUTPUTS = "strict outputs 雪"
STRICT_LEAF = f"{STRICT_SOURCES}/Leaf.bosatsu"
STRICT_Y = f"{STRICT_SOURCES}/Y.bosatsu"
STRICT_PRIVATE = f"{STRICT_SOURCES}/Private.bosatsu"
STRICT_SIBLING = f"{STRICT_SOURCES}/Sibling.bosatsu"
STRICT_X = f"{STRICT_SOURCES}/X.bosatsu"
STRICT_CLIENT = f"{STRICT_SOURCES}/Client.bosatsu"
STRICT_ONE = f"{STRICT_SOURCES}/One.bosatsu"
STRICT_TWO = f"{STRICT_SOURCES}/Two.bosatsu"
STRICT_LEFT = f"{STRICT_SOURCES}/Left.bosatsu"
STRICT_RIGHT = f"{STRICT_SOURCES}/Right.bosatsu"
STRICT_APP = f"{STRICT_SOURCES}/App.bosatsu"
STRICT_LABELS = "strict interface labels 雪.json"

STRICT_LEAF_SOURCE = (
    "package Leaf\nexport Leaf, leaf\nstruct Leaf(n: Int)\nleaf = Leaf(1)\n"
)
STRICT_Y_SOURCE = (
    "package Y\nfrom Leaf import Leaf, leaf\nexport Token(), token\n"
    "exposes Leaf\nstruct Token(value: Leaf)\ntoken = Token(leaf)\n"
)
STRICT_PRIVATE_SOURCE = "package Private\nexport enabled\nenabled = True\n"
STRICT_SIBLING_SOURCE = "package Sibling\nexport number\nnumber = 7\n"
STRICT_X_SOURCE = (
    "package X\nfrom Private import enabled\nfrom Y import token\n"
    "export shared\nexposes Y\nshared = if enabled: token\n         else: token\n"
)
STRICT_X_REDUCED_SOURCE = "package X\nexport shared\nshared = 1\n"
STRICT_CLIENT_X_SOURCE = "package Client\nfrom X import shared\nmain = [shared]\n"
STRICT_CLIENT_SIBLING_SOURCE = (
    "package Client\nfrom Sibling import number\nmain = number\n"
)
STRICT_PROVIDER_INT_SOURCE = (
    "package Shared/Api\nexport Token(), token\nstruct Token(value: Int)\ntoken = Token(1)\n"
)
STRICT_PROVIDER_BOOL_SOURCE = (
    "package Shared/Api\nexport Token(), token\nstruct Token(value: Bool)\ntoken = Token(True)\n"
)
STRICT_APP_BOTH_SOURCE = (
    "package App\nfrom Left import value as x\nfrom Right import value as y\nmain = (x, y)\n"
)
STRICT_APP_LEFT_SOURCE = "package App\nfrom Left import value\nmain = [value]\n"


def invoke(java: str, jar: Path, cwd: Path, *args: str) -> subprocess.CompletedProcess[bytes]:
    return subprocess.run(
        [java, "-jar", str(jar), *args],
        cwd=cwd,
        capture_output=True,
        check=False,
        timeout=60,
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


def path_states(cwd: Path, paths: list[str]) -> dict[str, tuple[str, bytes | str | None]]:
    states: dict[str, tuple[str, bytes | str | None]] = {}
    for name in paths:
        path = cwd / name
        if path.is_symlink():
            states[name] = ("symlink", path.readlink().as_posix())
        elif path.is_file():
            states[name] = ("file", path.read_bytes())
        elif path.is_dir():
            states[name] = ("directory", None)
        else:
            states[name] = ("absent", None)
    return states


def restore_path_states(
    cwd: Path, states: dict[str, tuple[str, bytes | str | None]]
) -> None:
    for name, (kind, value) in states.items():
        path = cwd / name
        if path.is_symlink() or path.is_file():
            path.unlink()
        elif path.is_dir() and kind != "directory":
            shutil.rmtree(path)
        if kind == "file":
            path.parent.mkdir(parents=True, exist_ok=True)
            assert isinstance(value, bytes)
            path.write_bytes(value)
        elif kind == "symlink":
            path.parent.mkdir(parents=True, exist_ok=True)
            assert isinstance(value, str)
            path.symlink_to(value)
        elif kind == "directory":
            path.mkdir(parents=True, exist_ok=True)


def assert_no_scratch(cwd: Path, label: str) -> None:
    leftovers = [
        path.relative_to(cwd).as_posix()
        for path in cwd.rglob("*")
        if ".bosatsu-stage-" in path.name or path.name.startswith("bosatsu-request-")
    ]
    if leftovers:
        raise AssertionError(f"{label} left request scratch behind: {leftovers!r}")


def changed_paths(before: dict[str, bytes], after: dict[str, bytes]) -> set[str]:
    return {
        name
        for name in before.keys() | after.keys()
        if before.get(name) != after.get(name)
    }


def without_jvm_trace_frames(message: str) -> str:
    return "".join(
        line for line in message.splitlines(keepends=True) if not line.startswith("\tat ")
    )


class PersistentWorker:
    """One bounded, context-managed reader for a real candidate JVM process."""

    def __init__(self, java: str, jar: Path, cwd: Path, timeout: float = 15):
        self.timeout = timeout
        self._stderr_dir = tempfile.TemporaryDirectory(prefix="bosatsu-worker-stderr-")
        self._stderr_path = Path(self._stderr_dir.name) / "stderr.log"
        try:
            with self._stderr_path.open("wb") as stderr_writer:
                self.process = subprocess.Popen(
                    [java, "-jar", str(jar), "tool", "bazel-worker", "--persistent_worker"],
                    cwd=cwd,
                    stdin=subprocess.PIPE,
                    stdout=subprocess.PIPE,
                    stderr=stderr_writer,
                    bufsize=0,
                )
            assert self.process.stdin is not None
            assert self.process.stdout is not None
            self._stderr_reader = self._stderr_path.open("rb", buffering=0)
            self._stdout_buffer = bytearray()
        except BaseException:
            process = getattr(self, "process", None)
            if process is not None and process.poll() is None:
                process.kill()
                process.wait(timeout=5)
            self._stderr_dir.cleanup()
            raise

    def __enter__(self) -> "PersistentWorker":
        return self

    def __exit__(self, _kind: object, _value: object, _traceback: object) -> None:
        if self.process.poll() is None:
            self._stop()
        elif self.process.stdin is not None:
            try:
                self.process.stdin.close()
            except (BrokenPipeError, OSError):
                pass
            self.process.stdin = None
        if self.process.stdout is not None:
            self.process.stdout.close()
        self._stderr_reader.close()
        self._stderr_dir.cleanup()

    def _stop(self) -> None:
        if self.process.stdin is not None:
            try:
                self.process.stdin.close()
            except (BrokenPipeError, OSError):
                pass
            self.process.stdin = None
        if self.process.poll() is None:
            try:
                self.process.terminate()
            except ProcessLookupError:
                pass
            try:
                self.process.wait(timeout=2)
            except subprocess.TimeoutExpired:
                self.process.kill()
                self.process.wait(timeout=5)

    def _send(self, payload: bytes) -> None:
        if self.process.poll() is not None or self.process.stdin is None:
            raise AssertionError("persistent worker exited before the next request")
        try:
            self.process.stdin.write(payload)
            self.process.stdin.flush()
        except (BrokenPipeError, OSError) as error:
            raise AssertionError("persistent worker closed stdin before its response") from error

    def _take_line(self) -> bytes | None:
        newline = self._stdout_buffer.find(b"\n")
        if newline < 0:
            return None
        line = bytes(self._stdout_buffer[: newline + 1])
        del self._stdout_buffer[: newline + 1]
        return line

    def _read_line(self) -> bytes:
        deadline = time.monotonic() + self.timeout
        while True:
            line = self._take_line()
            if line is not None:
                return line
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise AssertionError("timed out waiting for a complete persistent response line")
            assert self.process.stdout is not None
            ready, _, _ = select.select(
                [self.process.stdout], [], [], min(remaining, 0.05)
            )
            if ready:
                chunk = os.read(self.process.stdout.fileno(), 65536)
                if not chunk:
                    raise AssertionError("persistent worker closed stdout before a complete response")
                self._stdout_buffer.extend(chunk)
            elif self.process.poll() is not None:
                raise AssertionError(
                    "persistent worker exited without a response: "
                    f"{self.stderr_bytes().decode('utf-8', errors='replace')}"
                )

    def _read_response(self, expected_id: int) -> dict[str, object]:
        line = self._read_line()
        if not line.endswith(b"\n") or line.count(b"\n") != 1:
            raise AssertionError(f"worker response is not one LF-terminated frame: {line!r}")
        try:
            response = json.loads(line[:-1].decode("utf-8", errors="strict"))
        except (UnicodeDecodeError, json.JSONDecodeError) as error:
            raise AssertionError(f"worker wrote invalid response JSON: {line!r}") from error
        if not isinstance(response, dict):
            raise AssertionError(f"worker response is not a JSON object: {response!r}")
        if set(response) != {"requestId", "exitCode", "output", "wasCancelled"}:
            raise AssertionError(f"worker response has an unexpected shape: {response!r}")
        if type(response["requestId"]) is not int or response["requestId"] != expected_id:
            raise AssertionError(f"worker returned the wrong request ID: {response!r}")
        if type(response["exitCode"]) is not int or not isinstance(response["output"], str):
            raise AssertionError(f"worker returned invalid response fields: {response!r}")
        if type(response["wasCancelled"]) is not bool or response["wasCancelled"]:
            raise AssertionError(f"worker unexpectedly reported cancellation: {response!r}")
        self._assert_no_extra_stdout()
        self.assert_stderr_empty()
        if self.process.poll() is not None:
            raise AssertionError("persistent worker exited before stdin was closed")
        return response

    def _assert_no_extra_stdout(self, quiet_period: float = 0.05) -> None:
        if self._stdout_buffer:
            raise AssertionError(f"worker wrote extra stdout after its response: {bytes(self._stdout_buffer)!r}")
        assert self.process.stdout is not None
        ready, _, _ = select.select([self.process.stdout], [], [], quiet_period)
        if ready:
            extra = os.read(self.process.stdout.fileno(), 65536)
            if extra:
                raise AssertionError(f"worker wrote extra stdout after its response: {extra!r}")

    def stderr_bytes(self) -> bytes:
        self._stderr_reader.seek(0)
        return self._stderr_reader.read()

    def assert_stderr_empty(self) -> None:
        stderr = self.stderr_bytes()
        if stderr:
            raise AssertionError(f"ordinary worker request wrote to stderr: {stderr!r}")

    @staticmethod
    def _request_bytes(payload: dict[str, object]) -> bytes:
        return json.dumps(payload, ensure_ascii=False, separators=(",", ":")).encode("utf-8") + b"\n"

    def request(self, payload: dict[str, object]) -> dict[str, object]:
        expected_id = payload.get("requestId", payload.get("request_id", 0))
        if type(expected_id) is not int:
            raise AssertionError(f"test request ID must be an integer: {expected_id!r}")
        self._send(self._request_bytes(payload))
        return self._read_response(expected_id)

    def control_then_request(
        self, control: dict[str, object], payload: dict[str, object]
    ) -> dict[str, object]:
        expected_id = payload.get("requestId", payload.get("request_id", 0))
        if type(expected_id) is not int:
            raise AssertionError(f"test request ID must be an integer: {expected_id!r}")
        self._send(self._request_bytes(control) + self._request_bytes(payload))
        return self._read_response(expected_id)

    def raw(self, payload: bytes) -> None:
        self._send(payload)

    def finish(self) -> tuple[int, bytes, bytes]:
        if self.process.stdin is not None:
            try:
                self.process.stdin.close()
            except (BrokenPipeError, OSError):
                pass
            self.process.stdin = None
        deadline = time.monotonic() + self.timeout
        extra = bytearray(self._stdout_buffer)
        self._stdout_buffer.clear()
        eof = False
        assert self.process.stdout is not None
        while not (eof and self.process.poll() is not None):
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                self._stop()
                raise AssertionError("persistent worker did not exit after stdin EOF")
            ready, _, _ = select.select(
                [self.process.stdout], [], [], min(remaining, 0.05)
            )
            if ready:
                chunk = os.read(self.process.stdout.fileno(), 65536)
                if chunk:
                    extra.extend(chunk)
                else:
                    eof = True
                    if self.process.poll() is None:
                        time.sleep(0.01)
        status = self.process.wait(timeout=max(0.1, deadline - time.monotonic()))
        return status, bytes(extra), self.stderr_bytes()


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
            "mixed persistent and parameter-file startup",
            ("--persistent_worker", "@" + (params_dir / "empty.params").as_posix()),
            "requires exactly one @parameter-file argument",
        ),
        (
            "extra persistent startup argument",
            ("--persistent_worker", "extra"),
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
    persistent: PersistentWorker,
    name: str,
    command: list[str],
    outputs: list[str],
    *,
    line_ending: str = "\n",
    final_newline: bool = True,
    expect_success: bool = True,
    compare_diagnostics: bool = True,
    request_fields: dict[str, object] | None = None,
    extra_paths: list[str] | None = None,
    control_request: dict[str, object] | None = None,
) -> dict[str, object]:
    parameter_path = Path("parameter files 雪") / f"{name}.params"
    parameter_file = cwd / parameter_path
    parameter_file.parent.mkdir(parents=True, exist_ok=True)
    content = line_ending.join(command)
    if final_newline:
        content += line_ending
    parameter_file.write_bytes(content.encode("utf-8"))

    state_paths = list(dict.fromkeys(outputs + (extra_paths or [])))
    initial_states = path_states(cwd, state_paths)
    direct: subprocess.CompletedProcess[bytes] | None = None
    expected_outputs: dict[str, bytes] = {}
    persistent_response: dict[str, object] | None = None

    routes = ("direct", "params", "persistent")
    for route in routes:
        restore_path_states(cwd, initial_states)
        before = workspace_snapshot(cwd)
        if route == "direct":
            result = invoke(java, jar, cwd, "tool", *command)
            direct = result
            if expect_success and result.returncode != 0:
                raise AssertionError(
                    f"direct {name} command failed ({result.returncode}):\n"
                    f"{result.stderr.decode('utf-8', errors='replace')}"
                )
            if not expect_success and result.returncode == 0:
                raise AssertionError(f"direct {name} command unexpectedly succeeded")
        elif route == "params":
            if direct is None:
                raise AssertionError("direct route did not run before @params")
            result = invoke(java, jar, cwd, "tool", "bazel-worker", "@" + parameter_path.as_posix())
            if compare_diagnostics:
                check_result(name, direct, result)
            else:
                if (direct.returncode, direct.stdout) != (result.returncode, result.stdout):
                    raise AssertionError(f"{name} status or stdout differs between direct and @params")
                direct_error = without_jvm_trace_frames(direct.stderr.decode("utf-8", errors="replace"))
                params_error = without_jvm_trace_frames(result.stderr.decode("utf-8", errors="replace"))
                if direct_error != params_error:
                    raise AssertionError(f"{name} diagnostics differ between direct and @params")
        else:
            if direct is None:
                raise AssertionError("direct route did not run before persistent worker")
            payload: dict[str, object] = {"arguments": command}
            payload.update(request_fields or {})
            if payload.get("arguments") != command:
                raise AssertionError("request_fields cannot replace the persistent command arguments")
            if control_request is None:
                persistent_response = persistent.request(payload)
            else:
                persistent_response = persistent.control_then_request(control_request, payload)
            if persistent_response["exitCode"] != direct.returncode:
                raise AssertionError(
                    f"{name} status differs through persistent worker: "
                    f"direct={direct.returncode}, persistent={persistent_response!r}"
                )
            direct_output = (direct.stdout + direct.stderr).decode("utf-8", errors="strict")
            actual_output = persistent_response["output"]
            assert isinstance(actual_output, str)
            if compare_diagnostics:
                if actual_output != direct_output:
                    raise AssertionError(
                        f"{name} diagnostics differ through persistent worker\n"
                        f"direct stdout+stderr:\n{direct_output}\n"
                        f"persistent output:\n{actual_output}"
                    )
            elif without_jvm_trace_frames(actual_output) != without_jvm_trace_frames(direct_output):
                raise AssertionError(f"{name} diagnostics differ through persistent worker")

        after = workspace_snapshot(cwd)
        unexpected = changed_paths(before, after) - set(outputs)
        if unexpected:
            raise AssertionError(f"{name} {route} route changed undeclared workspace files: {sorted(unexpected)}")
        assert_no_scratch(cwd, f"{name} {route} route")
        if route == "direct" and direct is not None and direct.returncode == 0:
            expected_outputs = output_bytes(cwd, outputs)
        if expect_success:
            actual_outputs = output_bytes(cwd, outputs)
            if route == "direct":
                expected_outputs = actual_outputs
            elif actual_outputs != expected_outputs:
                raise AssertionError(f"{name} output bytes differ through {route}")
        elif path_states(cwd, state_paths) != initial_states:
            raise AssertionError(f"{name} {route} failure changed prior outputs or obstructions")

    assert persistent_response is not None
    return persistent_response


def strict_signature(name: str) -> str:
    return f"{STRICT_OUTPUTS}/{name}.sig"


def strict_compile_command(
    output: str,
    sources: list[str],
    interfaces: list[str],
    *,
    labels: str | None = None,
) -> list[str]:
    command = [
        "compile-packages",
        "--packages-out",
        f"{output}.pkg",
        "--sig-out",
        f"{output}.sig",
    ]
    for source in sources:
        command.extend(("--src", source))
    for interface in interfaces:
        command.extend(("--direct-interface", interface))
    command.append("--error-unused-interfaces")
    if labels is not None:
        command.extend(("--interface-labels", labels))
    return command


def strict_compare(
    java: str,
    jar: Path,
    cwd: Path,
    persistent: PersistentWorker,
    name: str,
    output: str,
    sources: list[str],
    interfaces: list[str],
    *,
    labels: str | None = None,
    expect_success: bool = True,
    request_id: int = 0,
) -> dict[str, object]:
    return compare_command(
        java,
        jar,
        cwd,
        persistent,
        name,
        strict_compile_command(output, sources, interfaces, labels=labels),
        [f"{output}.pkg", f"{output}.sig"],
        expect_success=expect_success,
        request_fields={"requestId": request_id},
    )


def strict_prepare(
    java: str,
    jar: Path,
    cwd: Path,
    name: str,
    output: str,
    sources: list[str],
    interfaces: list[str],
) -> None:
    command = strict_compile_command(output, sources, interfaces)
    result = invoke(java, jar, cwd, "tool", *command)
    if result.returncode != 0:
        raise AssertionError(
            f"candidate jar could not prepare {name}:\n"
            f"{result.stderr.decode('utf-8', errors='replace')}"
        )
    output_bytes(cwd, [f"{output}.pkg", f"{output}.sig"])
    assert_no_scratch(cwd, name)


def write_strict_labels(cwd: Path, entries: list[tuple[str, str]]) -> None:
    manifest = {
        "schema_version": 1,
        "interfaces": [{"path": path, "label": label} for path, label in entries],
    }
    (cwd / STRICT_LABELS).write_text(
        json.dumps(manifest, ensure_ascii=False), encoding="utf-8"
    )


def write_strict_sources(cwd: Path) -> None:
    (cwd / STRICT_SOURCES).mkdir(parents=True, exist_ok=True)
    (cwd / STRICT_OUTPUTS).mkdir(parents=True, exist_ok=True)
    for path, source in (
        (STRICT_LEAF, STRICT_LEAF_SOURCE),
        (STRICT_Y, STRICT_Y_SOURCE),
        (STRICT_PRIVATE, STRICT_PRIVATE_SOURCE),
        (STRICT_SIBLING, STRICT_SIBLING_SOURCE),
        (STRICT_X, STRICT_X_SOURCE),
        (STRICT_CLIENT, STRICT_CLIENT_X_SOURCE),
        (STRICT_ONE, STRICT_PROVIDER_INT_SOURCE),
        (STRICT_TWO, STRICT_PROVIDER_BOOL_SOURCE),
        (STRICT_LEFT, ""),
        (STRICT_RIGHT, ""),
        (STRICT_APP, STRICT_APP_LEFT_SOURCE),
    ):
        (cwd / path).write_text(source, encoding="utf-8")


def strict_facade_source(name: str) -> str:
    return (
        f"package {name}\nfrom Shared/Api import token\nexport value\n"
        "exposes Shared/Api\nvalue = token\n"
    )


def assert_strict_output(response: dict[str, object], *witnesses: str) -> str:
    output = response["output"]
    if not isinstance(output, str):
        raise AssertionError(f"strict-interface worker output was not text: {response!r}")
    for witness in witnesses:
        if witness not in output:
            raise AssertionError(
                f"strict-interface diagnostic lacks {witness!r}: {output!r}"
            )
    return output


def replay_strict_interface_matrix(
    java: str, jar: Path, cwd: Path, persistent: PersistentWorker
) -> None:
    write_strict_sources(cwd)
    leaf_sig = strict_signature("leaf")
    y_sig = strict_signature("y")
    private_sig = strict_signature("private")
    wide_sig = strict_signature("wide")

    for name, output, sources, interfaces in (
        ("strict-build-leaf", f"{STRICT_OUTPUTS}/leaf", [STRICT_LEAF], []),
        ("strict-build-y", f"{STRICT_OUTPUTS}/y", [STRICT_Y], [leaf_sig]),
        ("strict-build-private", f"{STRICT_OUTPUTS}/private", [STRICT_PRIVATE], []),
        (
            "strict-build-wide",
            f"{STRICT_OUTPUTS}/wide",
            [STRICT_X, STRICT_SIBLING],
            [y_sig, leaf_sig, private_sig],
        ),
    ):
        strict_prepare(java, jar, cwd, name, output, sources, interfaces)

    # Selecting only Sibling from the wide signature must not pull in X's exposure.
    (cwd / STRICT_CLIENT).write_text(STRICT_CLIENT_SIBLING_SOURCE, encoding="utf-8")
    sibling_only = strict_compare(
        java,
        jar,
        cwd,
        persistent,
        "strict-sibling-only",
        f"{STRICT_OUTPUTS}/sibling-only",
        [STRICT_CLIENT],
        [wide_sig],
        request_id=210,
    )
    if sibling_only["exitCode"] != 0:
        raise AssertionError(f"Sibling-only selection failed: {sibling_only!r}")

    write_strict_labels(
        cwd,
        [(wide_sig, "//interfaces:wide"), (y_sig, "//interfaces:y")],
    )
    unused_y = strict_compare(
        java,
        jar,
        cwd,
        persistent,
        "strict-unused-y-label",
        f"{STRICT_OUTPUTS}/sibling-unused-y",
        [STRICT_CLIENT],
        [wide_sig, y_sig],
        labels=STRICT_LABELS,
        expect_success=False,
        request_id=211,
    )
    assert_strict_output(unused_y, f"{y_sig} (//interfaces:y)")

    (cwd / STRICT_CLIENT).write_text(STRICT_CLIENT_X_SOURCE, encoding="utf-8")
    missing_y = strict_compare(
        java,
        jar,
        cwd,
        persistent,
        "strict-missing-y",
        f"{STRICT_OUTPUTS}/missing-y",
        [STRICT_CLIENT],
        [wide_sig],
        expect_success=False,
        request_id=212,
    )
    assert_strict_output(missing_y, "Client -> X -> Y")

    missing_leaf = strict_compare(
        java,
        jar,
        cwd,
        persistent,
        "strict-missing-leaf-labels",
        f"{STRICT_OUTPUTS}/missing-leaf",
        [STRICT_CLIENT],
        [wide_sig, y_sig],
        labels=STRICT_LABELS,
        expect_success=False,
        request_id=213,
    )
    assert_strict_output(
        missing_leaf,
        "Client -> X -> Y -> Leaf",
        f"{wide_sig} (//interfaces:wide)",
        f"{y_sig} (//interfaces:y)",
    )

    exact = strict_compare(
        java,
        jar,
        cwd,
        persistent,
        "strict-exact-exposure-closure",
        f"{STRICT_OUTPUTS}/exact-exposure",
        [STRICT_CLIENT],
        [leaf_sig, y_sig, wide_sig, wide_sig],
        request_id=214,
    )
    if exact["exitCode"] != 0:
        raise AssertionError(f"exact exposure closure failed: {exact!r}")

    unnecessary_private = strict_compare(
        java,
        jar,
        cwd,
        persistent,
        "strict-unnecessary-private",
        f"{STRICT_OUTPUTS}/unnecessary-private",
        [STRICT_CLIENT],
        [wide_sig, leaf_sig, private_sig, y_sig],
        expect_success=False,
        request_id=215,
    )
    assert_strict_output(unnecessary_private, f"unused direct interfaces: {private_sig}")

    # Reversing declarations and labels changes neither diagnostics' meaning nor outputs.
    ordered_interfaces = [wide_sig, y_sig, leaf_sig]
    write_strict_labels(
        cwd,
        [(path, f"//first:{index}") for index, path in enumerate(ordered_interfaces)],
    )
    ordered = strict_compare(
        java,
        jar,
        cwd,
        persistent,
        "strict-label-order-first",
        f"{STRICT_OUTPUTS}/label-order",
        [STRICT_CLIENT],
        ordered_interfaces,
        labels=STRICT_LABELS,
        request_id=216,
    )
    if ordered["exitCode"] != 0:
        raise AssertionError(f"labelled exact closure failed: {ordered!r}")
    ordered_bytes = output_bytes(
        cwd, [f"{STRICT_OUTPUTS}/label-order.pkg", f"{STRICT_OUTPUTS}/label-order.sig"]
    )

    reversed_interfaces = list(reversed(ordered_interfaces))
    write_strict_labels(
        cwd,
        [(path, f"@second//:{index}") for index, path in enumerate(reversed_interfaces)],
    )
    reordered = strict_compare(
        java,
        jar,
        cwd,
        persistent,
        "strict-label-order-reversed",
        f"{STRICT_OUTPUTS}/label-order",
        [STRICT_CLIENT],
        reversed_interfaces,
        labels=STRICT_LABELS,
        request_id=217,
    )
    if reordered["exitCode"] != 0:
        raise AssertionError(f"reordered labelled closure failed: {reordered!r}")
    if output_bytes(
        cwd, [f"{STRICT_OUTPUTS}/label-order.pkg", f"{STRICT_OUTPUTS}/label-order.sig"]
    ) != ordered_bytes:
        raise AssertionError("declaration order changed successful package/signature bytes")

    write_strict_labels(
        cwd,
        [(wide_sig, "//app:facade"), (y_sig, "//types:owner")],
    )
    labelled_missing = strict_compare(
        java,
        jar,
        cwd,
        persistent,
        "strict-labelled-missing-leaf",
        f"{STRICT_OUTPUTS}/label-order",
        [STRICT_CLIENT],
        [wide_sig, y_sig],
        labels=STRICT_LABELS,
        expect_success=False,
        request_id=218,
    )
    assert_strict_output(
        labelled_missing,
        "Client -> X -> Y -> Leaf",
        f"{wide_sig} (//app:facade)",
        f"{y_sig} (//types:owner)",
    )

    group_local = strict_compare(
        java,
        jar,
        cwd,
        persistent,
        "strict-group-local-owner",
        f"{STRICT_OUTPUTS}/group-local",
        [STRICT_CLIENT, STRICT_Y],
        [wide_sig, leaf_sig],
        request_id=219,
    )
    if group_local["exitCode"] != 0:
        raise AssertionError(f"group-local Y did not satisfy X's exposure: {group_local!r}")
    redundant_y = strict_compare(
        java,
        jar,
        cwd,
        persistent,
        "strict-group-local-redundant-y",
        f"{STRICT_OUTPUTS}/group-local",
        [STRICT_CLIENT, STRICT_Y],
        [wide_sig, leaf_sig, y_sig],
        expect_success=False,
        request_id=220,
    )
    assert_strict_output(redundant_y, f"unused direct interfaces: {y_sig}")

    # Both wrappers expose the same package through different private providers.
    (cwd / STRICT_LEFT).write_text(strict_facade_source("Left"), encoding="utf-8")
    (cwd / STRICT_RIGHT).write_text(strict_facade_source("Right"), encoding="utf-8")
    for name, output, sources, interfaces in (
        ("strict-build-one", f"{STRICT_OUTPUTS}/one", [STRICT_ONE], []),
        ("strict-build-two", f"{STRICT_OUTPUTS}/two", [STRICT_TWO], []),
        ("strict-build-left", f"{STRICT_OUTPUTS}/left", [STRICT_LEFT], [strict_signature("one")]),
        ("strict-build-right", f"{STRICT_OUTPUTS}/right", [STRICT_RIGHT], [strict_signature("two")]),
    ):
        strict_prepare(java, jar, cwd, name, output, sources, interfaces)

    one_sig = strict_signature("one")
    two_sig = strict_signature("two")
    left_sig = strict_signature("left")
    right_sig = strict_signature("right")
    (cwd / STRICT_APP).write_text(STRICT_APP_LEFT_SOURCE, encoding="utf-8")
    baseline = strict_compare(
        java,
        jar,
        cwd,
        persistent,
        "strict-ambiguity-baseline",
        f"{STRICT_OUTPUTS}/ambiguity-app",
        [STRICT_APP],
        [one_sig, left_sig],
        request_id=234,
    )
    if baseline["exitCode"] != 0:
        raise AssertionError(f"single provider did not compile: {baseline!r}")

    provider_interfaces = [left_sig, right_sig, one_sig, two_sig]
    write_strict_labels(
        cwd,
        [
            (left_sig, "//:left"),
            (right_sig, "//:right"),
            (one_sig, "@shared_v1//:lib"),
            (two_sig, "@shared_v2//:lib"),
        ],
    )
    (cwd / STRICT_APP).write_text(STRICT_APP_BOTH_SOURCE, encoding="utf-8")
    for index, interfaces in enumerate((provider_interfaces, list(reversed(provider_interfaces)))):
        ambiguous = strict_compare(
            java,
            jar,
            cwd,
            persistent,
            f"strict-exposed-provider-ambiguity-{index}",
            f"{STRICT_OUTPUTS}/ambiguity-app",
            [STRICT_APP],
            interfaces,
            labels=STRICT_LABELS,
            expect_success=False,
            request_id=235 + index,
        )
        assert_strict_output(
            ambiguous,
            "ambiguous interface App -> ",
            " -> Shared/Api",
            f"{one_sig} (@shared_v1//:lib), {two_sig} (@shared_v2//:lib)",
        )

    # The same wide.sig path changes exposure while the worker stays warm.
    (cwd / STRICT_X).write_text(STRICT_X_SOURCE, encoding="utf-8")
    (cwd / STRICT_CLIENT).write_text(STRICT_CLIENT_X_SOURCE, encoding="utf-8")
    reused_output = f"{STRICT_OUTPUTS}/reused-client"
    warm_success = strict_compare(
        java,
        jar,
        cwd,
        persistent,
        "strict-reused-exposure-before-edit",
        reused_output,
        [STRICT_CLIENT],
        [wide_sig, y_sig, leaf_sig],
        request_id=240,
    )
    if warm_success["exitCode"] != 0:
        raise AssertionError(f"initial reused-path client compile failed: {warm_success!r}")
    old_client_outputs = output_bytes(cwd, [f"{reused_output}.pkg", f"{reused_output}.sig"])

    (cwd / STRICT_X).write_text(STRICT_X_REDUCED_SOURCE, encoding="utf-8")
    rebuilt = strict_compare(
        java,
        jar,
        cwd,
        persistent,
        "strict-rebuild-wide-without-exposure",
        f"{STRICT_OUTPUTS}/wide",
        [STRICT_X, STRICT_SIBLING],
        [],
        request_id=241,
    )
    if rebuilt["exitCode"] != 0:
        raise AssertionError(f"wide signature rebuild failed: {rebuilt!r}")

    stale_declarations = strict_compare(
        java,
        jar,
        cwd,
        persistent,
        "strict-reused-exposure-stale-declarations",
        reused_output,
        [STRICT_CLIENT],
        [wide_sig, y_sig, leaf_sig],
        expect_success=False,
        request_id=242,
    )
    assert_strict_output(stale_declarations, f"unused direct interfaces: {leaf_sig}, {y_sig}")
    if output_bytes(cwd, [f"{reused_output}.pkg", f"{reused_output}.sig"]) != old_client_outputs:
        raise AssertionError("failed reused-path compile changed either prior client output")

    recovered = strict_compare(
        java,
        jar,
        cwd,
        persistent,
        "strict-reused-exposure-recovery",
        reused_output,
        [STRICT_CLIENT],
        [wide_sig],
        request_id=243,
    )
    if recovered["exitCode"] != 0:
        raise AssertionError(f"reused-path client did not recover with corrected dependencies: {recovered!r}")


def check_strict_interface_matrix(java: str, jar: Path, work: Path) -> None:
    cwd = work / "strict interface workspace"
    cwd.mkdir()
    for label in ("warm", "restarted"):
        with PersistentWorker(java, jar, cwd) as persistent:
            replay_strict_interface_matrix(java, jar, cwd, persistent)
            status, stdout, stderr = persistent.finish()
            if status != 0 or stdout or stderr:
                raise AssertionError(
                    f"{label} strict-interface worker did not finish cleanly: "
                    f"status={status}, stdout={stdout!r}, stderr={stderr!r}"
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


def compile_command() -> list[str]:
    return ["compile-packages", "--src", SOURCE, "--packages-out", BUNDLE, "--sig-out", SIGNATURE]


def check_registry_rejections(
    java: str, jar: Path, cwd: Path, persistent: PersistentWorker
) -> None:
    for index, operation in enumerate(("check", "fetch", "transpile", "publish-artifacts")):
        params = cwd / "parameter files 雪" / f"reject-{operation}.params"
        params.parent.mkdir(parents=True, exist_ok=True)
        params.write_text(operation + "\n", encoding="utf-8")
        before = workspace_snapshot(cwd)
        one_shot = invoke(
            java,
            jar,
            cwd,
            "tool",
            "bazel-worker",
            "@" + params.relative_to(cwd).as_posix(),
        )
        expected = f"unsupported bazel worker operation: {operation}\n"
        if one_shot.returncode == 0 or one_shot.stdout or one_shot.stderr.decode("utf-8") != expected:
            raise AssertionError(f"one-shot registry rejected {operation} unexpectedly: {one_shot!r}")
        response = persistent.request({"arguments": [operation], "requestId": 100 + index})
        if response["exitCode"] != one_shot.returncode or response["output"] != expected:
            raise AssertionError(f"persistent registry disagrees for {operation}: {response!r}")
        if workspace_snapshot(cwd) != before:
            raise AssertionError(f"registry rejection for {operation} changed workspace files")
        assert_no_scratch(cwd, f"registry rejection for {operation}")

    empty = persistent.request({"arguments": [], "requestId": 104})
    if empty["exitCode"] == 0 or empty["output"] != "expected a bazel worker operation\n":
        raise AssertionError(f"persistent worker accepted an empty command: {empty!r}")


def test_worker(java: str, jar: Path, work: Path) -> None:
    cwd = work / "candidate workspace"
    cwd.mkdir()
    write_fixture(cwd)
    (cwd / "generated files 雪").mkdir()

    with PersistentWorker(java, jar, cwd) as persistent:
        header_command = ["headers", "--src", SOURCE, "--out", "generated files 雪/headers.json"]
        headers_result = compare_command(
            java,
            jar,
            cwd,
            persistent,
            "headers",
            header_command,
            ["generated files 雪/headers.json"],
            request_fields={"unknown_worker_field": {"ignored": True}},
        )
        if headers_result["output"] != "":
            raise AssertionError("successful headers request returned action diagnostics")

        digest_cases = (
            ("missing-input-digest", [{"path": SOURCE}]),
            ("empty-input-digest", [{"path": SOURCE, "digest": ""}]),
            (
                "repeated-opaque-digests",
                [
                    {"path": SOURCE, "digest": "AQID"},
                    {"path": SOURCE, "digest": "AQID"},
                ],
            ),
        )
        for name, inputs in digest_cases:
            compare_command(
                java,
                jar,
                cwd,
                persistent,
                name,
                ["headers", "--src", SOURCE, "--out", f"generated files 雪/{name}.json"],
                [f"generated files 雪/{name}.json"],
                request_fields={"inputs": inputs, "requestId": 0},
            )

        (cwd / SOURCE).write_text(
            'package Demo/Suite\nexport value\nvalue = 1\nchecks = Assertion(True, "candidate 雪")\n',
            encoding="utf-8",
        )
        compile_args = compile_command()
        initial_compile = compare_command(
            java,
            jar,
            cwd,
            persistent,
            "compile-initial",
            compile_args,
            [BUNDLE, SIGNATURE],
            line_ending="\r\n",
            request_fields={"requestId": 0, "inputs": [{"path": SOURCE, "digest": "AQID"}]},
        )
        if initial_compile["exitCode"] != 0:
            raise AssertionError("initial persistent compilation failed")
        initial_bundle = (cwd / BUNDLE).read_bytes()
        initial_signature = (cwd / SIGNATURE).read_bytes()

        (cwd / SOURCE).write_text(
            'package Demo/Suite\nexport value\nvalue = 2\nchecks = Assertion(True, "candidate 雪")\n',
            encoding="utf-8",
        )
        compare_command(
            java,
            jar,
            cwd,
            persistent,
            "compile-body-edit",
            compile_args,
            [BUNDLE, SIGNATURE],
            request_fields={
                "requestId": 7,
                "inputs": [{"path": SOURCE, "digest": "AQID"}, {"path": SOURCE, "digest": "AQID"}],
            },
        )
        body_bundle = (cwd / BUNDLE).read_bytes()
        if body_bundle == initial_bundle:
            raise AssertionError("same-path body edit reused stale compiled package bytes")
        if (cwd / SIGNATURE).read_bytes() != initial_signature:
            raise AssertionError("private body edit unexpectedly changed exported signature bytes")

        (cwd / SOURCE).write_text(
            'package Demo/Suite\nexport value\nvalue = missing_name\nchecks = Assertion(True, "candidate 雪")\n',
            encoding="utf-8",
        )
        bad_type = compare_command(
            java,
            jar,
            cwd,
            persistent,
            "compile-type-error",
            compile_args,
            [BUNDLE, SIGNATURE],
            expect_success=False,
            request_fields={"requestId": 8, "inputs": [{"path": SOURCE, "digest": "AQID"}], "verbosity": 1},
        )
        if "unknown name" not in str(bad_type["output"]).lower() or "\n" not in str(bad_type["output"]):
            raise AssertionError(f"type error did not retain its multiline diagnostic: {bad_type!r}")
        if (cwd / BUNDLE).read_bytes() != body_bundle:
            raise AssertionError("failed same-path compile published stale or partial package bytes")

        (cwd / SOURCE).write_text(
            'package Demo/Suite\nexport value\nvalue = "updated type"\nchecks = Assertion(True, "candidate 雪")\n',
            encoding="utf-8",
        )
        compare_command(
            java,
            jar,
            cwd,
            persistent,
            "compile-type-edit-recovery",
            compile_args,
            [BUNDLE, SIGNATURE],
            request_fields={"requestId": 8, "inputs": [{"path": SOURCE, "digest": "AQID"}]},
        )
        type_bundle = (cwd / BUNDLE).read_bytes()
        type_signature = (cwd / SIGNATURE).read_bytes()
        if type_bundle == body_bundle or type_signature == initial_signature:
            raise AssertionError("same-path type edit did not recompute implementation and signature bytes")

        literal_source = "@literal source 雪.bosatsu"
        (cwd / literal_source).write_text(
            "package Demo/Literal\nexport value\nvalue = 9\n", encoding="utf-8"
        )
        compare_command(
            java,
            jar,
            cwd,
            persistent,
            "literal-at-argument",
            ["headers", "--src", literal_source, "--out", "generated files 雪/literal.json"],
            ["generated files 雪/literal.json"],
            request_fields={"requestId": 21},
        )

        compare_command(
            java,
            jar,
            cwd,
            persistent,
            "validate-graph",
            ["validate-graph", "--root", ROOT_GRAPH, "--out", "generated files 雪/validated stamp"],
            ["generated files 雪/validated stamp"],
            final_newline=False,
            request_fields={"requestId": 29},
        )
        compare_command(
            java,
            jar,
            cwd,
            persistent,
            "emit-c",
            ["emit-c", "--runtime-manifest", RUNTIME, "--test-packages", BUNDLE, "--out", "generated files 雪/test suite.c"],
            ["generated files 雪/test suite.c"],
            line_ending="\r\n",
            request_fields={"requestId": 30},
        )

        compare_command(
            java,
            jar,
            cwd,
            persistent,
            "missing-source",
            ["headers", "--src", "missing input 雪.bosatsu", "--out", "generated files 雪/missing.json"],
            ["generated files 雪/missing.json"],
            expect_success=False,
            request_fields={"requestId": 31},
        )
        compare_command(
            java,
            jar,
            cwd,
            persistent,
            "missing-graph",
            ["validate-graph", "--root", "missing graph 雪.json", "--out", "generated files 雪/missing stamp"],
            ["generated files 雪/missing stamp"],
            expect_success=False,
            request_fields={"requestId": 32},
        )
        compare_command(
            java,
            jar,
            cwd,
            persistent,
            "missing-runtime",
            ["emit-c", "--runtime-manifest", "missing runtime 雪.json", "--test-packages", BUNDLE, "--out", "generated files 雪/missing.c"],
            ["generated files 雪/missing.c"],
            expect_success=False,
            request_fields={"requestId": 33},
        )
        compare_command(
            java,
            jar,
            cwd,
            persistent,
            "empty-argument",
            ["headers", "--src", "", "--out", "generated files 雪/empty-argument.json"],
            ["generated files 雪/empty-argument.json"],
            expect_success=False,
            request_fields={"requestId": 34},
        )

        for index, option in enumerate(("--cache_dir", "--cache-dir", "--no-cache")):
            compare_command(
                java,
                jar,
                cwd,
                persistent,
                f"invalid-inference-option-{index}",
                compile_args + [option, "cache"],
                [BUNDLE, SIGNATURE],
                expect_success=False,
                request_fields={"requestId": 40 + index},
            )

        check_registry_rejections(java, jar, cwd, persistent)

        before_sandbox = workspace_snapshot(cwd)
        sandbox = persistent.request(
            {
                "arguments": ["headers", "--src", SOURCE, "--out", "generated files 雪/sandbox.json"],
                "requestId": 50,
                "sandboxDir": "unsupported sandbox 雪",
            }
        )
        if sandbox["exitCode"] == 0 or "unsupported nonempty sandboxDir" not in str(sandbox["output"]):
            raise AssertionError(f"persistent worker did not reject sandboxDir: {sandbox!r}")
        if workspace_snapshot(cwd) != before_sandbox:
            raise AssertionError("sandboxDir rejection changed the workspace")
        assert_no_scratch(cwd, "sandboxDir rejection")

        compare_command(
            java,
            jar,
            cwd,
            persistent,
            "ignored-cancel-before-action",
            ["headers", "--src", SOURCE, "--out", "generated files 雪/after-cancel.json"],
            ["generated files 雪/after-cancel.json"],
            request_fields={"requestId": 52},
            control_request={"requestId": 51, "cancel": True},
        )

        previous_signature = b"previous published signature"
        (cwd / SIGNATURE).write_bytes(previous_signature)
        (cwd / BUNDLE).unlink()
        (cwd / BUNDLE).mkdir()
        failed_publication = compare_command(
            java,
            jar,
            cwd,
            persistent,
            "multi-output-publication-failure",
            compile_args,
            [BUNDLE, SIGNATURE],
            expect_success=False,
            compare_diagnostics=False,
            request_fields={"requestId": 60},
        )
        if "expected a regular output file" not in str(failed_publication["output"]):
            raise AssertionError(f"publication failure lacks its useful diagnostic: {failed_publication!r}")
        if (cwd / SIGNATURE).read_bytes() != previous_signature or not (cwd / BUNDLE).is_dir():
            raise AssertionError("publication failure did not preserve prior signature and obstruction")

        (cwd / BUNDLE).rmdir()
        compare_command(
            java,
            jar,
            cwd,
            persistent,
            "multi-output-publication-retry",
            compile_args,
            [BUNDLE, SIGNATURE],
            request_fields={"requestId": 61},
        )
        check_startup_failures(java, jar, cwd)
        persistent_status, extra_stdout, stderr = persistent.finish()
        if persistent_status != 0 or extra_stdout or stderr:
            raise AssertionError(
                "persistent worker did not finish cleanly after EOF: "
                f"status={persistent_status}, stdout={extra_stdout!r}, stderr={stderr!r}"
            )

    check_transport_failures(java, jar, work)
    check_strict_interface_matrix(java, jar, work)


def check_transport_failures(java: str, jar: Path, work: Path) -> None:
    cwd = work / "transport workspace"
    cwd.mkdir()
    write_fixture(cwd)
    (cwd / "generated files 雪").mkdir()

    with PersistentWorker(java, jar, cwd) as clean:
        status, stdout, stderr = clean.finish()
        if status != 0 or stdout or stderr:
            raise AssertionError(
                f"clean EOF failed: status={status}, stdout={stdout!r}, stderr={stderr!r}"
            )

    failures = (
        ("malformed JSON", b'{"arguments":[}\n', "invalid worker JSON"),
        ("truncated frame", b'{"arguments":["headers"]}', "truncated worker request"),
        ("invalid UTF-8", b"\xc3(\n", "not valid UTF-8"),
    )
    for label, request, expected in failures:
        before = workspace_snapshot(cwd)
        with PersistentWorker(java, jar, cwd) as broken:
            broken.raw(request)
            status, stdout, stderr = broken.finish()
        message = stderr.decode("utf-8", errors="strict")
        if status == 0 or stdout or "bazel worker transport error" not in message or expected not in message:
            raise AssertionError(
                f"{label} did not retire the worker cleanly: "
                f"status={status}, stdout={stdout!r}, stderr={message!r}"
            )
        if workspace_snapshot(cwd) != before:
            raise AssertionError(f"{label} changed workspace files")
        assert_no_scratch(cwd, label)

    with PersistentWorker(java, jar, cwd) as restarted:
        compare_command(
            java,
            jar,
            cwd,
            restarted,
            "request-after-restart",
            ["headers", "--src", SOURCE, "--out", "generated files 雪/restarted.json"],
            ["generated files 雪/restarted.json"],
            request_fields={"requestId": 71},
        )
        status, stdout, stderr = restarted.finish()
        if status != 0 or stdout or stderr:
            raise AssertionError(
                f"restarted worker did not finish cleanly: status={status}, stdout={stdout!r}, stderr={stderr!r}"
            )


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
        test_worker(java, jar, work)
        test_bosatsuj(java, jar, work)
    print("candidate jar Bazel worker acceptance passed")


if __name__ == "__main__":
    main()
