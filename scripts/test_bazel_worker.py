#!/usr/bin/env python3
"""Compare direct, one-shot, and persistent JVM file-command invocations."""
import argparse
from collections import deque
import hashlib
import io
import json
import os
from pathlib import Path
import select
import shutil
import subprocess
import tarfile
import tempfile
import time
import zipfile


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


LEGACY_CORE = "core_alpha-v9.0.1.bosatsu_lib"
LEGACY_CORE_SIZE = 937080
LEGACY_CORE_SHA256 = "b6e7d87fcb786084dd90da490fe9514678fee7f6920fc17e0abdf28210a11dff"
LEGACY_CORE_BLAKE3 = "098f5eff00611599b54b5e6bce8b7696c8fdf89f4e99f8bfbbec521764ed8927"
INSPECTION_ROOT = "test_workspace/inspect_library/parser-v1.0.0.bosatsu_lib"
INSPECTION_CONTEXT = "test_workspace/inspect_library/parser-context.json"
INSPECTION_OUTPUTS = "inspection outputs 雪"


def inspection_command(
    input_path: str,
    name: str,
    version: str,
    hashes: list[str],
    *,
    descriptors_only: bool = False,
    dependency_context: str | None = None,
    outputs: dict[str, str] | None = None,
) -> list[str]:
    command = [
        "inspect-library",
        "--input",
        input_path,
        "--expected-name",
        name,
        "--expected-version",
        version,
    ]
    for digest in hashes:
        command.extend(["--expected-hash", digest])
    if descriptors_only:
        command.append("--descriptors-only")
    if dependency_context is not None:
        command.extend(["--dependency-context", dependency_context])
    for flag, path in (outputs or {}).items():
        command.extend([flag, path])
    return command


def copy_inspection_fixtures(prepared: Path, cwd: Path) -> None:
    destination = cwd / "test_workspace/inspect_library"
    shutil.copytree(prepared, destination, dirs_exist_ok=True)


def inspection_files(prepared: Path) -> tuple[dict[str, object], dict[str, object]]:
    if not prepared.is_dir():
        raise AssertionError(
            f"prepared inspection fixtures are missing: {prepared}; run "
            "python3 scripts/prepare_inspect_library_fixtures.py --out-dir <directory>"
        )
    root_pin_path = prepared / "parser-root.json"
    context_path = prepared / "parser-context.json"
    legacy_path = prepared / LEGACY_CORE
    required = [root_pin_path, context_path, legacy_path]
    missing = [path.name for path in required if not path.is_file()]
    if missing:
        raise AssertionError(
            f"prepared inspection fixtures at {prepared} are incomplete: {missing!r}; "
            "run scripts/prepare_inspect_library_fixtures.py"
        )
    root_pin = json.loads(root_pin_path.read_text(encoding="utf-8"))
    context = json.loads(context_path.read_text(encoding="utf-8"))
    legacy_bytes = legacy_path.read_bytes()
    actual_sha256 = sha256(legacy_bytes)
    if len(legacy_bytes) != LEGACY_CORE_SIZE or actual_sha256 != LEGACY_CORE_SHA256:
        raise AssertionError(
            "prepared legacy core does not match its immutable pin: "
            f"size={len(legacy_bytes)}, sha256={actual_sha256}"
        )
    if root_pin.get("schema_version") != 1 or len(root_pin.get("libraries", [])) != 1:
        raise AssertionError(f"invalid parser root pin file: {root_pin!r}")
    if context.get("schema_version") != 1 or not context.get("libraries"):
        raise AssertionError(f"invalid parser dependency context: {context!r}")
    return root_pin["libraries"][0], context


def _inspection_library_path(cwd: Path, relative: str) -> Path:
    return cwd / relative


def test_library_inspection(
    java: str, candidate_jar: Path, work: Path, prepared: Path
) -> None:
    root, context = inspection_files(prepared)
    fixture_by_identity = {
        (entry["name"], entry["version"]): entry for entry in context["libraries"]
    }
    if len(fixture_by_identity) != len(context["libraries"]):
        raise AssertionError("inspection fixture context repeats a library identity")

    # Descriptor discovery is a resolver loop: each invocation gets exactly
    # the selected child's bytes plus the authenticated pins from its parent.
    resolver_cwd = work / "inspection descriptor resolver workspace"
    resolver_cwd.mkdir()
    resolver_jar = Path("candidate compiler 雪.jar")
    shutil.copyfile(candidate_jar, resolver_cwd / resolver_jar)
    resolver_input_dir = resolver_cwd / "test_workspace/inspect_library"
    resolver_input_dir.mkdir(parents=True)
    resolver_output_dir = resolver_cwd / INSPECTION_OUTPUTS
    resolver_output_dir.mkdir()
    root_source = prepared / Path(root["path"]).name
    shutil.copyfile(root_source, resolver_cwd / INSPECTION_ROOT)
    with PersistentWorker(java, resolver_jar, resolver_cwd) as persistent:
        root_descriptor_path = f"{INSPECTION_OUTPUTS}/parser-descriptor.json"
        root_command = inspection_command(
            INSPECTION_ROOT,
            root["name"],
            root["version"],
            root["hashes"],
            descriptors_only=True,
            outputs={"--descriptor-out": root_descriptor_path},
        )
        compare_command(
            java,
            resolver_jar,
            resolver_cwd,
            persistent,
            "inspect-parser-root-descriptor",
            root_command,
            [root_descriptor_path],
            request_fields={"requestId": 300},
        )
        root_descriptor = json.loads(
            (resolver_cwd / root_descriptor_path).read_text(encoding="utf-8")
        )
        if root_descriptor["identity"] != {"name": "parser", "version": "1.0.0"}:
            raise AssertionError(f"unexpected Parser descriptor identity: {root_descriptor!r}")
        (resolver_cwd / INSPECTION_ROOT).unlink()

        pending = deque(
            ("parser", "1.0.0", dependency)
            for dependency in root_descriptor["dependencies"]
        )
        expanded = {("parser", "1.0.0")}
        verified_edges = 0
        common_from_public: list[str] | None = None
        request_id = 301
        while pending:
            parent_name, parent_version, edge = pending.popleft()
            identity = edge["identity"]
            key = (identity["name"], identity["version"])
            fixture = fixture_by_identity.get(key)
            if fixture is None:
                raise AssertionError(f"no local fixture for authenticated edge to {key!r}")
            if not all(value.startswith("blake3:") for value in edge["hashes"]):
                raise AssertionError(f"fixture edge did not retain its BLAKE3-only pin: {edge!r}")

            relative_input = fixture["path"]
            selected_file = prepared / Path(relative_input).name
            local_file = _inspection_library_path(resolver_cwd, relative_input)
            local_file.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(selected_file, local_file)
            if len(list(resolver_cwd.glob("test_workspace/inspect_library/*.bosatsu_lib"))) != 1:
                raise AssertionError("descriptor discovery exposed more than the selected child file")

            output = f"{INSPECTION_OUTPUTS}/descriptor-{request_id}.json"
            command = inspection_command(
                relative_input,
                key[0],
                key[1],
                edge["hashes"],
                descriptors_only=True,
                outputs={"--descriptor-out": output},
            )
            compare_command(
                java,
                resolver_jar,
                resolver_cwd,
                persistent,
                f"inspect-child-{request_id}",
                command,
                [output],
                request_fields={"requestId": request_id},
            )
            document = json.loads((resolver_cwd / output).read_text(encoding="utf-8"))
            child_bytes = selected_file.read_bytes()
            actual_sha256 = sha256(child_bytes)
            if document["identity"] != identity or document["digests"]["sha256"] != actual_sha256:
                raise AssertionError(
                    f"child metadata was not derived from its authenticated bytes: {document!r}"
                )
            verified_edges += 1
            if parent_name == "public_child" and key[0] == "common":
                common_from_public = list(edge["hashes"])
            if key not in expanded:
                expanded.add(key)
                for child_edge in document["dependencies"]:
                    pending.append((key[0], key[1], child_edge))
            local_file.unlink()
            request_id += 1

        if verified_edges != 11 or len(expanded) != 9:
            raise AssertionError(
                f"Parser closure was not traversed edge by edge: "
                f"edges={verified_edges}, identities={len(expanded)}"
            )
        if common_from_public is None:
            raise AssertionError("the public side of the common diamond edge was not traversed")

    # A bad child pin must fail before any descriptor metadata replaces prior output.
    tamper_cwd = work / "inspection tamper workspace"
    tamper_cwd.mkdir()
    tamper_jar = Path("candidate compiler 雪.jar")
    shutil.copyfile(candidate_jar, tamper_cwd / tamper_jar)
    tamper_input = "test_workspace/inspect_library/common-v1.0.0.bosatsu_lib"
    tamper_file = tamper_cwd / tamper_input
    tamper_file.parent.mkdir(parents=True)
    tampered_common = (prepared / Path(fixture_by_identity[("common", "1.0.0")]["path"]).name).read_bytes() + b"\x00"
    tamper_file.write_bytes(tampered_common)
    tamper_output = f"{INSPECTION_OUTPUTS}/common-descriptor.json"
    (tamper_cwd / INSPECTION_OUTPUTS).mkdir()
    (tamper_cwd / tamper_output).write_bytes(b"previous descriptor\n")
    with PersistentWorker(java, tamper_jar, tamper_cwd) as persistent:
        failed = compare_command(
            java,
            tamper_jar,
            tamper_cwd,
            persistent,
            "inspect-tampered-child-before-descriptors",
            inspection_command(
                tamper_input,
                "common",
                "1.0.0",
                common_from_public,
                descriptors_only=True,
                outputs={"--descriptor-out": tamper_output},
            ),
            [tamper_output],
            expect_success=False,
            request_fields={"requestId": 340},
        )
        if "pin mismatch" not in str(failed["output"]):
            raise AssertionError(f"tampered child diagnostic omitted its pin failure: {failed!r}")

    # Full inspection consumes the complete declared graph and publishes the
    # independent signature, exposure, membership, implementation and root projections.
    full_cwd = work / "inspection full workspace"
    full_cwd.mkdir()
    full_jar = Path("candidate compiler 雪.jar")
    shutil.copyfile(candidate_jar, full_cwd / full_jar)
    copy_inspection_fixtures(prepared, full_cwd)
    output_paths = {
        "--sig-out": f"{INSPECTION_OUTPUTS}/parser.sig",
        "--exposures-out": f"{INSPECTION_OUTPUTS}/exposures.json",
        "--membership-out": f"{INSPECTION_OUTPUTS}/membership.json",
        "--implementation-out": f"{INSPECTION_OUTPUTS}/implementation.json",
        "--descriptor-out": f"{INSPECTION_OUTPUTS}/descriptor.json",
    }
    all_outputs = list(output_paths.values())
    output_dir = full_cwd / INSPECTION_OUTPUTS
    output_dir.mkdir()
    with PersistentWorker(java, full_jar, full_cwd) as persistent:
        command = inspection_command(
            INSPECTION_ROOT,
            root["name"],
            root["version"],
            root["hashes"],
            dependency_context=INSPECTION_CONTEXT,
            outputs=output_paths,
        )
        compare_command(
            java,
            full_jar,
            full_cwd,
            persistent,
            "inspect-parser-full-graph",
            command,
            all_outputs,
            request_fields={"requestId": 350},
        )
        signature = (full_cwd / output_paths["--sig-out"]).read_bytes()
        exposures = json.loads((full_cwd / output_paths["--exposures-out"]).read_text(encoding="utf-8"))
        membership = json.loads((full_cwd / output_paths["--membership-out"]).read_text(encoding="utf-8"))
        implementation = json.loads((full_cwd / output_paths["--implementation-out"]).read_text(encoding="utf-8"))
        if not signature or not exposures.get("packages") or not membership.get("libraries"):
            raise AssertionError("full Parser inspection omitted a requested projection")
        shared_versions = {
            (item["identity"]["name"], item["identity"]["version"])
            for item in implementation["artifacts"]
            if item["identity"]["name"] == "shared"
        }
        if shared_versions != {("shared", "1.0.0"), ("shared", "2.0.0")}:
            raise AssertionError(f"private Shared releases lost their scopes: {shared_versions!r}")
        if not any(
            edge["to"]["name"] == "shared" and edge["from"]["name"] in {"left", "right"}
            for edge in implementation["edges"]
        ):
            raise AssertionError("implementation projection omitted private-version referring edges")

        signature_only = f"{INSPECTION_OUTPUTS}/signature-only.sig"
        compare_command(
            java,
            full_jar,
            full_cwd,
            persistent,
            "inspect-parser-signature-only",
            inspection_command(
                INSPECTION_ROOT,
                root["name"],
                root["version"],
                root["hashes"],
                dependency_context=INSPECTION_CONTEXT,
                outputs={"--sig-out": signature_only},
            ),
            [signature_only],
            request_fields={"requestId": 351},
        )
        if (full_cwd / signature_only).read_bytes() != signature:
            raise AssertionError("signature-only inspection differs from the full signature projection")

        # Reordering declarations preserves every canonical projection byte.
        reverse_context = dict(context)
        reverse_context["libraries"] = list(reversed(context["libraries"]))
        reverse_context_path = "test_workspace/inspect_library/parser-context-reversed.json"
        (full_cwd / reverse_context_path).write_text(
            json.dumps(reverse_context, sort_keys=True), encoding="utf-8"
        )
        reverse_outputs = {
            flag: f"{INSPECTION_OUTPUTS}/reordered-{Path(path).name}"
            for flag, path in output_paths.items()
        }
        compare_command(
            java,
            full_jar,
            full_cwd,
            persistent,
            "inspect-parser-reordered-context",
            inspection_command(
                INSPECTION_ROOT,
                root["name"],
                root["version"],
                root["hashes"],
                dependency_context=reverse_context_path,
                outputs=reverse_outputs,
            ),
            list(reverse_outputs.values()),
            request_fields={"requestId": 352},
        )
        for original, reordered in zip(all_outputs, reverse_outputs.values()):
            if (full_cwd / original).read_bytes() != (full_cwd / reordered).read_bytes():
                raise AssertionError(f"reordered context changed projection bytes for {original}")

        # Equal identity aliases coalesce. Distinct bytes under one identity fail.
        common_entry = fixture_by_identity[("common", "1.0.0")]
        alias_path = "test_workspace/inspect_library/aliases/common-copy.bosatsu_lib"
        alias_file = full_cwd / alias_path
        alias_file.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(prepared / Path(common_entry["path"]).name, alias_file)
        alias_context = dict(context)
        alias_context["libraries"] = context["libraries"] + [
            dict(common_entry, path=alias_path)
        ]
        alias_context_path = "test_workspace/inspect_library/parser-context-alias.json"
        (full_cwd / alias_context_path).write_text(
            json.dumps(alias_context, sort_keys=True), encoding="utf-8"
        )
        alias_outputs = {
            flag: f"{INSPECTION_OUTPUTS}/alias-{Path(path).name}"
            for flag, path in output_paths.items()
        }
        compare_command(
            java,
            full_jar,
            full_cwd,
            persistent,
            "inspect-parser-equal-alias",
            inspection_command(
                INSPECTION_ROOT,
                root["name"],
                root["version"],
                root["hashes"],
                dependency_context=alias_context_path,
                outputs=alias_outputs,
            ),
            list(alias_outputs.values()),
            request_fields={"requestId": 353},
        )
        alias_implementation = json.loads(
            (full_cwd / alias_outputs["--implementation-out"]).read_text(encoding="utf-8")
        )
        common_artifact = next(
            item for item in alias_implementation["artifacts"]
            if item["identity"] == {"name": "common", "version": "1.0.0"}
        )
        if len(common_artifact["paths"]) != 2:
            raise AssertionError(f"equal identity aliases were not retained: {common_artifact!r}")

        conflicting_path = "test_workspace/inspect_library/aliases/common-conflict.bosatsu_lib"
        conflicting_file = full_cwd / conflicting_path
        conflicting_file.write_bytes(alias_file.read_bytes() + b"\xf0\x01\x01")
        conflicting_entry = dict(
            common_entry,
            path=conflicting_path,
            hashes=[f"sha256:{sha256(conflicting_file.read_bytes())}"],
        )
        conflicting_context = dict(context)
        conflicting_context["libraries"] = context["libraries"] + [conflicting_entry]
        conflicting_context_path = "test_workspace/inspect_library/parser-context-conflict.json"
        (full_cwd / conflicting_context_path).write_text(
            json.dumps(conflicting_context, sort_keys=True), encoding="utf-8"
        )
        conflict = compare_command(
            java,
            full_jar,
            full_cwd,
            persistent,
            "inspect-parser-conflicting-alias",
            inspection_command(
                INSPECTION_ROOT,
                root["name"],
                root["version"],
                root["hashes"],
                dependency_context=conflicting_context_path,
                outputs=output_paths,
            ),
            all_outputs,
            expect_success=False,
            request_fields={"requestId": 354},
        )
        if "conflicting files claim common" not in str(conflict["output"]):
            raise AssertionError(f"conflicting alias diagnostic was not useful: {conflict!r}")

        # Omitting a declared dependency must fail without replacing any of
        # the projections which the previous successful inspection published.
        missing_context = dict(context)
        missing_context["libraries"] = [
            entry for entry in context["libraries"]
            if (entry["name"], entry["version"]) != ("legacy", "1.0.0")
        ]
        missing_context_path = "test_workspace/inspect_library/parser-context-missing.json"
        (full_cwd / missing_context_path).write_text(
            json.dumps(missing_context, sort_keys=True), encoding="utf-8"
        )
        missing = compare_command(
            java,
            full_jar,
            full_cwd,
            persistent,
            "inspect-parser-incomplete-context",
            inspection_command(
                INSPECTION_ROOT,
                root["name"],
                root["version"],
                root["hashes"],
                dependency_context=missing_context_path,
                outputs=output_paths,
            ),
            all_outputs,
            expect_success=False,
            request_fields={"requestId": 355},
        )
        if "legacy" not in str(missing["output"]):
            raise AssertionError(f"incomplete graph diagnostic omitted its missing dependency: {missing!r}")

        # Reuse the same path across a successful request, a changed dependency
        # with stale pins, and a successful recovery request in this worker.
        common_file = full_cwd / common_entry["path"]
        original_common = common_file.read_bytes()
        common_file.write_bytes(original_common + b"\xf0\x01\x01")
        changed = compare_command(
            java,
            full_jar,
            full_cwd,
            persistent,
            "inspect-parser-reused-path-bad-dependency",
            command,
            all_outputs,
            expect_success=False,
            request_fields={"requestId": 356},
        )
        if "pin mismatch" not in str(changed["output"]):
            raise AssertionError(f"same-path dependency edit escaped authentication: {changed!r}")
        common_file.write_bytes(original_common)
        recovered = compare_command(
            java,
            full_jar,
            full_cwd,
            persistent,
            "inspect-parser-reused-path-recovery",
            command,
            all_outputs,
            request_fields={"requestId": 357},
        )
        if recovered["exitCode"] != 0:
            raise AssertionError(f"persistent inspection did not recover after restoring a pin: {recovered!r}")

        # The second incoming Common edge is bad even though the first one has
        # already authenticated the same identity. Rewrite only pinned strings
        # in the serialized artifacts so every enclosing artifact remains a
        # valid protobuf with the exact expected edge order.
        private_entry = fixture_by_identity[("private_child", "1.0.0")]
        private_path = full_cwd / private_entry["path"]
        original_private = private_path.read_bytes()
        original_common_pin = next(
            digest for digest in common_entry["hashes"] if digest.startswith("blake3:")
        ).encode("ascii")
        bad_common_pin = ("sha256:" + "0" * 64).encode("ascii")
        if original_private.count(original_common_pin) != 1:
            raise AssertionError("could not identify the private-child Common descriptor pin")
        bad_private = original_private.replace(original_common_pin, bad_common_pin, 1)
        private_path.write_bytes(bad_private)
        bad_private_descriptor = f"{INSPECTION_OUTPUTS}/bad-private-descriptor.json"
        private_probe = invoke(
            java,
            full_jar,
            full_cwd,
            "tool",
            *inspection_command(
                private_entry["path"],
                "private_child",
                "1.0.0",
                [f"sha256:{sha256(bad_private)}"],
                descriptors_only=True,
                outputs={"--descriptor-out": bad_private_descriptor},
            ),
        )
        if private_probe.returncode != 0:
            raise AssertionError(
                "candidate jar could not inspect the modified private child for its new root pin: "
                + private_probe.stderr.decode("utf-8", errors="replace")
            )
        bad_private_doc = json.loads(
            (full_cwd / bad_private_descriptor).read_text(encoding="utf-8")
        )
        old_private_pin = next(
            digest for digest in private_entry["hashes"] if digest.startswith("blake3:")
        ).encode("ascii")
        new_private_pin = f"blake3:{bad_private_doc['digests']['blake3']}".encode("ascii")
        root_file = full_cwd / INSPECTION_ROOT
        original_root_bytes = root_file.read_bytes()
        if original_root_bytes.count(old_private_pin) != 1:
            raise AssertionError("could not identify the Parser private-child descriptor pin")
        bad_root_bytes = original_root_bytes.replace(old_private_pin, new_private_pin, 1)
        root_file.write_bytes(bad_root_bytes)
        bad_root_descriptor = f"{INSPECTION_OUTPUTS}/bad-root-descriptor.json"
        root_probe = invoke(
            java,
            full_jar,
            full_cwd,
            "tool",
            *inspection_command(
                INSPECTION_ROOT,
                "parser",
                "1.0.0",
                [f"sha256:{sha256(bad_root_bytes)}"],
                descriptors_only=True,
                outputs={"--descriptor-out": bad_root_descriptor},
            ),
        )
        if root_probe.returncode != 0:
            raise AssertionError(
                "candidate jar could not inspect the modified Parser root for its new pin: "
                + root_probe.stderr.decode("utf-8", errors="replace")
            )
        bad_root_doc = json.loads((full_cwd / bad_root_descriptor).read_text(encoding="utf-8"))
        bad_context = dict(context)
        bad_context["libraries"] = [
            dict(
                entry,
                hashes=[
                    f"blake3:{bad_private_doc['digests']['blake3']}",
                    f"sha256:{sha256(bad_private)}",
                ],
            )
            if (entry["name"], entry["version"]) == ("private_child", "1.0.0")
            else entry
            for entry in context["libraries"]
        ]
        bad_context_path = "test_workspace/inspect_library/parser-context-bad-diamond.json"
        (full_cwd / bad_context_path).write_text(
            json.dumps(bad_context, sort_keys=True), encoding="utf-8"
        )
        bad_diamond = compare_command(
            java,
            full_jar,
            full_cwd,
            persistent,
            "inspect-parser-second-diamond-edge-bad-pin",
            inspection_command(
                INSPECTION_ROOT,
                "parser",
                "1.0.0",
                [
                    f"blake3:{bad_root_doc['digests']['blake3']}",
                    f"sha256:{sha256(bad_root_bytes)}",
                ],
                dependency_context=bad_context_path,
                outputs=output_paths,
            ),
            all_outputs,
            expect_success=False,
            request_fields={"requestId": 358},
        )
        if "sha256 pin mismatch" not in str(bad_diamond["output"]) or "private_child" not in str(bad_diamond["output"]):
            raise AssertionError(f"second diamond edge failure lost its witness: {bad_diamond!r}")

    # The released legacy core provides real old-format signature and exposure
    # evidence, while its empty descriptor list remains separate from traversal.
    legacy_cwd = work / "inspection legacy core workspace"
    legacy_cwd.mkdir()
    legacy_jar = Path("candidate compiler 雪.jar")
    shutil.copyfile(candidate_jar, legacy_cwd / legacy_jar)
    copy_inspection_fixtures(prepared, legacy_cwd)
    legacy_input = f"test_workspace/inspect_library/{LEGACY_CORE}"
    legacy_context = "test_workspace/inspect_library/legacy-empty-context.json"
    (legacy_cwd / legacy_context).write_text(
        json.dumps({"schema_version": 1, "libraries": []}), encoding="utf-8"
    )
    legacy_outputs = {
        "--sig-out": f"{INSPECTION_OUTPUTS}/legacy.sig",
        "--exposures-out": f"{INSPECTION_OUTPUTS}/legacy-exposures.json",
        "--membership-out": f"{INSPECTION_OUTPUTS}/legacy-membership.json",
        "--descriptor-out": f"{INSPECTION_OUTPUTS}/legacy-descriptor.json",
    }
    (legacy_cwd / INSPECTION_OUTPUTS).mkdir()
    with PersistentWorker(java, legacy_jar, legacy_cwd) as persistent:
        legacy_command = inspection_command(
            legacy_input,
            "core_alpha",
            "9.0.1",
            [f"sha256:{LEGACY_CORE_SHA256}", f"blake3:{LEGACY_CORE_BLAKE3}"],
            dependency_context=legacy_context,
            outputs=legacy_outputs,
        )
        compare_command(
            java,
            legacy_jar,
            legacy_cwd,
            persistent,
            "inspect-released-legacy-core",
            legacy_command,
            list(legacy_outputs.values()),
            request_fields={"requestId": 370},
        )
        legacy_signature = (legacy_cwd / legacy_outputs["--sig-out"]).read_bytes()
        legacy_exposures = json.loads(
            (legacy_cwd / legacy_outputs["--exposures-out"]).read_text(encoding="utf-8")
        )
        legacy_descriptor = json.loads(
            (legacy_cwd / legacy_outputs["--descriptor-out"]).read_text(encoding="utf-8")
        )
        if not legacy_signature or not legacy_exposures.get("packages"):
            raise AssertionError("released legacy core did not yield nonempty signature/exposure projections")
        if legacy_descriptor["digests"] != {
            "sha256": LEGACY_CORE_SHA256,
            "blake3": LEGACY_CORE_BLAKE3,
        } or legacy_descriptor["dependencies"]:
            raise AssertionError(f"legacy core identity or empty descriptors changed: {legacy_descriptor!r}")



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

    def __init__(self, java: str, jar: Path, cwd: Path, timeout: float = 60):
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

    def request_raw(self, payload: bytes, expected_id: int) -> dict[str, object]:
        self._send(payload)
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


def reject_input_alias(
    java: str,
    jar: Path,
    cwd: Path,
    name: str,
    command: list[str],
    inputs: list[str],
) -> None:
    parameter_path = Path("parameter files 雪") / f"{name}.params"
    parameter_file = cwd / parameter_path
    parameter_file.parent.mkdir(parents=True, exist_ok=True)
    parameter_file.write_text("\n".join(command) + "\n", encoding="utf-8")
    before = {path: (cwd / path).read_bytes() for path in inputs}

    direct = invoke(java, jar, cwd, "tool", *command)
    if direct.returncode == 0 or b"refers to requested input" not in direct.stderr:
        raise AssertionError(
            f"direct {name} did not reject an input alias:\n"
            f"{direct.stderr.decode('utf-8', errors='replace')}"
        )
    if {path: (cwd / path).read_bytes() for path in inputs} != before:
        raise AssertionError(f"direct {name} changed a requested input while rejecting its alias")

    worker = invoke(java, jar, cwd, "tool", "bazel-worker", "@" + parameter_path.as_posix())
    check_result(name, direct, worker)
    if worker.returncode == 0:
        raise AssertionError(f"one-shot {name} accepted an output alias to a requested input")
    if {path: (cwd / path).read_bytes() for path in inputs} != before:
        raise AssertionError(f"one-shot {name} changed a requested input while rejecting its alias")


TOOLCHAIN_MANIFEST = "manifests 雪/toolchain.json"
TOOLCHAIN_STAMP = "generated files 雪/toolchain stamp"
TOOLCHAIN_CORE = "components 雪/core.bosatsu_lib"
TOOLCHAIN_SOURCE = "sources 雪/Prog.bosatsu"
TOOLCHAIN_SECOND_SOURCE = "sources 雪/Two.bosatsu"
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
    out: str = TOOLCHAIN_STAMP,
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
        out,
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
    (cwd / TOOLCHAIN_STAMP).parent.mkdir(parents=True)
    candidate_name = Path("candidate compiler 雪.jar")
    shutil.copyfile(candidate_jar, cwd / candidate_name)
    candidate_bytes = (cwd / candidate_name).read_bytes()
    write_toolchain_manifest(cwd, compiler_bytes=candidate_bytes)

    with PersistentWorker(java, candidate_name, cwd) as persistent:
        # The selected candidate jar is also the exact jar launched for verification.
        compare_command(
            java,
            candidate_name,
            cwd,
            persistent,
            "toolchain-compiler-only",
            toolchain_command(candidate_name),
            [TOOLCHAIN_STAMP],
        )
        reject_input_alias(
            java,
            candidate_name,
            cwd,
            "toolchain-manifest-alias",
            toolchain_command(candidate_name, out=TOOLCHAIN_MANIFEST),
            [TOOLCHAIN_MANIFEST, candidate_name.as_posix()],
        )
        reject_input_alias(
            java,
            candidate_name,
            cwd,
            "toolchain-candidate-alias",
            toolchain_command(candidate_name, out="./" + candidate_name.as_posix()),
            [TOOLCHAIN_MANIFEST, candidate_name.as_posix()],
        )

        core = core_library_fixture()
        (cwd / TOOLCHAIN_CORE).parent.mkdir(parents=True, exist_ok=True)
        (cwd / TOOLCHAIN_CORE).write_bytes(core)
        write_toolchain_manifest(cwd, compiler_bytes=candidate_bytes, core_bytes=core)
        compare_command(
            java,
            candidate_name,
            cwd,
            persistent,
            "toolchain-binary-core",
            toolchain_command(candidate_name, core_library=TOOLCHAIN_CORE),
            [TOOLCHAIN_STAMP],
        )
        reject_input_alias(
            java,
            candidate_name,
            cwd,
            "toolchain-core-alias",
            toolchain_command(candidate_name, core_library=TOOLCHAIN_CORE, out=TOOLCHAIN_CORE),
            [TOOLCHAIN_MANIFEST, candidate_name.as_posix(), TOOLCHAIN_CORE],
        )

        wrong_core = core_library_fixture(repo="https://example.invalid/altered-core")
        (cwd / TOOLCHAIN_CORE).write_bytes(wrong_core)
        compare_command(
            java,
            candidate_name,
            cwd,
            persistent,
            "toolchain-substituted-core",
            toolchain_command(candidate_name, core_library=TOOLCHAIN_CORE),
            [TOOLCHAIN_STAMP],
            expect_success=False,
        )

        wrong_identity = core_library_fixture(name="other_core")
        (cwd / TOOLCHAIN_CORE).write_bytes(wrong_identity)
        write_toolchain_manifest(
            cwd,
            compiler_bytes=candidate_bytes,
            core_bytes=wrong_identity,
            core_name="core_alpha",
        )
        (cwd / TOOLCHAIN_STAMP).unlink(missing_ok=True)
        compare_command(
            java,
            candidate_name,
            cwd,
            persistent,
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
            persistent,
            "toolchain-core-source",
            toolchain_command(candidate_name, core_sources=[TOOLCHAIN_SOURCE]),
            [TOOLCHAIN_STAMP],
        )

        second_source = b"package Example/Two\n\nvalue = 2\n"
        (cwd / TOOLCHAIN_SECOND_SOURCE).write_bytes(second_source)
        write_toolchain_manifest(
            cwd,
            compiler_bytes=candidate_bytes,
            core_sources={"Bosatsu/Prog": source, "Example/Two": second_source},
        )
        for source_path in (TOOLCHAIN_SOURCE, TOOLCHAIN_SECOND_SOURCE):
            reject_input_alias(
                java,
                candidate_name,
                cwd,
                "toolchain-source-alias-" + Path(source_path).stem,
                toolchain_command(
                    candidate_name,
                    core_sources=[TOOLCHAIN_SOURCE, TOOLCHAIN_SECOND_SOURCE],
                    out=source_path,
                ),
                [
                    TOOLCHAIN_MANIFEST,
                    candidate_name.as_posix(),
                    TOOLCHAIN_SOURCE,
                    TOOLCHAIN_SECOND_SOURCE,
                ],
            )

        changed_source = b"package Bosatsu/Prog\n\nmain = 2\n"
        (cwd / TOOLCHAIN_SOURCE).write_bytes(changed_source)
        compare_command(
            java,
            candidate_name,
            cwd,
            persistent,
            "toolchain-modified-core-source",
            toolchain_command(candidate_name, core_sources=[TOOLCHAIN_SOURCE]),
            [TOOLCHAIN_STAMP],
            expect_success=False,
        )
        wrong_package = b"package Bosatsu/Changed\n\nmain = 1\n"
        (cwd / TOOLCHAIN_SOURCE).write_bytes(wrong_package)
        (cwd / TOOLCHAIN_STAMP).unlink(missing_ok=True)
        compare_command(
            java,
            candidate_name,
            cwd,
            persistent,
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
            persistent,
            "toolchain-native-runtime",
            toolchain_command(candidate_name, runtime_archive=TOOLCHAIN_RUNTIME),
            [TOOLCHAIN_STAMP],
        )
        reject_input_alias(
            java,
            candidate_name,
            cwd,
            "toolchain-runtime-alias",
            toolchain_command(candidate_name, runtime_archive=TOOLCHAIN_RUNTIME, out=TOOLCHAIN_RUNTIME),
            [TOOLCHAIN_MANIFEST, candidate_name.as_posix(), TOOLCHAIN_RUNTIME],
        )

        changed_runtime = runtime_archive_fixture() + b"independently hashed substitute"
        (cwd / TOOLCHAIN_RUNTIME).write_bytes(changed_runtime)
        (cwd / TOOLCHAIN_STAMP).unlink(missing_ok=True)
        compare_command(
            java,
            candidate_name,
            cwd,
            persistent,
            "toolchain-substituted-runtime",
            toolchain_command(candidate_name, runtime_archive=TOOLCHAIN_RUNTIME),
            [TOOLCHAIN_STAMP],
            expect_success=False,
        )

        altered_name = Path("substituted compiler 雪.jar")
        shutil.copyfile(cwd / candidate_name, cwd / altered_name)
        with zipfile.ZipFile(cwd / altered_name, mode="a") as jar_file:
            jar_file.writestr("toolchain-fixture-marker.txt", b"different candidate bytes\n")
        (cwd / TOOLCHAIN_STAMP).unlink(missing_ok=True)
        with PersistentWorker(java, altered_name, cwd) as altered_worker:
            compare_command(
                java,
                altered_name,
                cwd,
                altered_worker,
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


def check_escaped_json_request(
    java: str, jar: Path, cwd: Path, persistent: PersistentWorker
) -> None:
    output = "generated files 雪/escaped/slash headers.json"
    command = ["headers", "--src", SOURCE, "--out", output]
    (cwd / output).parent.mkdir(parents=True, exist_ok=True)
    params = cwd / "parameter files 雪" / "escaped JSON params.params"
    params.parent.mkdir(parents=True, exist_ok=True)
    params.write_text("\n".join(command) + "\n", encoding="utf-8")

    direct = invoke(java, jar, cwd, "tool", *command)
    if direct.returncode != 0:
        raise AssertionError(f"direct escaped-path headers failed: {direct.stderr!r}")
    expected = output_bytes(cwd, [output])
    params_result = invoke(
        java, jar, cwd, "tool", "bazel-worker", "@" + params.relative_to(cwd).as_posix()
    )
    check_result("escaped JSON @params", direct, params_result)
    if output_bytes(cwd, [output]) != expected:
        raise AssertionError("escaped JSON @params output differs from direct execution")
    assert_no_scratch(cwd, "escaped JSON @params")

    payload = json.dumps(
        {
            "arguments": command,
            "requestId": 72,
            "inputs": [{"path": SOURCE, "digest": ""}],
            "unknown": {"nested": "escaped/slash"},
        },
        ensure_ascii=False,
        separators=(",", ":"),
    )
    payload = payload.replace("/", r"\/")
    payload = payload.replace('"headers"', r'"\u0068eaders"', 1)
    if r"\/" not in payload or r"\u0068" not in payload:
        raise AssertionError(f"escaped JSON fixture is missing its wire escapes: {payload!r}")
    (cwd / output).unlink()
    response = persistent.request_raw((payload + "\n").encode("utf-8"), 72)
    if response["exitCode"] != direct.returncode or response["output"] != (
        direct.stdout + direct.stderr
    ).decode("utf-8", errors="strict"):
        raise AssertionError(f"escaped JSON request differs from direct execution: {response!r}")
    if output_bytes(cwd, [output]) != expected:
        raise AssertionError("escaped JSON request output differs from direct and @params")
    assert_no_scratch(cwd, "escaped JSON request")


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
        check_escaped_json_request(java, jar, cwd, persistent)

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
    prior_output = cwd / "generated files 雪" / "prior output.json"
    prior_output.write_bytes(b"preserve prior output")

    with PersistentWorker(java, jar, cwd) as clean:
        status, stdout, stderr = clean.finish()
        if status != 0 or stdout or stderr:
            raise AssertionError(
                f"clean EOF failed: status={status}, stdout={stdout!r}, stderr={stderr!r}"
            )

    action = [
        "headers",
        "--src",
        SOURCE,
        "--out",
        "generated files 雪/malformed syntax output.json",
    ]
    invalid_operation = json.dumps(
        {"arguments": action, "requestId": 75}, ensure_ascii=False, separators=(",", ":")
    ).replace('"headers"', r'"\x68eaders"', 1)
    invalid_ignored_field = json.dumps(
        {"arguments": action, "requestId": 76}, ensure_ascii=False, separators=(",", ":")
    )[:-1] + r',"ignored":{"nested":"\x41"}}'
    failures = (
        ("malformed JSON", b'{"arguments":[}\n', "invalid worker JSON"),
        ("truncated frame", b'{"arguments":["headers"]}', "truncated worker request"),
        ("invalid UTF-8", b"\xc3(\n", "not valid UTF-8"),
        ("non-JSON escaped operation", (invalid_operation + "\n").encode("utf-8"), "invalid worker JSON"),
        ("invalid ignored nested string", (invalid_ignored_field + "\n").encode("utf-8"), "invalid worker JSON"),
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
    parser.add_argument("--inspection-fixtures", type=Path, required=True)
    args = parser.parse_args()
    jar = args.jar.resolve()
    if not jar.is_file():
        parser.error(f"candidate jar does not exist: {jar}")
    inspection_fixtures = args.inspection_fixtures.resolve()
    if not inspection_fixtures.is_dir():
        parser.error(
            f"prepared inspection fixtures do not exist: {inspection_fixtures}; run "
            "python3 scripts/prepare_inspect_library_fixtures.py --out-dir <directory>"
        )
    java = shutil.which("java")
    if java is None:
        raise RuntimeError("java is required")
    with tempfile.TemporaryDirectory(prefix="bosatsu-bazel-worker-") as tmp:
        work = Path(tmp)
        test_toolchain_verification(java, jar, work)
        test_worker(java, jar, work)
        test_bosatsuj(java, jar, work)
        test_library_inspection(java, jar, work, inspection_fixtures)
    print("candidate jar Bazel worker and library inspection acceptance passed")


if __name__ == "__main__":
    main()
