# Code Plan #2475

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2475` 0j: Dispatch one-shot Bazel parameter files through the JVM file runner
- Pending steps: `0`
- Completed steps: `3`
- Total steps: `3`

## Summary

Implement `java -jar <candidate> tool bazel-worker @params` by decoding one UTF-8 multiline parameter file, classifying its operation, and executing it through the existing FileCommandRunner lifecycle. Initially accept only `headers`, `compile-packages`, `validate-graph`, and `emit-c`. Verify standalone parity, argument fidelity, rejection behavior, and failure recovery through JVM tests and the assembled candidate jar, then update design tracker item 0j.

The bounded production surface is the JVM routing boundary in PathModule, a small worker argument/operation module, and worker-facing methods on FileCommandRunner. Existing command parsers and StagedOutputs remain the enforcing mechanisms for command options and output publication. Persistent JSON transport, declared-input authorization for that transport, inspection/verification implementations, Bazel rules, and compiler/backend changes belong to other roadmap children.

Plan trajectory: initial steps 2; review revisions 1; review-origin steps 1.

## Current State

The one-shot dispatcher, shared four-operation classification, parameter-file decoder, FileCommandRunner integration, candidate-jar acceptance harness, launcher checks, and tracker update are implemented. Both original plan steps are completed and remain immutable history.

Pre-PR inspection confirmed that BazelWorkerTest's runCli and runCliCaptured helpers replace JVM-wide streams while concurrent CLI suites can write to them. One corrective test-only step moves those route assertions into the existing candidate-jar subprocess harness while preserving malformed-startup and successful-route coverage. Production routing and lifecycle behavior need no change for this defect.

The source issue, roadmap #2470, worker_args node, and dependency handoff remain aligned, with no upstream dependencies or scope amendment. This revision inspected the authoritative candidate patch and repository guidance, changed no repository files, and ran no tests.

## Problem

Bazel one-shot actions cannot currently submit a multiline parameter file to the JVM CLI. They need exactly one expansion that preserves argument boundaries, spaces, Unicode, empty values, and LF/CRLF behavior, followed by in-process execution with ordinary diagnostics and exit status. Reusing the runner's broad standalone supports list would expose legacy operations beyond the required worker surface. The dispatcher must therefore classify worker operations separately while preserving the runner's existing publication, cleanup, and recovery guarantees.

## Steps

1. [x] `step-1` Implement classified one-shot dispatch through FileCommandRunner

Add a small JVM module at `cli/src/main/scala/dev/bosatsu/BazelWorker.scala` for the worker-operation classification and parameter-file decoding. Represent the recognized operations with an immutable enum or equivalent checked type containing exactly `headers`, `compile-packages`, `validate-graph`, and `emit-c`. Keep this as the sole worker allowlist; FileCommandRunner.supports remains the broader standalone routing predicate. Expose the classification for reuse by the later persistent transport and registration by the inspection/verification children, without implementing those children now.

Add worker-facing execution methods to `cli/src/main/scala/dev/bosatsu/FileCommandRunner.scala`. Both captured and streaming worker callers must classify the leading operation before delegating directly to the existing runTo request lifecycle. Share existing capture plumbing where needed rather than copying request setup or publication. Retain the existing four command parsers as the option boundary: they already reject `--cache_dir`, so do not add a token blacklist that could mistake an argument value for an option. Do not construct a second command engine or publish outputs in the transport.

In `cli/src/main/scala/dev/bosatsu/PathModule.scala`, intercept the `tool bazel-worker` prefix before ordinary file-command routing, including malformed invocations. Require exactly one argument consisting of `@` followed by a nonempty filename. Read that file relative to the invocation directory using strict UTF-8 decoding. Parse the first physical line as the operation and each remaining physical line as one argument. Accept LF and CRLF, removing only the CR belonging to a CRLF delimiter. Do not trim, unquote, unescape, normalize Unicode, or shell-split content. A final line terminator closes the preceding line without creating another argument; an additional empty line represents an empty argument. Accept an unterminated final nonempty line. Reject empty files and empty operation lines. Expand only the outer file: subsequent `@...` values remain literal arguments. Report invocation, read, decoding, and classification failures through the ordinary diagnostic/ExitCode.Error path, using existing exception reporting where applicable.

Add `BazelWorkerTest.scala` and focused extensions to existing runner/strict-interface tests. Exercise the production file loader and worker execution methods with an explicit directory and captured streams, using FileCommandFixtures. Compare direct and worker results for all four operations using small fixtures from HeadersCommandTest, CompilePackagesCommandTest, ValidateGraphCommandTest, and EmitCCommandTest. Reuse the strict-interface X/Sibling/Y scenarios from StrictInterfacesCommandTest through a minimal test-only invocation callback so they execute through parameter files as well as directly. Add a worker compile failure caused by a blocked output destination, repair the destination, and successfully retry with the same runner. Verify prior outputs and cleanup after failure, and exercise worker output retention/symlink behavior while retaining the existing comprehensive staging tests.

Run focused suites with `sbt "cli/testOnly dev.bosatsu.BazelWorkerTest dev.bosatsu.FileCommandRunnerTest dev.bosatsu.StagedOutputsTest dev.bosatsu.StrictInterfacesCommandTest"`. Keep production APIs immutable, use explicit arguments on new APIs, and follow existing formatting without running scalafmt.

#### Invariants

- The complete in-scope worker call chain is PathModule's bazel-worker route, one parameter-file decode, the runner's classified worker entry point, and existing runTo/requestPlatform/publication. No accepted one-shot path bypasses classification or publishes outputs independently.
- Exactly the four registered build operations are accepted initially; legacy, online, publishing, process-executing, and nested worker operations cannot reach execution through the worker entry point.
- The existing registered command parsers reject filesystem-cache options; ordinary interactive command availability remains unchanged.
- One physical argument line yields one unchanged argument value, including empty values and spaces; only LF/CRLF delimiters are removed, and no recursive expansion occurs.
- Successful completion and failed-request cleanup retain FileCommandRunner's existing resource ownership, staging, rollback, symlink, access, and request-work completion guarantees.

#### Property Tests

- Generate nonempty recognized operation vectors with Unicode arguments excluding physical line delimiters, including empty strings, whitespace, quotes, backslashes, and leading @ characters. Encoding each entry as an LF- or CRLF-terminated line and decoding must recover the exact vector.
- For vectors with a nonempty final entry, adding or removing its final line terminator must preserve the decoded vector.
- For the bounded four-operation inventory, worker and direct execution on equivalent fixtures must produce equal exit codes, stdout, stderr, and declared output bytes.

#### Assertion Tests

- Cover empty files, empty operation lines, invalid UTF-8, missing/unreadable parameter files, absent @ filenames, multiple startup arguments, and unsupported persistent-worker startup. Each must return a diagnostic and failure status without publishing artifacts.
- Pin LF, CRLF, mixed supported delimiters, final unterminated lines, consecutive empty arguments, and trailing empty arguments. Verify a nested @ value is passed literally and is never opened as another parameter file.
- Reject unknown operations and representative existing commands including fetch, publish, eval/test execution, transpile, protoc-plugin, check, and assemble. Assert the worker classification contains exactly the four intended names.
- For every registered operation, reject both separate-value and equals forms of --cache_dir and reject an unknown option without creating a cache or output artifact.
- Compare successful and failing direct/worker executions for the four operations, including missing command inputs and invalid source diagnostics.
- Run the existing strict-interface exposure/unused-interface matrix through the one-shot file path, preserving direct invocation coverage.
- Force a multi-output worker compilation write failure, verify previous bytes survive and staging scratch is removed, then repair and succeed using the same runner. Cover byte-identical output retention and a symlink output through the worker path.

#### Completion Notes

Implemented strict one-file parameter decoding, the four-operation worker classification, PathModule routing, and captured/streaming FileCommandRunner entry points. Added parity, rejection, lifecycle recovery, symlink, and strict-interface coverage. The focused suites passed (42 tests, 1 skipped), and the final BazelWorkerTest rerun passed (8 tests). `bash ./test_cli.sh` passed; direct `./test_cli.sh` stopped because `/bin/sh` lacks `time`. MergeXO retains the supervised `scripts/test_basic.sh` gate.

2. [x] `step-2` Verify candidate-jar and launcher behavior and complete tracker 0j

Add `scripts/test_bazel_worker.py` as a portable subprocess acceptance harness taking an explicit `--jar` path. Use argument arrays, isolated working directories, and fixtures created by the harness. Invoke the assembled candidate with `java -jar <candidate> tool bazel-worker @params` and compare it with direct `tool <operation> ...` invocation using the same relative input and output names. Cover all four registered operations, including a compiled test bundle and local runtime manifest for emit-c; emission does not require compiling or running native C. Use source, manifest, output, and parameter-file paths containing spaces and non-ASCII characters. Compare raw output-file bytes, captured stdout/stderr, and process status. Include failing command cases and a blocked output followed by a successful retry; same-runner recovery is established by step-1 tests.

Wire this harness into `test_cli.sh` immediately after its existing assembly build, passing the freshly assembled jar selected consistently with bosatsuj's assembly convention. Do not independently rebuild another jar inside the harness. Extend `scripts/test_launcher_passthrough.sh` to retain its existing eval case and verify the literal `tool`, `bazel-worker`, and `@<path with spaces and Unicode>` arguments through both bosatsuj and the Java-platform bosatsu launcher. Use local fake Java/artifact fixtures for argv recording, with no artifact download; exercise the actual bosatsuj path against the candidate in the subprocess harness as well. Change launcher production code only if these checks demonstrate an argument-forwarding defect.

Update the command-contract section of `docs/design/hermetic-bazel-rules.md` with the implemented line, terminal-newline, empty-argument, strict UTF-8, and single-expansion semantics. Document the four-operation classification, rejection of cache options, and the registration point downstream children must extend. After candidate acceptance passes, mark only tracker item 0j complete and add durable implementation/test provenance using issue #2475 and checked-in test links; include the implementing PR reference if already available. Other tracker items retain their current status.

Run `scripts/test_launcher_passthrough.sh` and `./test_cli.sh`. Inspect changed executable coverage using the repository entry point `sbt "coverage; clean; coreJVM/test; cli/test; coverageReport"`, addressing uncovered new error branches with focused tests. MergeXO separately owns the supervised `scripts/test_basic.sh` gate, which supplies the shared JVM/CLI test gate; do not add a step that runs or waits for that gate. Keep the acceptance harness and tests in the PR. No separate evidence document is needed.

#### Invariants

- Acceptance evidence exercises the candidate jar containing this branch's dispatcher and compares actual process behavior and artifact bytes.
- Both JVM launcher paths forward the literal @params argument intact, including spaces and Unicode in its filename.
- The subprocess harness invokes the compiler through argv arrays and does not introduce shell tokenization, nested expansion, or a native-tool dependency.
- The documented contract and tracker describe only the implemented one-shot surface; persistent transport and other roadmap deliverables remain separate.

#### Property Tests

- Apply the candidate-jar parity contract across the four registered operations and both LF and CRLF parameter-file encodings: equivalent argument vectors yield equal artifact bytes, diagnostics, and status.

#### Assertion Tests

- Run the candidate jar with paths containing spaces and non-ASCII characters; compare successful headers, package compilation, graph validation, and test C emission against direct invocation.
- Compare command failures for invalid source, missing input, and invalid options. Check malformed parameter input and rejected online/process/cache requests return nonzero status with useful stderr and no new outputs.
- Exercise empty argument values, terminal newline cases, literal @ operands, and a failed write followed by a successful candidate invocation.
- Record argv from both Java launcher paths and assert @params remains exactly one unchanged argument; retain the existing eval passthrough regression.
- Run ./test_cli.sh with the newly integrated candidate harness and inspect the repository coverage report for changed executable branches.

#### Completion Notes

Added the candidate-jar parity harness, launcher passthrough checks, and tests for the streaming worker route; documented parameter-file semantics and updated tracker 0j with issue and test links. The candidate harness and `./test_cli.sh` passed; `BazelWorkerTest` passed all 10 tests under focused coverage. The full coverage run hit a 30-second timeout in `ToolAndLibCommandTest`; its isolated rerun passed all 172 tests without coverage.

3. [x] `step-3` Isolate worker CLI stream assertions in subprocess tests

Invariant family: cli-stream-isolation

The route tests added in step-1 and step-2 use runCli and runCliCaptured in cli/src/test/scala/dev/bosatsu/BazelWorkerTest.scala to replace JVM-wide System.err and System.out. Concurrent suites, including PathModuleTest's FibBench execution, write through those same streams. This can contaminate assertions or swallow unrelated diagnostics. Repair both helpers together by removing them and moving their route-level assertions into the existing scripts/test_bazel_worker.py candidate-jar subprocess harness. Both existing steps are completed; no pending step can absorb this correction.

Preserve the complete malformed-startup inventory through subprocess cases: no parameter argument, a bare filename, bare @, a missing file, multiple parameter arguments, --persistent_worker, an empty file, an empty operation line, and invalid UTF-8. Reuse existing missing-file and UTF-8 cases rather than duplicating them. Assert failure status, useful stderr, empty stdout, and no published outputs. Preserve the accepted headers route assertion by extending the existing successful candidate-jar case to require empty stdout/stderr while retaining direct/worker status and output-byte parity with spaces and Unicode paths.

Retain decoder, classification, lifecycle, and explicitly scoped FileCommandRunner stream tests in Scala. Process-owned pipes from the harness's existing invoke helper provide the isolation for every migrated route case, eliminating the shared-state race across both affected helpers. Do not introduce suite-local locks, disable parallel testing, or change production routing for this test defect.

Run sbt "cli/testOnly dev.bosatsu.BazelWorkerTest dev.bosatsu.PathModuleTest" with normal concurrency, then ./test_cli.sh to assemble this branch's candidate and execute the extended subprocess acceptance harness. Completion requires preserving the migrated assertions and removing all JVM-wide stream replacement from BazelWorkerTest.

#### Invariants

- Worker route assertions capture only the candidate subprocess's stdout and stderr.
- BazelWorkerTest never replaces JVM-wide output streams, so concurrent suites retain their own diagnostics and output.
- Malformed-startup and successful-route coverage remains intact through the assembled candidate jar.
- Existing runner-scoped tests continue to cover worker classification and request lifecycle behavior.

#### Property Tests

- Across the malformed-startup case table, each isolated candidate invocation fails with useful stderr, empty stdout, and no published outputs.

#### Assertion Tests

- A valid headers parameter file with spaces and Unicode paths succeeds with empty stdout/stderr and output bytes matching direct invocation.
- Run BazelWorkerTest and PathModuleTest together under normal test concurrency.
- Run ./test_cli.sh against the freshly assembled candidate, including all migrated startup cases.
- Inspect BazelWorkerTest to confirm both global-capture helpers and all System.setOut/System.setErr calls are removed.

#### Completion Notes

Removed the PathModule route tests and JVM-wide stream replacement helpers from BazelWorkerTest, retaining the runner-scoped stream test. Moved all nine malformed-startup cases into the candidate-jar harness, asserting failure status, useful stderr, empty stdout, and an unchanged workspace; successful headers now also requires empty stdout/stderr while preserving parity checks. `sbt "cli/testOnly dev.bosatsu.BazelWorkerTest dev.bosatsu.PathModuleTest"` passed (26 tests), and `./test_cli.sh` passed, including candidate-jar acceptance, launcher checks, and 6,118 Bosatsu tests.
