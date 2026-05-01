# Code Plan #2356

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2356` Migrate file and directory C IO effects to libuv-backed operations
- Source design doc: `docs/design/2342-document-the-libuv-c-runtime-integration-contract.md`
- Pending steps: `0`
- Completed steps: `5`
- Total steps: `5`

## Summary

Migrate the existing C backend `IO/Core` file and directory externals from direct stdio/POSIX filesystem calls to libuv-backed operations while preserving the current Bosatsu API, value shapes, handle behavior, and `IOError` mapping. The branch now has the `uv_file`-backed handle representation, file open/read/write/read_all/copy/temp-file/flush/close operations on that representation, directory/path operations routed through libuv filesystem calls where libuv preserves current semantics, focused C harness coverage across file and directory behavior, and a completed repair for reviewer finding F1 so synchronous libuv filesystem requests are cleaned on both success and negative-status completions.

## Current State

The repo has the libuv-owned C Prog runtime, private suspend/resume support, and the phase-one `IO/Core` migration for wall time, monotonic time, environment lookup, and sleep. `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c` stores runtime-owned file handles as `uv_file` descriptors, keeps standard streams non-owning, uses local `uv_fs_*` cleanup helpers for file read/write/open and suspend/resume-backed flush/close operations, and routes directory/path operations through synchronous libuv filesystem primitives for stat/lstat, mkdir, chmod mode repair, scandir, unlink, rmdir, and rename. This round added a cleanup helper for synchronous `uv_fs_*` start results and routed negative-status paths for read, write, stat/lstat, mkdir, chmod, unlink, rmdir, rename, open_file, recursive scandir, and list_dir through request cleanup before propagating errors. The narrow `mkdtemp` compatibility fallback remains for `create_temp_dir` because libuv has no exact prefix-preserving equivalent. Focused `make -C c_runtime test_out` and the configured `scripts/test_basic.sh` gate both passed after the cleanup repair.

## Problem

The remaining C `IO/Core` directory and path effects previously bypassed the libuv filesystem API. That implementation gap is now mostly closed for the scoped operations, but pre-PR review identified an approval-blocking cleanup contract violation: synchronous libuv filesystem requests returned immediately on negative status in several helpers, which could leak libuv request-owned state even though observable behavior tests still passed. This round closed that cleanup-contract gap without changing the existing `IOError` mapping or Bosatsu return shapes.

## Steps

1. [x] `step-1` Introduce Libuv File Handle And Fs Helpers

Replace the stdio-centered private file handle implementation in `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c` with a libuv-compatible representation based on `uv_file` for runtime-owned files, while preserving the existing generated symbols and Bosatsu handle values. Add small local helpers for starting and completing `uv_fs_*` work through the existing Prog suspend/resume API, converting libuv status values through the existing `IOError` mapping, and cleaning up every `uv_fs_t` request with `uv_fs_req_cleanup`. Keep stdin/stdout/stderr behavior compatible, including not closing process-owned standard streams.

#### Invariants

- No public Bosatsu API, generated symbol name, or `Prog` runner entry point changes.
- Runtime-owned file handles record readable, writable, close-on-close, and closed state exactly as today.
- Closing a runtime-owned file handle closes the underlying descriptor at most once; closing stdin/stdout/stderr remains a no-op for process ownership.
- Closed, read-only, and write-only handle checks continue to raise the same existing `IOError` variants before scheduling filesystem work.
- Every libuv filesystem request is cleaned up exactly once, and any suspended request remains GC-reachable until completion.

#### Property Tests

- For a deterministic table of handle states and requested operations, assert the operation is either allowed or raises the same BadFileDescriptor-style error class as the current contract.
- For repeated open/close sequences over generated temporary filenames, assert close is idempotent and no later read/write/flush succeeds on the closed handle.

#### Assertion Tests

- C harness coverage now checks opening a missing file for read as NotFound and CreateNew on an existing path as AlreadyExists.
- C harness coverage now checks closing a runtime-owned file handle twice succeeds and then verifies read on a closed handle reports BadFileDescriptor.
- Standard streams remain constructible as non-owning handles; their process-owned flush behavior stays on a narrow stdio compatibility branch.

#### Completion Notes

Implemented `BSTS_Core_Handle` around `uv_file`, with `FILE *` retained only for non-owning standard-stream flush compatibility. Added local `uv_fs_*` helpers that clean requests and used suspend/resume-backed requests for runtime-owned `flush` and `close`. To avoid an unsafe intermediate state where `open_file` produced descriptors but data operations expected `FILE *`, this round also moved file open/read/write/read_all/copy/temp-file handles onto the new descriptor representation. Verified with `make -C c_runtime test_out` and `scripts/test_basic.sh` before pre-PR review. Reviewer finding F1 later showed this step's cleanup invariant was not fully satisfied by synchronous error paths; that repair is now completed in `step-5`.

2. [x] `step-2` Expand File Data Operation Coverage

The core file data operations use the libuv-backed `uv_file` handle representation, and this step adds focused C harness coverage for that descriptor model. Cover `read_utf8`, `write_utf8`, `read_bytes`, `write_bytes`, `read_all_bytes`, `copy_bytes`, `flush`, and `create_temp_file` behavior. Preserve EOF behavior as `None` for bounded reads, empty bytes for `read_all_bytes` on an empty file, existing invalid UTF-8 behavior for `read_utf8`, and integer byte counts for `copy_bytes`.

#### Invariants

- Bounded byte and UTF-8 reads return `None` only at EOF before reading bytes.
- `read_utf8` accepts only valid UTF-8 byte prefixes and raises the existing InvalidUtf8 variant for invalid input.
- `read_all_bytes` preserves chunk-size validation, result-size bounds, and byte ordering.
- `copy_bytes` preserves chunk-size validation, optional max-total semantics, EOF termination, and the exact number of copied bytes returned.
- Append, truncate, read, and create-new modes preserve current observable file contents and error behavior.
- Flush preserves successful unit behavior for writable runtime-owned handles and no-op success for non-writable handles where that is the current contract.

#### Property Tests

- For byte arrays covering empty files, bounded reads, non-text bytes, and multi-chunk reads, write through the C `IO/Core` path, read back with bounded reads and `read_all_bytes`, and assert byte-for-byte equality.
- For copy limits and chunk sizes that cross chunk boundaries, copy from a source file to a destination file and assert destination bytes equal the expected prefix and returned count equals the copied length.
- For valid UTF-8 strings containing multibyte data, assert write/read round trips preserve the exact string bytes.

#### Assertion Tests

- C harness coverage now checks EOF on `read_bytes` and `read_utf8`.
- C harness coverage now checks invalid UTF-8 bytes read through `read_utf8` as InvalidUtf8.
- C harness coverage now checks write/read UTF-8, write/read bytes, `read_all_bytes`, `copy_bytes` with `None` and a finite limit, `flush`, and create-temp-file prefix/suffix behavior.
- C harness coverage now checks reading from a write-only handle and writing to a read-only handle as BadFileDescriptor.

#### Completion Notes

Added focused `c_runtime/test.c` harness coverage for the libuv-backed file data descriptor model. The tests exercise UTF-8 write/flush/read/EOF, byte write/flush/bounded-read/EOF, `read_all_bytes` across chunks and on an empty file, `copy_bytes` with unlimited and finite limits, invalid UTF-8 error mapping, read/write permission errors, and `create_temp_file` prefix/suffix plus writable returned handle behavior. No implementation fix was needed beyond the existing mkstemp/mkstemps compatibility fallback already normalizing to a `uv_file` handle. Verified with `make -C c_runtime test_out` and `scripts/test_basic.sh` before pre-PR review.

3. [x] `step-3` Migrate Directory And Path Operations

Move `create_temp_dir`, `list_dir`, `stat`, `mkdir`, `mkdir_with_mode`, `remove`, and `rename` toward libuv-backed filesystem calls. Use direct `uv_fs_stat`/`uv_fs_lstat`, `uv_fs_mkdir`, `uv_fs_scandir`/`uv_fs_scandir_next`, `uv_fs_unlink`, `uv_fs_rmdir`, and `uv_fs_rename` where they preserve current semantics. Keep local recursive traversal, temp-dir naming, symlink-aware remove behavior, and post-mkdir chmod/mode repair only where libuv does not provide an exact semantic match. Preserve sorted `list_dir` output, `stat` returning `None` for missing paths, stat kind/size/mtime/mode encoding, and recursive mkdir/remove behavior.

#### Invariants

- `list_dir` excludes `.` and `..`, returns joined child paths, and preserves deterministic sorted order.
- `stat` returns `None` for missing paths and raises existing `IOError` variants for other failures.
- `stat` continues to distinguish file, directory, symlink, and other kinds, with size, mtime nanoseconds, and optional POSIX mode encoded as before.
- Recursive mkdir accepts existing directory components and applies requested leaf mode semantics where supported by the platform.
- Recursive remove does not follow directory symlinks as directories and removes nested files/directories in a deterministic cleanup path.
- Rename preserves existing success and error behavior, including cross-device failures mapping to the existing variant when reported by the platform.

#### Property Tests

- C harness list_dir coverage asserts a shallow generated fixture directory returns exactly the sorted set of immediate joined child paths.
- C harness recursive remove coverage asserts a nested generated directory tree is removed and non-recursive remove fails on a non-empty directory.
- C harness mkdir_with_mode coverage asserts the requested POSIX mode bits are recorded by stat on this POSIX/libuv C runtime target.

#### Assertion Tests

- C harness coverage now checks mkdir, recursive mkdir, mkdir_with_mode, stat of file/dir/symlink, list_dir sorting, rename, non-recursive remove, and recursive remove.
- C harness coverage now checks missing path stat as `None`, listing a missing directory as NotFound, mkdir existing path as AlreadyExists, removing a missing path as NotFound, renaming from a missing source as NotFound, and non-recursive remove of a non-empty directory as NotEmpty.
- C harness coverage now checks temp-dir creation with the default directory and requested prefix, plus invalid prefix handling as InvalidArgument.

#### Completion Notes

Added synchronous libuv filesystem wrappers for stat/lstat, mkdir, chmod, unlink, rmdir, and rename, and reused the existing request cleanup/error mapping path. Migrated directory/path effects to `uv_fs_scandir`, `uv_fs_lstat`, `uv_fs_stat`, `uv_fs_mkdir`, `uv_fs_chmod`, `uv_fs_unlink`, `uv_fs_rmdir`, and `uv_fs_rename` while preserving sorted list output, symlink-aware stat/remove behavior, recursive mkdir/remove traversal, and existing `IOError` variants. Kept `mkdtemp` for `create_temp_dir` with a short compatibility comment because libuv has no exact prefix-preserving temp-directory helper. Added focused C harness coverage for success and common failure cases. Verified with `make -C c_runtime test_out` and `scripts/test_basic.sh` before pre-PR review. Reviewer finding F1 later showed that some synchronous directory/path error exits needed an explicit cleanup repair, now completed in `step-5`.

4. [x] `step-4` Verify Scope And Required Gate

Run focused C runtime coverage while iterating, then run the configured required gate `scripts/test_basic.sh` with the repo-owned 2400 second timeout before the branch is considered PR-ready. Review the final diff for accidental public API changes, uncleaned `uv_fs_t` requests, leaked malloc buffers, inconsistent `IOError` contexts, and compatibility fallbacks that should be documented in code comments.

#### Invariants

- `scripts/test_basic.sh` passes before PR submission.
- The final change remains scoped to C `IO/Core` file and directory runtime behavior plus focused C harness coverage.
- All libuv requests and runtime-owned handles have explicit cleanup/close paths for success and failure.
- Compatibility fallbacks are narrow, documented only where needed, and do not reintroduce a parallel public runtime model.

#### Property Tests

- Treat the byte round-trip, copy prefix/count, directory listing set equality, recursive remove, and mode-mask checks from earlier steps as the main contract-style coverage for this job.

#### Assertion Tests

- Focused C runtime target `make -C c_runtime test_out` passed after the directory/path operation migration and again after the final harness tightening.
- Configured required gate `scripts/test_basic.sh` passed after the final test edit.
- No platform-specific libuv filesystem behavior required a new follow-up plan item in this round.

#### Completion Notes

Verified the completed directory/path slice with `make -C c_runtime test_out`. Ran `scripts/test_basic.sh` after the initial implementation and reran it after tightening the C harness to explicitly check recursive remove and rename postconditions; the refreshed required gate passed. Final git status at that point showed only `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c` and `c_runtime/test.c` modified. Pre-PR review later found an approval-blocking cleanup issue that the completed verification checklist did not catch; the targeted repair and refreshed verification are now completed in `step-5`.

5. [x] `step-5` Close Synchronous Fs Request Cleanup Gaps

Address reviewer finding F1 by auditing every synchronous `uv_fs_*` call introduced or touched by this migration in `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c` and routing both success and negative-status completions through exactly one `uv_fs_req_cleanup` path whenever libuv may have initialized request-owned state. Preserve the existing `IOError` mapping and Bosatsu return shapes; this is a cleanup-contract repair, not a semantic rewrite. Re-run focused C runtime tests and the configured required gate after the audit so the next review can see both behavior preservation and the closed cleanup invariant.

#### Invariants

- Corresponds directly to blocking review finding F1: every synchronous `uv_fs_*` request in the migrated file/directory paths is cleaned exactly once on success and on negative libuv status.
- Error conversion observes the original libuv status after cleanup and continues to map missing open/list/stat/remove/rename cases to the same `IOError` or `None` behavior already covered by the harness.
- Recursive directory traversal and list_dir cleanup handle partial-progress failures without leaking the active scandir request or heap-owned child path buffers.
- The repair remains local to C `IO/Core` filesystem request cleanup plus focused verification.

#### Property Tests

- Use the existing missing-path and invalid-operation harness cases as cleanup-contract exercisers: missing open, missing list_dir, missing stat, missing remove, missing rename, mkdir existing path, non-empty remove, read/write permission errors, and closed/read-only/write-only handle errors should preserve their observable result while passing under the cleaned request paths.
- Leak-checker or sanitizer coverage was not added in this round; the focused and required gates exercise the synchronous error matrix behaviorally after the cleanup audit.

#### Assertion Tests

- Inspected `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c` for every synchronous `uv_fs_open`, `uv_fs_stat`, `uv_fs_lstat`, `uv_fs_mkdir`, `uv_fs_chmod`, `uv_fs_unlink`, `uv_fs_rmdir`, `uv_fs_rename`, `uv_fs_read`, `uv_fs_write`, and `uv_fs_scandir` call site and verified initialized request results route through cleanup before returning.
- Ran `make -C c_runtime test_out` after the cleanup repair; it passed.
- Ran the configured required gate `scripts/test_basic.sh` after the cleanup repair; it passed.

#### Completion Notes

Added `bsts_core_uv_fs_cleanup_start_result` to normalize synchronous libuv start results and request results through a single cleanup path. Updated `bsts_core_uv_read`, `bsts_core_uv_write_all`, stat/lstat, mkdir, chmod, unlink, rmdir, rename, open_file, recursive scandir, and list_dir negative-status paths to call cleanup before propagating errno or existing Bosatsu `IOError` values. The diff is local to `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c` and preserves the existing return shapes and error mapping. Verified with `make -C c_runtime test_out` and `scripts/test_basic.sh`.
