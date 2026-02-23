# Minimal IO Functions for Basic Tools (`core_alpha` + `Prog`)

Status: proposed  
Date: 2026-02-20  
Issue: <https://github.com/johnynek/bosatsu/issues/1744>  
PR: <https://github.com/johnynek/bosatsu/pull/1745>

## Problem
`core_alpha` currently exposes only `Bosatsu/IO/Std` (`print`, `println`, stderr variants, and `read_stdin_utf8_bytes`).

That is enough for simple CLI output/input, but not enough for basic tool-building tasks (files, directories, process spawning, env lookup, and time/sleep).

The target API is a minimal core IO surface adapted to Bosatsu style and implemented uniformly on JVM evaluator runtime, transpiled Python runtime, and C runtime.

## Decision Summary
1. Add a new package `Bosatsu/IO/Core` in `core_alpha` containing the file/process/env/time primitives and related IO types.
2. Keep `Bosatsu/Prog` as the single effect boundary; every effectful operation returns `Prog[IOError, a]`.
3. Keep `Bosatsu/IO/Error::IOError` as the shared error channel.
4. Keep `argv` at the `Main` boundary (`Main(args -> ...)`) and do not duplicate it in `Bosatsu/IO/Core`.
5. Represent `Path` as an opaque Bosatsu `struct Path(to_String: String)` and export `path_sep` + path helpers.
6. Represent `Instant`/`Duration` as opaque Bosatsu structs backed by nanosecond `Int` fields.
7. Keep `Bosatsu/IO/Std` source-compatible and add `read_line` and `read_all_stdin`.
8. Do not add direct `exit` to `Bosatsu/IO/Core`; model termination with `Main` return codes and explicit result types.

## Where in `core_alpha`
### New package
1. `test_workspace/Bosatsu/IO/Core.bosatsu`

### Existing package updates
1. `test_workspace/Bosatsu/IO/Std.bosatsu`: wrappers over `Core` (`stdout`, `stderr`, `write_text`, `read_text`, `flush`) plus `read_line` and `read_all_stdin`.
2. `test_workspace/core_alpha_conf.json`: add `Bosatsu/IO/Core` to `exported_packages`.

### Runtime implementation files
1. JVM evaluator: `core/src/main/scala/dev/bosatsu/Predef.scala`
2. Python runtime: `test_workspace/ProgExt.py`, `test_workspace/Prog.bosatsu_externals`
3. C runtime: new `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c` and `.h`, plus `c_runtime/Makefile`

## Bosatsu API Shape (adapted)
Naming follows current stdlib style (`snake_case`), except where a type name appears in the identifier (`string_to_Path`, `path_to_String`). `Bosatsu/IO/Core` exports externals directly (no duplicated `*_impl` wrappers).

```bosatsu
package Bosatsu/IO/Core

from Bosatsu/Prog import Prog
from Bosatsu/IO/Error import IOError

export (
  Path,
  path_sep,
  string_to_Path,
  path_to_String,
  path_join,
  path_parent,
  path_file_name,
  Handle,
  Process,
  Instant,
  Duration,
  FileKind(),
  FileStat(),
  OpenMode(),
  Stdio(),
  StdioConfig(),
  SpawnResult(),
  stdin,
  stdout,
  stderr,
  read_text,
  write_text,
  flush,
  close,
  open_file,
  list_dir,
  stat,
  mkdir,
  remove,
  rename,
  get_env,
  spawn,
  wait,
  now_wall,
  now_mono,
  sleep,
)

external path_sep: String

# Opaque in public API because constructor is not exported.
struct Path(to_String: String)

# Lift/projection helpers stay in Bosatsu so callers only depend on Path.
# `None` means the input is not a valid cross-platform path in the
# portable profile described below.
def string_to_Path(s: String) -> Option[Path]
def path_to_String(path: Path) -> String
external def path_join(base: Path, child: Path) -> Path
external def path_parent(path: Path) -> Option[Path]
external def path_file_name(path: Path) -> Option[String]

external struct Handle
external struct Process

# Opaque in public API because constructors are not exported.
struct Instant(epoch_nanos: Int)
struct Duration(to_nanos: Int)

enum FileKind:
  File
  Dir
  Symlink
  Other

struct FileStat(kind: FileKind, size_bytes: Int, mtime: Instant)

enum OpenMode:
  Read
  WriteTruncate
  Append

enum Stdio:
  Inherit
  Pipe
  Null
  UseHandle(handle: Handle)

struct StdioConfig(stdin: Stdio, stdout: Stdio, stderr: Stdio)

struct SpawnResult(
  proc: Process,
  stdin: Option[Handle],
  stdout: Option[Handle],
  stderr: Option[Handle],
)

external stdin: Handle
external stdout: Handle
external stderr: Handle

external def read_text(h: Handle, max_chars: Int) -> Prog[IOError, Option[String]]
external def write_text(h: Handle, s: String) -> Prog[IOError, Unit]
external def flush(h: Handle) -> Prog[IOError, Unit]
external def close(h: Handle) -> Prog[IOError, Unit]
external def open_file(path: Path, mode: OpenMode) -> Prog[IOError, Handle]
external def list_dir(path: Path) -> Prog[IOError, List[Path]]
external def stat(path: Path) -> Prog[IOError, Option[FileStat]]
external def mkdir(path: Path, recursive: Bool) -> Prog[IOError, Unit]
external def remove(path: Path, recursive: Bool) -> Prog[IOError, Unit]
external def rename(from: Path, to: Path) -> Prog[IOError, Unit]
external def get_env(name: String) -> Prog[IOError, Option[String]]
external def spawn(cmd: String, args: List[String], stdio: StdioConfig) -> Prog[IOError, SpawnResult]
external def wait(p: Process) -> Prog[IOError, Int]
external now_wall: Prog[IOError, Instant]
external now_mono: Prog[IOError, Duration]
external def sleep(d: Duration) -> Prog[IOError, Unit]
```

## Path representation tradeoff
1. Chosen shape: native `struct Path(to_String: String)` with hidden constructor, plus `path_sep` and helper APIs.
2. Advantage: callers manipulate `Path` values without runtime-specific wrapper allocation.
3. Cost: runtime implementations still convert `Path.to_String` to platform-native path objects at IO call boundaries.
4. Alternative considered: `external struct Path` parsed once per value. That can reduce repeated conversion but increases runtime payload complexity and portability risk.
5. Follow-up option: if profiling shows conversion overhead, keep the same surface API and switch internals to `external struct Path`.

## Path parsing and cross-platform behavior
1. `string_to_Path: String -> Option[Path]` is intentionally partial because not every `String` can be used as a path on every target runtime.
2. POSIX baseline: pathnames are slash-separated byte sequences; `NUL` is not allowed, slash is the separator, null pathname is invalid, and exactly two leading slashes have implementation-defined meaning.
3. Java baseline (`java.nio.file.FileSystem.getPath`): parsing is implementation-dependent, uses platform path rules, and throws `InvalidPathException` for rejected strings (for example, `NUL` on UNIX).
4. Python baseline (`pathlib`): `PurePath` parsing is lexical (no filesystem access), while concrete IO operations apply host filesystem validation later.
5. To get deterministic behavior across macOS, Linux/Unix, and Windows, `string_to_Path` uses a Bosatsu-level portable parser instead of delegating directly to host-native parsing.
6. Parsing policy:
   1. Accept separators `/` and `\` in input; normalize stored `Path.to_String` to `/`.
   2. Accept roots in these forms: relative (`a/b`), POSIX absolute (`/a/b`), Windows drive absolute (`C:/a/b`), UNC (`//server/share/a`).
   3. Reject Windows drive-relative form (`C:tmp/file`) because meaning depends on per-drive current directory.
   4. Reject Windows device namespace prefixes (`\\\\?\\`, `\\\\.\\`) in v1.
   5. Reject ambiguous POSIX-like paths that start with exactly `//` unless they parse as UNC with non-empty server/share components.
   6. Reject strings containing `NUL` (`\\u0000`) or control characters `\\u0001..\\u001F`.
   7. Reject path components containing Windows-reserved characters `< > : " | ? *` (except the drive colon in `C:/...`).
   8. Reject Windows reserved device names as components (case-insensitive): `CON`, `PRN`, `AUX`, `NUL`, `COM1..COM9`, `LPT1..LPT9` (including with extensions like `NUL.txt`).
   9. Reject components with trailing space or trailing dot to avoid Windows shell/API mismatch.
   10. Keep `.` and `..` as lexical components; do not resolve symlinks or normalize away `..` at parse time.
   11. Do not enforce `PATH_MAX`/`NAME_MAX` at parse time; those checks remain runtime/filesystem specific.
7. `path_join(base: Path, child: Path) -> Path` stays total because both inputs are already validated `Path` values.
8. Why `Option[Path]` instead of total `String -> Path`: parsing failure is expected for non-portable or malformed inputs, and callers can handle `None` without exceptions in pure code.
9. Representative rejected inputs:
   1. `""` (empty string)
   2. `"a\u0000b"`
   3. `"C:tmp\\x"`
   4. `"foo/<bar>"`
   5. `"NUL.txt"`
   6. `"dir/ends-with-dot."`
10. Representative accepted flow:
   1. `string_to_Path("src") -> Some(child)`
   2. `string_to_Path("/tmp") -> Some(base)`
   3. `path_join(base, child) -> /tmp/src`
11. References:
   1. POSIX pathname definition and resolution: <https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/V1_chap03.html#tag_03_271>, <https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/V1_chap04.html#tag_04_11>
   2. Java parsing contract (`FileSystem.getPath`): <https://docs.oracle.com/en/java/javase/21/docs/api/java.base/java/nio/file/FileSystem.html#getPath(java.lang.String,java.lang.String...)>
   3. Python `pathlib` lexical behavior and flavor differences: <https://docs.python.org/3/library/pathlib.html>
   4. Windows naming constraints: <https://learn.microsoft.com/en-us/windows/win32/fileio/naming-a-file>

## Process termination tradeoff
1. This design intentionally omits direct `exit` from `Bosatsu/IO/Core`.
2. Abrupt process termination from inside arbitrary `Prog` code makes cleanup and structured error handling less safe.
3. Preferred pattern: return status from `Main` (`Prog[err, Int]`), or encode early termination in program types such as `Prog[IOError, Result[Int, a]]`.

## Mapping from the requested primitives
1. `stdin` -> `Bosatsu/IO/Core::stdin`
2. `stdout` -> `Bosatsu/IO/Core::stdout`
3. `stderr` -> `Bosatsu/IO/Core::stderr`
4. `readText` -> `read_text`
5. `writeText` -> `write_text`
6. `flush` -> `flush`
7. `close` -> `close`
8. `openFile` -> `open_file`
9. `listDir` -> `list_dir`
10. `stat` -> `stat`
11. `mkdir` -> `mkdir`
12. `remove` -> `remove`
13. `rename` -> `rename`
14. `argv` -> `Main(args -> ...)` argument list (not duplicated in `Bosatsu/IO/Core`)
15. `getEnv` -> `get_env`
16. `exit` -> intentionally omitted; use `Main` return codes and typed early-termination (`Result[Int, a]`) instead
17. `spawn` -> `spawn`
18. `wait` -> `wait`
19. `nowWall` -> `now_wall`
20. `nowMono` -> `now_mono`
21. `sleep` -> `sleep`

## Type mapping in Bosatsu / Predef / core_alpha
1. `String`, `Int`, `Bool`, `List[a]`, `Option[a]`, `Unit` map to existing `Bosatsu/Predef` builtins.
2. `Path` maps to opaque `struct Path(to_String: String)` in `Bosatsu/IO/Core` with helpers (`string_to_Path`, `path_to_String`, `path_join`, `path_parent`, `path_file_name`) and external `path_sep`.
3. `Int64 sizeBytes` maps to `Int` (Bosatsu `Int` is arbitrary precision; runtimes convert native 64-bit values).
4. `Instant` maps to opaque `struct Instant(epoch_nanos: Int)`.
5. `Duration` maps to opaque `struct Duration(to_nanos: Int)`.
6. `FileKind`, `FileStat`, `OpenMode`, `Stdio`, `StdioConfig`, `SpawnResult` are new `enum`/`struct` types in `Bosatsu/IO/Core`.
7. `Handle` and `Process` map to opaque `external struct` types in `Bosatsu/IO/Core`.
8. Program entrypoint args use `Bosatsu/Prog::Main(run: List[String] -> forall err. Prog[err, Int])`.
9. Constructors for `Path`, `Instant`, and `Duration` are intentionally hidden from consumers (type exported, constructor not exported).

## Runtime semantics (shared contract)
1. `read_text` returns `None` only for EOF; otherwise `Some(chunk)` where chunk length is `1..max_chars`.
2. `read_text(max_chars <= 0)` returns `InvalidArgument`.
3. `list_dir` returns child paths sorted by `path_to_String` for deterministic behavior.
4. `stat` returns `None` for missing path, `Some(FileStat(...))` otherwise.
5. `stat.kind` is `Symlink` when path itself is a symlink (`lstat`-style classification).
6. `remove(recursive = true)` removes directory trees without following symlinks.
7. `spawn` never invokes a shell; `cmd` + `args` are executed directly.
8. `wait` is idempotent: once complete, repeated waits return the same exit code.
9. Time precision is nanoseconds.
10. `now_wall` returns a wall-clock timestamp value (`Instant`) whose intended encoding is UNIX epoch nanoseconds.
11. `now_mono` returns a monotonic elapsed-time reading (`Duration`) and is not affected by wall-clock changes.
12. `sleep` consumes `Duration` in nanoseconds and returns `InvalidArgument` for negative durations.

## `Bosatsu/IO/Std` additions
1. Add `read_line: Prog[IOError, Option[String]]`.
2. Add `read_all_stdin: Prog[IOError, String]`.
3. `read_line` blocks until newline (`\n`) or EOF; returns `None` only when EOF is reached before reading any characters.
4. `read_line` strips trailing line ending (`\n` or `\r\n`) from returned text.
5. `read_all_stdin` reads until EOF (stdin stream closed), not merely until "nothing currently available".
6. Both functions are implemented in `Bosatsu/IO/Std` via repeated `Bosatsu/IO/Core::read_text(stdin, chunk_size)`.

## Java/Python/C implementation plan

### JVM (`Predef.scala`)
1. Extend `jvmExternals` with all `Bosatsu/IO/Core` symbols.
2. Add internal runtime objects:
   1. `HandleValue` (`stdin`, `stdout`, `stderr`, file, child pipe read/write)
   2. `ProcessValue` (wrap `java.lang.Process`, cached exit status)
3. Implement IO effects using `prog_effect` dispatch:
   1. Files: `java.nio.file.Files` + `java.io` streams
   2. Process: `ProcessBuilder` + redirected streams
   3. Env: `System.getenv`
   4. Path conversion: convert `Path.to_String` to `java.nio.file.Path` at each filesystem call
   5. Wall clock: `java.time.Instant.now()` converted to epoch nanoseconds
   6. Monotonic clock: `System.nanoTime()`
   7. Sleep: `Thread.sleep`/`LockSupport.parkNanos` from duration nanoseconds
4. Keep existing `ProgRunResult` testability by preserving capture-mode behavior for stdio when run under evaluator tests.

### Python (`ProgExt.py` + externals map)
1. Add runtime classes for `Handle` and `Process` wrapper values (`Path`/`Instant`/`Duration` remain Bosatsu struct values).
2. Add new `ProgExt` constructors returning `effect(...)` thunks for each primitive.
3. Implement using stdlib:
   1. Files/dirs/stat/remove/rename/path ops: `os`, `pathlib`, `shutil`
   2. Spawn/wait/pipes: `subprocess.Popen`
   3. Wall clock: `time.time_ns`
   4. Monotonic clock: `time.monotonic_ns`
   5. Sleep: `time.sleep(nanos / 1_000_000_000.0)`
   6. Env: `os.environ.get`
4. Update `test_workspace/Prog.bosatsu_externals` for `Bosatsu/IO/Core` symbol remapping.

### C (`c_runtime`)
1. Add `bosatsu_ext_Bosatsu_l_IO_l_Core.c/.h` and include in build/install targets.
2. Represent opaque runtime handles/processes as `alloc_external(...)` payloads (`Path`/`Instant`/`Duration` stay Bosatsu struct values).
3. Implement using POSIX APIs first (same approach as current IO/Error errno mapping):
   1. Files/dirs/stat/remove/rename/path ops: `open/fopen`, `readdir`, `lstat`, `mkdir`, `unlink/rmdir`, `rename`
   2. Spawn/wait/pipes: `fork/execvp/pipe/waitpid` (or `posix_spawn` variant)
   3. Wall clock: `clock_gettime(CLOCK_REALTIME, ...)`
   4. Monotonic clock: `clock_gettime(CLOCK_MONOTONIC, ...)`
   5. Sleep: `nanosleep`
   6. Env: `getenv`
4. Reuse and extend current errno-to-`IOError` mapper (`c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Std.c`) into shared helpers for `IO/Core`.

## Compatibility and migration
1. `Bosatsu/IO/Std` remains source-compatible.
2. Existing `print`/`println` behavior is preserved.
3. New code can choose either high-level `IO/Std` or low-level `IO/Core`.
4. `argv` remains available at the `Main(args -> ...)` boundary.
5. Program termination should flow through `Main` return codes rather than `IO/Core::exit`.
6. This design assumes the post-#1748 `Bosatsu/Prog` shape (`Prog[err, res]` with no reader env).

## Tests and conformance
1. Add package-level evaluator tests in `core/src/test/scala/dev/bosatsu/EvaluationTest.scala` for each primitive family.
2. Add Python transpile + execute tests using `Prog.bosatsu_externals` mappings.
3. Add C transpile + runtime tests linked against `c_runtime` for file/process/time operations.
4. Add cross-runtime parity tests for:
   1. EOF semantics
   2. error tag parity (`IOError` variants)
   3. spawn stdio pipe behavior
   4. wall/monotonic clock semantics
   5. sleep minimum behavior

## Rollout
1. Land this design doc.
2. Open issue: `add minimal set of IO functions for basic tools`.
3. Implement `Bosatsu/IO/Core` + runtime bindings in JVM/Python/C.
4. Rebase `Bosatsu/IO/Std` on `IO/Core` wrappers and add `read_line` / `read_all_stdin`.
5. Regenerate core_alpha docs and release a new `core_alpha` version.
