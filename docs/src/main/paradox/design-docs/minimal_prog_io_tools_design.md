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
2. Keep `Bosatsu/Prog` as the single effect boundary; every effectful operation returns `Prog[env, IOError, a]`.
3. Keep `Bosatsu/IO/Error::IOError` as the shared error channel.
4. Keep `argv` as `Bosatsu/Prog::read_env` (do not duplicate it in `Bosatsu/IO/Core`).
5. Add a `Path` abstraction (not raw `String`) with basic conversion/combination helpers.
6. Keep `Bosatsu/IO/Std` source-compatible and add `read_line` and `read_all_stdin`.

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
Naming follows current stdlib style (`snake_case`), and `Bosatsu/IO/Core` exports externals directly (no duplicated `*_impl` wrappers).

```bosatsu
package Bosatsu/IO/Core

from Bosatsu/Prog import Prog
from Bosatsu/IO/Error import IOError

export (
  Path,
  path_from_string,
  path_to_string,
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
  exit,
  spawn,
  wait,
  now_wall,
  now_mono,
  sleep,
)

external struct Path

# Lift a string into a Path (platform validation/normalization point).
external path_from_string: String -> Option[Path]
external path_to_string: Path -> String
external path_join: (base: Path, child: String) -> Path
external path_parent: Path -> Option[Path]
external path_file_name: Path -> Option[String]

external struct Handle
external struct Process

# Opaque runtime values.
external struct Instant
external struct Duration

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

external def read_text[env](h: Handle, max_chars: Int) -> Prog[env, IOError, Option[String]]
external def write_text[env](h: Handle, s: String) -> Prog[env, IOError, Unit]
external def flush[env](h: Handle) -> Prog[env, IOError, Unit]
external def close[env](h: Handle) -> Prog[env, IOError, Unit]
external def open_file[env](path: Path, mode: OpenMode) -> Prog[env, IOError, Handle]
external def list_dir[env](path: Path) -> Prog[env, IOError, List[Path]]
external def stat[env](path: Path) -> Prog[env, IOError, Option[FileStat]]
external def mkdir[env](path: Path, recursive: Bool) -> Prog[env, IOError, Unit]
external def remove[env](path: Path, recursive: Bool) -> Prog[env, IOError, Unit]
external def rename[env](from: Path, to: Path) -> Prog[env, IOError, Unit]
external def get_env[env](name: String) -> Prog[env, IOError, Option[String]]
external def exit[env, a](code: Int) -> Prog[env, IOError, a]
external def spawn[env](cmd: String, args: List[String], stdio: StdioConfig) -> Prog[env, IOError, SpawnResult]
external def wait[env](p: Process) -> Prog[env, IOError, Int]
external def now_wall[env]() -> Prog[env, IOError, Instant]
external def now_mono[env]() -> Prog[env, IOError, Duration]
external def sleep[env](d: Duration) -> Prog[env, IOError, Unit]
```

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
14. `argv` -> `Bosatsu/Prog::read_env` (not duplicated in `Bosatsu/IO/Core`)
15. `getEnv` -> `get_env`
16. `exit` -> `exit`
17. `spawn` -> `spawn`
18. `wait` -> `wait`
19. `nowWall` -> `now_wall`
20. `nowMono` -> `now_mono`
21. `sleep` -> `sleep`

## Type mapping in Bosatsu / Predef / core_alpha
1. `String`, `Int`, `Bool`, `List[a]`, `Option[a]`, `Unit` map to existing `Bosatsu/Predef` builtins.
2. `Path` maps to new opaque `external struct Path` in `Bosatsu/IO/Core` with helper functions (`path_from_string`, `path_to_string`, `path_join`, `path_parent`, `path_file_name`).
3. `Int64 sizeBytes` maps to `Int` (Bosatsu `Int` is arbitrary precision; runtimes convert native 64-bit values).
4. `Instant` maps to new opaque `external struct Instant`.
5. `Duration` maps to new opaque `external struct Duration`.
6. `FileKind`, `FileStat`, `OpenMode`, `Stdio`, `StdioConfig`, `SpawnResult` are new `enum`/`struct` types in `Bosatsu/IO/Core`.
7. `Handle` and `Process` map to opaque `external struct` types in `Bosatsu/IO/Core`.
8. `IO[Never]` for `exit` maps to polymorphic result: `forall a. Prog[env, IOError, a]`.

## Runtime semantics (shared contract)
1. `read_text` returns `None` only for EOF; otherwise `Some(chunk)` where chunk length is `1..max_chars`.
2. `read_text(max_chars <= 0)` returns `InvalidArgument`.
3. `list_dir` returns child paths sorted by `path_to_string` for deterministic behavior.
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
1. Add `read_line[env]() -> Prog[env, IOError, Option[String]]`.
2. Add `read_all_stdin[env]() -> Prog[env, IOError, String]`.
3. `read_line` blocks until newline (`\n`) or EOF; returns `None` only when EOF is reached before reading any characters.
4. `read_line` strips trailing line ending (`\n` or `\r\n`) from returned text.
5. `read_all_stdin` reads until EOF (stdin stream closed), not merely until "nothing currently available".
6. Both functions are implemented in `Bosatsu/IO/Std` via repeated `Bosatsu/IO/Core::read_text(stdin, chunk_size)`.

## Java/Python/C implementation plan

### JVM (`Predef.scala`)
1. Extend `jvmExternals` with all `Bosatsu/IO/Core` symbols.
2. Add internal runtime objects:
   1. `PathValue`
   2. `HandleValue` (`stdin`, `stdout`, `stderr`, file, child pipe read/write)
   3. `ProcessValue` (wrap `java.lang.Process`, cached exit status)
   4. `InstantValue` and `DurationValue`
3. Implement IO effects using `prog_effect` dispatch:
   1. Files: `java.nio.file.Files` + `java.io` streams
   2. Process: `ProcessBuilder` + redirected streams
   3. Env: `System.getenv`
   4. Wall clock: `java.time.Instant.now()` converted to epoch nanoseconds
   5. Monotonic clock: `System.nanoTime()`
   6. Sleep: `Thread.sleep`/`LockSupport.parkNanos` from duration nanoseconds
4. Keep existing `ProgRunResult` testability by preserving capture-mode behavior for stdio when run under evaluator tests.

### Python (`ProgExt.py` + externals map)
1. Add runtime classes for `Path`, `Handle`, `Process`, `Instant`, `Duration` wrapper values.
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
2. Represent opaque runtime paths/handles/processes/time values as `alloc_external(...)` payloads.
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
4. `argv` remains available through `Bosatsu/Prog::read_env`.
5. No changes to `Bosatsu/Prog` type shape are required.

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
