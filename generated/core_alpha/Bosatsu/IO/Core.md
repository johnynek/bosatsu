---
github.base_url=
---

# `Bosatsu/IO/Core`

source code:
- [`test_workspace/Bosatsu/IO/Core.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/Bosatsu/IO/Core.bosatsu)

public dependencies: [`Bosatsu/IO/Bytes`](Bytes.html), [`Bosatsu/IO/Error`](Error.html), [`Bosatsu/Prog`](../Prog.html)

## Index

- Types: [`Duration`](#type-duration), [`FileKind`](#type-filekind), [`FileStat`](#type-filestat),
[`Handle`](#type-handle), [`Instant`](#type-instant), [`OpenMode`](#type-openmode),
[`Path`](#type-path), [`Process`](#type-process), [`SpawnResult`](#type-spawnresult),
[`Stdio`](#type-stdio), [`StdioConfig`](#type-stdioconfig), [`TempFile`](#type-tempfile)
- Values: [`close`](#value-close), [`copy_bytes`](#value-copy-bytes),
[`create_temp_dir`](#value-create-temp-dir), [`create_temp_file`](#value-create-temp-file),
[`duration_to_nanos`](#value-duration-to-nanos), [`flush`](#value-flush),
[`get_env`](#value-get-env), [`list_dir`](#value-list-dir), [`mkdir`](#value-mkdir),
[`now_mono`](#value-now-mono), [`now_wall`](#value-now-wall), [`open_file`](#value-open-file),
[`path_file_name`](#value-path-file-name), [`path_join`](#value-path-join),
[`path_parent`](#value-path-parent), [`path_sep`](#value-path-sep),
[`path_to_String`](#value-path-to-string), [`read_all_bytes`](#value-read-all-bytes),
[`read_bytes`](#value-read-bytes), [`read_utf8`](#value-read-utf8), [`remove`](#value-remove),
[`rename`](#value-rename), [`sleep`](#value-sleep), [`spawn`](#value-spawn), [`stat`](#value-stat),
[`stderr`](#value-stderr), [`stdin`](#value-stdin), [`stdout`](#value-stdout),
[`string_to_Path`](#value-string-to-path), [`wait`](#value-wait),
[`write_bytes`](#value-write-bytes), [`write_utf8`](#value-write-utf8)

## Types

<a id="type-duration"></a>

### `Duration`

```bosatsu
type Duration
```

<a id="type-filekind"></a>

### `FileKind`

```bosatsu
type FileKind
```

#### Constructors

- `Dir`
- `File`
- `Other`
- `Symlink`

<a id="type-filestat"></a>

### `FileStat`

```bosatsu
type FileStat
```

#### Constructors

- `FileStat(kind: FileKind, size_bytes: Int, mtime: Instant)`

<a id="type-handle"></a>

### `Handle`

```bosatsu
type Handle
```

<a id="type-instant"></a>

### `Instant`

Opaque outside this package because constructors are not exported.

```bosatsu
type Instant
```

<a id="type-openmode"></a>

### `OpenMode`

```bosatsu
type OpenMode
```

#### Constructors

- `Append`
- `CreateNew`
- `Read`
- `WriteTruncate`

<a id="type-path"></a>

### `Path`

Opaque outside this package because constructor is not exported.

```bosatsu
type Path
```

<a id="type-process"></a>

### `Process`

```bosatsu
type Process
```

<a id="type-spawnresult"></a>

### `SpawnResult`

```bosatsu
type SpawnResult
```

#### Constructors

- `SpawnResult(proc: Process, stdin: Option[Handle], stdout: Option[Handle], stderr: Option[Handle])`

<a id="type-stdio"></a>

### `Stdio`

```bosatsu
type Stdio
```

#### Constructors

- `Inherit`
- `Null`
- `Pipe`
- `UseHandle(handle: Handle)`

<a id="type-stdioconfig"></a>

### `StdioConfig`

```bosatsu
type StdioConfig
```

#### Constructors

- `StdioConfig(stdin: Stdio, stdout: Stdio, stderr: Stdio)`

<a id="type-tempfile"></a>

### `TempFile`

```bosatsu
type TempFile
```

#### Constructors

- `TempFile(path: Path, handle: Handle)`

## Values

<a id="value-close"></a>

### `close`

references: [`Bosatsu/IO/Error::IOError`](Error.html#type-ioerror), [`Bosatsu/Prog::Prog`](../Prog.html#type-prog), [`Handle`](#type-handle), [`Unit`](../Predef.html#type-unit)

```bosatsu
def close(h: Handle) -> Bosatsu/Prog::Prog[Bosatsu/IO/Error::IOError, ()]
```

<a id="value-copy-bytes"></a>

### `copy_bytes`

references: [`Bosatsu/IO/Error::IOError`](Error.html#type-ioerror), [`Bosatsu/Prog::Prog`](../Prog.html#type-prog), [`Handle`](#type-handle), [`Int`](../Predef.html#type-int), [`Option`](../Predef.html#type-option)

```bosatsu
def copy_bytes(src: Handle, dst: Handle, chunk_size: Int, max_total: Option[Int]) -> Bosatsu/Prog::Prog[Bosatsu/IO/Error::IOError, Int]
```

<a id="value-create-temp-dir"></a>

### `create_temp_dir`

references: [`Bosatsu/IO/Error::IOError`](Error.html#type-ioerror), [`Bosatsu/Prog::Prog`](../Prog.html#type-prog), [`Option`](../Predef.html#type-option), [`Path`](#type-path), [`String`](../Predef.html#type-string)

```bosatsu
def create_temp_dir(dir: Option[Path], prefix: String) -> Bosatsu/Prog::Prog[Bosatsu/IO/Error::IOError, Path]
```

<a id="value-create-temp-file"></a>

### `create_temp_file`

references: [`Bosatsu/IO/Error::IOError`](Error.html#type-ioerror), [`Bosatsu/Prog::Prog`](../Prog.html#type-prog), [`Option`](../Predef.html#type-option), [`Path`](#type-path), [`String`](../Predef.html#type-string), [`TempFile`](#type-tempfile)

```bosatsu
def create_temp_file(dir: Option[Path], prefix: String, suffix: String) -> Bosatsu/Prog::Prog[Bosatsu/IO/Error::IOError, TempFile]
```

<a id="value-duration-to-nanos"></a>

### `duration_to_nanos`

references: [`Duration`](#type-duration), [`Int`](../Predef.html#type-int)

```bosatsu
def duration_to_nanos(d: Duration) -> Int
```

<a id="value-flush"></a>

### `flush`

references: [`Bosatsu/IO/Error::IOError`](Error.html#type-ioerror), [`Bosatsu/Prog::Prog`](../Prog.html#type-prog), [`Handle`](#type-handle), [`Unit`](../Predef.html#type-unit)

```bosatsu
def flush(h: Handle) -> Bosatsu/Prog::Prog[Bosatsu/IO/Error::IOError, ()]
```

<a id="value-get-env"></a>

### `get_env`

references: [`Bosatsu/IO/Error::IOError`](Error.html#type-ioerror), [`Bosatsu/Prog::Prog`](../Prog.html#type-prog), [`Option`](../Predef.html#type-option), [`String`](../Predef.html#type-string)

```bosatsu
def get_env(name: String) -> Bosatsu/Prog::Prog[Bosatsu/IO/Error::IOError, Option[String]]
```

<a id="value-list-dir"></a>

### `list_dir`

references: [`Bosatsu/IO/Error::IOError`](Error.html#type-ioerror), [`Bosatsu/Prog::Prog`](../Prog.html#type-prog), [`List`](../Predef.html#type-list), [`Path`](#type-path)

```bosatsu
def list_dir(path: Path) -> Bosatsu/Prog::Prog[Bosatsu/IO/Error::IOError, List[Path]]
```

<a id="value-mkdir"></a>

### `mkdir`

references: [`Bool`](../Predef.html#type-bool), [`Bosatsu/IO/Error::IOError`](Error.html#type-ioerror), [`Bosatsu/Prog::Prog`](../Prog.html#type-prog), [`Path`](#type-path), [`Unit`](../Predef.html#type-unit)

```bosatsu
def mkdir(path: Path, recursive: Bool) -> Bosatsu/Prog::Prog[Bosatsu/IO/Error::IOError, ()]
```

<a id="value-now-mono"></a>

### `now_mono`

references: [`Bosatsu/IO/Error::IOError`](Error.html#type-ioerror), [`Bosatsu/Prog::Prog`](../Prog.html#type-prog), [`Duration`](#type-duration)

```bosatsu
now_mono: Bosatsu/Prog::Prog[Bosatsu/IO/Error::IOError, Duration]
```

<a id="value-now-wall"></a>

### `now_wall`

references: [`Bosatsu/IO/Error::IOError`](Error.html#type-ioerror), [`Bosatsu/Prog::Prog`](../Prog.html#type-prog), [`Instant`](#type-instant)

```bosatsu
now_wall: Bosatsu/Prog::Prog[Bosatsu/IO/Error::IOError, Instant]
```

<a id="value-open-file"></a>

### `open_file`

references: [`Bosatsu/IO/Error::IOError`](Error.html#type-ioerror), [`Bosatsu/Prog::Prog`](../Prog.html#type-prog), [`Handle`](#type-handle), [`OpenMode`](#type-openmode), [`Path`](#type-path)

```bosatsu
def open_file(path: Path, mode: OpenMode) -> Bosatsu/Prog::Prog[Bosatsu/IO/Error::IOError, Handle]
```

<a id="value-path-file-name"></a>

### `path_file_name`

references: [`Option`](../Predef.html#type-option), [`Path`](#type-path), [`String`](../Predef.html#type-string)

```bosatsu
def path_file_name(path: Path) -> Option[String]
```

<a id="value-path-join"></a>

### `path_join`

references: [`Path`](#type-path)

```bosatsu
def path_join(base: Path, child: Path) -> Path
```

<a id="value-path-parent"></a>

### `path_parent`

references: [`Option`](../Predef.html#type-option), [`Path`](#type-path)

```bosatsu
def path_parent(path: Path) -> Option[Path]
```

<a id="value-path-sep"></a>

### `path_sep`

references: [`String`](../Predef.html#type-string)

```bosatsu
path_sep: String
```

<a id="value-path-to-string"></a>

### `path_to_String`

references: [`Path`](#type-path), [`String`](../Predef.html#type-string)

```bosatsu
def path_to_String(path: Path) -> String
```

<a id="value-read-all-bytes"></a>

### `read_all_bytes`

references: [`Bosatsu/IO/Bytes::Bytes`](Bytes.html#type-bytes), [`Bosatsu/IO/Error::IOError`](Error.html#type-ioerror), [`Bosatsu/Prog::Prog`](../Prog.html#type-prog), [`Handle`](#type-handle), [`Int`](../Predef.html#type-int)

```bosatsu
def read_all_bytes(h: Handle, chunk_size: Int) -> Bosatsu/Prog::Prog[Bosatsu/IO/Error::IOError, Bosatsu/IO/Bytes::Bytes]
```

<a id="value-read-bytes"></a>

### `read_bytes`

references: [`Bosatsu/IO/Bytes::Bytes`](Bytes.html#type-bytes), [`Bosatsu/IO/Error::IOError`](Error.html#type-ioerror), [`Bosatsu/Prog::Prog`](../Prog.html#type-prog), [`Handle`](#type-handle), [`Int`](../Predef.html#type-int), [`Option`](../Predef.html#type-option)

```bosatsu
def read_bytes(h: Handle, max_bytes: Int) -> Bosatsu/Prog::Prog[Bosatsu/IO/Error::IOError, Option[Bosatsu/IO/Bytes::Bytes]]
```

<a id="value-read-utf8"></a>

### `read_utf8`

references: [`Bosatsu/IO/Error::IOError`](Error.html#type-ioerror), [`Bosatsu/Prog::Prog`](../Prog.html#type-prog), [`Handle`](#type-handle), [`Int`](../Predef.html#type-int), [`Option`](../Predef.html#type-option), [`String`](../Predef.html#type-string)

```bosatsu
def read_utf8(h: Handle, max_chars: Int) -> Bosatsu/Prog::Prog[Bosatsu/IO/Error::IOError, Option[String]]
```

<a id="value-remove"></a>

### `remove`

references: [`Bool`](../Predef.html#type-bool), [`Bosatsu/IO/Error::IOError`](Error.html#type-ioerror), [`Bosatsu/Prog::Prog`](../Prog.html#type-prog), [`Path`](#type-path), [`Unit`](../Predef.html#type-unit)

```bosatsu
def remove(path: Path, recursive: Bool) -> Bosatsu/Prog::Prog[Bosatsu/IO/Error::IOError, ()]
```

<a id="value-rename"></a>

### `rename`

references: [`Bosatsu/IO/Error::IOError`](Error.html#type-ioerror), [`Bosatsu/Prog::Prog`](../Prog.html#type-prog), [`Path`](#type-path), [`Unit`](../Predef.html#type-unit)

```bosatsu
def rename(from: Path, to: Path) -> Bosatsu/Prog::Prog[Bosatsu/IO/Error::IOError, ()]
```

<a id="value-sleep"></a>

### `sleep`

references: [`Bosatsu/IO/Error::IOError`](Error.html#type-ioerror), [`Bosatsu/Prog::Prog`](../Prog.html#type-prog), [`Duration`](#type-duration), [`Unit`](../Predef.html#type-unit)

```bosatsu
def sleep(d: Duration) -> Bosatsu/Prog::Prog[Bosatsu/IO/Error::IOError, ()]
```

<a id="value-spawn"></a>

### `spawn`

references: [`Bosatsu/IO/Error::IOError`](Error.html#type-ioerror), [`Bosatsu/Prog::Prog`](../Prog.html#type-prog), [`List`](../Predef.html#type-list), [`SpawnResult`](#type-spawnresult), [`StdioConfig`](#type-stdioconfig), [`String`](../Predef.html#type-string)

```bosatsu
def spawn(cmd: String, args: List[String], stdio: StdioConfig) -> Bosatsu/Prog::Prog[Bosatsu/IO/Error::IOError, SpawnResult]
```

<a id="value-stat"></a>

### `stat`

references: [`Bosatsu/IO/Error::IOError`](Error.html#type-ioerror), [`Bosatsu/Prog::Prog`](../Prog.html#type-prog), [`FileStat`](#type-filestat), [`Option`](../Predef.html#type-option), [`Path`](#type-path)

```bosatsu
def stat(path: Path) -> Bosatsu/Prog::Prog[Bosatsu/IO/Error::IOError, Option[FileStat]]
```

<a id="value-stderr"></a>

### `stderr`

references: [`Handle`](#type-handle)

```bosatsu
stderr: Handle
```

<a id="value-stdin"></a>

### `stdin`

references: [`Handle`](#type-handle)

```bosatsu
stdin: Handle
```

<a id="value-stdout"></a>

### `stdout`

references: [`Handle`](#type-handle)

```bosatsu
stdout: Handle
```

<a id="value-string-to-path"></a>

### `string_to_Path`

references: [`Option`](../Predef.html#type-option), [`Path`](#type-path), [`String`](../Predef.html#type-string)

```bosatsu
def string_to_Path(s: String) -> Option[Path]
```

<a id="value-wait"></a>

### `wait`

references: [`Bosatsu/IO/Error::IOError`](Error.html#type-ioerror), [`Bosatsu/Prog::Prog`](../Prog.html#type-prog), [`Int`](../Predef.html#type-int), [`Process`](#type-process)

```bosatsu
def wait(p: Process) -> Bosatsu/Prog::Prog[Bosatsu/IO/Error::IOError, Int]
```

<a id="value-write-bytes"></a>

### `write_bytes`

references: [`Bosatsu/IO/Bytes::Bytes`](Bytes.html#type-bytes), [`Bosatsu/IO/Error::IOError`](Error.html#type-ioerror), [`Bosatsu/Prog::Prog`](../Prog.html#type-prog), [`Handle`](#type-handle), [`Unit`](../Predef.html#type-unit)

```bosatsu
def write_bytes(h: Handle, bytes: Bosatsu/IO/Bytes::Bytes) -> Bosatsu/Prog::Prog[Bosatsu/IO/Error::IOError, ()]
```

<a id="value-write-utf8"></a>

### `write_utf8`

references: [`Bosatsu/IO/Error::IOError`](Error.html#type-ioerror), [`Bosatsu/Prog::Prog`](../Prog.html#type-prog), [`Handle`](#type-handle), [`String`](../Predef.html#type-string), [`Unit`](../Predef.html#type-unit)

```bosatsu
def write_utf8(h: Handle, s: String) -> Bosatsu/Prog::Prog[Bosatsu/IO/Error::IOError, ()]
```