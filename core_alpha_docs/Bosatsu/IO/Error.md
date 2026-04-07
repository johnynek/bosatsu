# `Bosatsu/IO/Error`

source code:
- [`test_workspace/Bosatsu/IO/Error.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/Bosatsu/IO/Error.bosatsu)

## Index

- Types: [`IOError`](#type-ioerror)
- Values: [`error_to_String`](#value-error-to-string)

## Types

<a id="type-ioerror"></a>

### `IOError`

```bosatsu
type IOError
```

#### Constructors

- `AccessDenied(context: String)`
- `AlreadyExists(context: String)`
- `BadFileDescriptor(context: String)`
- `BrokenPipe(context: String)`
- `CrossDeviceLink(context: String)`
- `Interrupted(context: String)`
- `InvalidArgument(context: String)`
- `InvalidUtf8(context: String)`
- `IsDirectory(context: String)`
- `NameTooLong(context: String)`
- `NoSpace(context: String)`
- `NotDirectory(context: String)`
- `NotEmpty(context: String)`
- `NotFound(context: String)`
- `Other(context: String, code: Int, message: String)`
- `QuotaExceeded(context: String)`
- `ReadOnlyFileSystem(context: String)`
- `TimedOut(context: String)`
- `TooManyOpenFiles(context: String)`
- `Unsupported(context: String)`
- `WouldBlock(context: String)`

## Values

<a id="value-error-to-string"></a>

### `error_to_String`

references: [`IOError`](#type-ioerror), [`String`](../Predef.md#type-string)

```bosatsu
def error_to_String(err: IOError) -> String
```