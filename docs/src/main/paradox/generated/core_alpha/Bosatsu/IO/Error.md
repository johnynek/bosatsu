# `Bosatsu/IO/Error`

## Types

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

### `error_to_String`

```bosatsu
def error_to_String(arg1: IOError) -> String
```