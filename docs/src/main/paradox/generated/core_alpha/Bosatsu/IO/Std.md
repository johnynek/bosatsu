# `Bosatsu/IO/Std`

public dependencies: `Bosatsu/IO/Error`, `Bosatsu/Prog`

## Values

### `print`

```bosatsu
def print[a](arg1: String) -> Bosatsu/Prog::Prog[a, Bosatsu/IO/Error::IOError, ()]
```

### `print_err`

```bosatsu
def print_err[a](arg1: String) -> Bosatsu/Prog::Prog[a, Bosatsu/IO/Error::IOError, ()]
```

### `print_errln`

```bosatsu
def print_errln[a](arg1: String) -> Bosatsu/Prog::Prog[a, Bosatsu/IO/Error::IOError, ()]
```

### `println`

```bosatsu
def println[a](arg1: String) -> Bosatsu/Prog::Prog[a, Bosatsu/IO/Error::IOError, ()]
```

### `read_stdin_utf8_bytes`

```bosatsu
def read_stdin_utf8_bytes[a](arg1: Int) -> Bosatsu/Prog::Prog[a, Bosatsu/IO/Error::IOError, String]
```