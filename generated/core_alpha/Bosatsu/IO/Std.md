---
github.base_url=
---

# `Bosatsu/IO/Std`

source code:
- [`test_workspace/Bosatsu/IO/Std.bosatsu`](https://github.com/johnynek/bosatsu/blob/main/test_workspace/Bosatsu/IO/Std.bosatsu)

public dependencies: [`Bosatsu/IO/Error`](Error.html), [`Bosatsu/Prog`](../Prog.html)

## Index

- Values: [`print`](#value-print), [`print_err`](#value-print-err),
[`print_errln`](#value-print-errln), [`println`](#value-println),
[`read_all_stdin`](#value-read-all-stdin), [`read_line`](#value-read-line),
[`read_stdin_utf8_bytes`](#value-read-stdin-utf8-bytes), [`show_error`](#value-show-error)

## Values

<a id="value-print"></a>

### `print`

Write text to standard output.

references: [`Bosatsu/IO/Error::IOError`](Error.html#type-ioerror), [`Bosatsu/Prog::Prog`](../Prog.html#type-prog), [`String`](../Predef.html#type-string), [`Unit`](../Predef.html#type-unit)

```bosatsu
def print(str: String) -> Bosatsu/Prog::Prog[Bosatsu/IO/Error::IOError, ()]
```

<a id="value-print-err"></a>

### `print_err`

Write text to standard error.

references: [`Bosatsu/IO/Error::IOError`](Error.html#type-ioerror), [`Bosatsu/Prog::Prog`](../Prog.html#type-prog), [`String`](../Predef.html#type-string), [`Unit`](../Predef.html#type-unit)

```bosatsu
def print_err(str: String) -> Bosatsu/Prog::Prog[Bosatsu/IO/Error::IOError, ()]
```

<a id="value-print-errln"></a>

### `print_errln`

Write a string plus newline to standard error.

references: [`Bosatsu/IO/Error::IOError`](Error.html#type-ioerror), [`Bosatsu/Prog::Prog`](../Prog.html#type-prog), [`String`](../Predef.html#type-string), [`Unit`](../Predef.html#type-unit)

```bosatsu
def print_errln(str: String) -> Bosatsu/Prog::Prog[Bosatsu/IO/Error::IOError, ()]
```

<a id="value-println"></a>

### `println`

Write a string plus newline to standard output.

references: [`Bosatsu/IO/Error::IOError`](Error.html#type-ioerror), [`Bosatsu/Prog::Prog`](../Prog.html#type-prog), [`String`](../Predef.html#type-string), [`Unit`](../Predef.html#type-unit)

```bosatsu
def println(str: String) -> Bosatsu/Prog::Prog[Bosatsu/IO/Error::IOError, ()]
```

<a id="value-read-all-stdin"></a>

### `read_all_stdin`

Read all UTF-8 text from stdin until EOF.

references: [`Bosatsu/IO/Error::IOError`](Error.html#type-ioerror), [`Bosatsu/Prog::Prog`](../Prog.html#type-prog), [`String`](../Predef.html#type-string)

```bosatsu
read_all_stdin: Bosatsu/Prog::Prog[Bosatsu/IO/Error::IOError, String]
```

<a id="value-read-line"></a>

### `read_line`

Read a single line from stdin.
Returns None only if EOF is reached before reading any characters.

references: [`Bosatsu/IO/Error::IOError`](Error.html#type-ioerror), [`Bosatsu/Prog::Prog`](../Prog.html#type-prog), [`Option`](../Predef.html#type-option), [`String`](../Predef.html#type-string)

```bosatsu
read_line: Bosatsu/Prog::Prog[Bosatsu/IO/Error::IOError, Option[String]]
```

<a id="value-read-stdin-utf8-bytes"></a>

### `read_stdin_utf8_bytes`

Read UTF-8 text from standard input.
n=0 behaves like n=1; empty string indicates EOF.

references: [`Bosatsu/IO/Error::IOError`](Error.html#type-ioerror), [`Bosatsu/Prog::Prog`](../Prog.html#type-prog), [`Int`](../Predef.html#type-int), [`String`](../Predef.html#type-string)

```bosatsu
def read_stdin_utf8_bytes(n: Int) -> Bosatsu/Prog::Prog[Bosatsu/IO/Error::IOError, String]
```

<a id="value-show-error"></a>

### `show_error`

Render IOError to stderr and recover with a default value.

references: [`Bosatsu/IO/Error::IOError`](Error.html#type-ioerror), [`Bosatsu/Prog::Prog`](../Prog.html#type-prog)

```bosatsu
def show_error[a
](prog: Bosatsu/Prog::Prog[Bosatsu/IO/Error::IOError, a], default: a) -> forall b: *. Bosatsu/Prog::Prog[b, a]
```