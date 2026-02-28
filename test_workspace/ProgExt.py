# Prog is an ADT with the following values:
# Pure(a) => (0, a)
# Raise(e) => (1, e)
# FlatMap(p, f) => (2, p, f)
# Recover(p, f) => (3, p, f)
# ApplyFix(a, f) => (4, a, f)
# Effect(f) => (5, f)

import errno as _errno
import os
import shutil
import stat as _stat
import subprocess
import sys
import tempfile
import time
from typing import Optional, Tuple, Union

def pure(a): return (0, a)
def raise_error(e): return (1, e)
def flat_map(p, f): return (2, p, f)
def recover(p, f): return (3, p, f)
def apply_fix(a, f): return (4, a, f)
# this is a thunk we run
def effect(f): return (5, f)

_pure_unit = pure(())

_IOERR_NOT_FOUND = 0
_IOERR_ACCESS_DENIED = 1
_IOERR_ALREADY_EXISTS = 2
_IOERR_NOT_DIRECTORY = 3
_IOERR_IS_DIRECTORY = 4
_IOERR_NOT_EMPTY = 5
_IOERR_TOO_MANY_OPEN_FILES = 6
_IOERR_READ_ONLY_FILESYSTEM = 7
_IOERR_CROSS_DEVICE_LINK = 8
_IOERR_NO_SPACE = 9
_IOERR_QUOTA_EXCEEDED = 10
_IOERR_NAME_TOO_LONG = 11
_IOERR_INVALID_ARGUMENT = 12
_IOERR_INVALID_UTF8 = 13
_IOERR_BAD_FILE_DESCRIPTOR = 14
_IOERR_INTERRUPTED = 15
_IOERR_WOULD_BLOCK = 16
_IOERR_TIMED_OUT = 17
_IOERR_BROKEN_PIPE = 18
_IOERR_UNSUPPORTED = 19
_IOERR_OTHER = 20

def _ioerr(tag: int, context: str):
    return (tag, context)

def _ioerror_other(context: str, code: int, message: str):
    return (_IOERR_OTHER, context, code, message)

def _ioerror_from_errno(err: Optional[int], context: str):
    if err is None:
        return _ioerror_other(context, 0, "unknown error")
    if err == 0:
        return _ioerror_other(context, 0, "unknown error")
    if err == _errno.ENOENT:
        return _ioerr(_IOERR_NOT_FOUND, context)
    if err == _errno.EACCES:
        return _ioerr(_IOERR_ACCESS_DENIED, context)
    if err == _errno.EEXIST:
        return _ioerr(_IOERR_ALREADY_EXISTS, context)
    if err == _errno.ENOTDIR:
        return _ioerr(_IOERR_NOT_DIRECTORY, context)
    if err == _errno.EISDIR:
        return _ioerr(_IOERR_IS_DIRECTORY, context)
    if err == _errno.ENOTEMPTY:
        return _ioerr(_IOERR_NOT_EMPTY, context)
    if err == _errno.EMFILE:
        return _ioerr(_IOERR_TOO_MANY_OPEN_FILES, context)
    if err == _errno.EROFS:
        return _ioerr(_IOERR_READ_ONLY_FILESYSTEM, context)
    if err == _errno.EXDEV:
        return _ioerr(_IOERR_CROSS_DEVICE_LINK, context)
    if err == _errno.ENOSPC:
        return _ioerr(_IOERR_NO_SPACE, context)
    if hasattr(_errno, "EDQUOT") and err == _errno.EDQUOT:
        return _ioerr(_IOERR_QUOTA_EXCEEDED, context)
    if err == _errno.ENAMETOOLONG:
        return _ioerr(_IOERR_NAME_TOO_LONG, context)
    if err == _errno.EINVAL:
        return _ioerr(_IOERR_INVALID_ARGUMENT, context)
    if err == _errno.EILSEQ:
        return _ioerr(_IOERR_INVALID_UTF8, context)
    if err == _errno.EBADF:
        return _ioerr(_IOERR_BAD_FILE_DESCRIPTOR, context)
    if err == _errno.EINTR:
        return _ioerr(_IOERR_INTERRUPTED, context)
    if err == _errno.EAGAIN:
        return _ioerr(_IOERR_WOULD_BLOCK, context)
    if hasattr(_errno, "EWOULDBLOCK") and err == _errno.EWOULDBLOCK:
        return _ioerr(_IOERR_WOULD_BLOCK, context)
    if err == _errno.ETIMEDOUT:
        return _ioerr(_IOERR_TIMED_OUT, context)
    if err == _errno.EPIPE:
        return _ioerr(_IOERR_BROKEN_PIPE, context)
    if hasattr(_errno, "EOPNOTSUPP") and err == _errno.EOPNOTSUPP:
        return _ioerr(_IOERR_UNSUPPORTED, context)
    if hasattr(_errno, "ENOTSUP") and err == _errno.ENOTSUP:
        return _ioerr(_IOERR_UNSUPPORTED, context)
    return _ioerror_other(context, err, os.strerror(err))


_none = (0,)
def _some(value): return (1, value)

class _BosatsuBytes:
    __slots__ = ("data", "offset", "length")

    def __init__(self, data, offset: int = 0, length: Optional[int] = None):
        if isinstance(data, memoryview):
            data = data.tobytes()
        elif isinstance(data, bytearray):
            data = bytes(data)
        elif not isinstance(data, bytes):
            data = bytes(data)

        if length is None:
            length = len(data) - int(offset)

        offset_i = int(offset)
        length_i = int(length)
        if offset_i < 0 or length_i < 0 or (offset_i + length_i) > len(data):
            raise ValueError(f"invalid Bytes view ({offset_i}, {length_i}) for data length {len(data)}")

        self.data = data
        self.offset = offset_i
        self.length = length_i

def _as_bosatsu_bytes(value):
    if isinstance(value, _BosatsuBytes):
        return value
    return None

def _bytes_view_slice(value: _BosatsuBytes):
    start = value.offset
    end = value.offset + value.length
    return value.data[start:end]

def _normalize_byte_int(value: int) -> int:
    return int(value) & 0xff

def _as_optional_int(value):
    if isinstance(value, tuple) and len(value) >= 1:
        tag = value[0]
        if tag == 0:
            return (True, None)
        if tag == 1 and len(value) >= 2:
            try:
                return (True, int(value[1]))
            except Exception:
                return (False, None)
    return (False, None)

def _as_optional_path(value):
    if isinstance(value, tuple) and len(value) >= 1:
        tag = value[0]
        if tag == 0:
            return (True, None)
        if tag == 1 and len(value) >= 2:
            try:
                return (True, _to_path_string(value[1]))
            except ValueError:
                return (False, None)
    return (False, None)

def _normalize_temp_prefix(prefix: str) -> str:
    base = prefix if len(prefix) > 0 else "tmp"
    if len(base) >= 3:
        return base
    return base + ("_" * (3 - len(base)))

def _is_valid_temp_name_part(part: str) -> bool:
    for ch in part:
        if ch == "/" or ch == "\\":
            return False
        if ord(ch) < 32:
            return False
    return True

def _array_to_pylist(value):
    if isinstance(value, list):
        return value
    if isinstance(value, tuple):
        if len(value) == 3 and isinstance(value[0], (list, tuple)):
            data = value[0]
            offset = int(value[1])
            length = int(value[2])
            return list(data[offset:offset + length])
        return list(value)
    if hasattr(value, "data") and hasattr(value, "offset") and hasattr(value, "len"):
        return list(value.data[value.offset:value.offset + value.len])
    try:
        return list(value)
    except TypeError as exc:
        raise ValueError(f"invalid Array value: {value!r}") from exc

class _CoreHandle:
    __slots__ = ("stream", "readable", "writable", "closeable", "closed")

    def __init__(self, stream, readable: bool, writable: bool, closeable: bool):
        self.stream = stream
        self.readable = readable
        self.writable = writable
        self.closeable = closeable
        self.closed = False

class _CoreProcess:
    __slots__ = ("process", "exit_code")

    def __init__(self, process):
        self.process = process
        self.exit_code = None

def _invalid_argument(context: str):
    return _ioerr(_IOERR_INVALID_ARGUMENT, context)

def _bad_file_descriptor(context: str):
    return _ioerr(_IOERR_BAD_FILE_DESCRIPTOR, context)

def _unsupported(context: str):
    return _ioerr(_IOERR_UNSUPPORTED, context)

def _to_path_string(path_value):
    if isinstance(path_value, str):
        return path_value
    if isinstance(path_value, tuple) and len(path_value) >= 1 and isinstance(path_value[0], str):
        # Backward-compatible shape; current transpiler represents struct-1 as identity.
        return path_value[0]
    raise ValueError(f"invalid Path value: {path_value!r}")

def _normalize_path(path: str) -> str:
    return path.replace("\\", "/")

def _to_duration_nanos(duration_value):
    if isinstance(duration_value, int):
        return int(duration_value)
    if isinstance(duration_value, tuple) and len(duration_value) >= 1:
        # Backward-compatible shape; current transpiler represents struct-1 as identity.
        return int(duration_value[0])
    raise ValueError(f"invalid Duration value: {duration_value!r}")

def _to_bool(v) -> bool:
    if v is True or v is False:
        return bool(v)
    if isinstance(v, tuple) and len(v) >= 1:
        if v[0] == 1:
            return True
        if v[0] == 0:
            return False
    return bool(v)

def _as_handle(value):
    if isinstance(value, _CoreHandle):
        return value
    return None

def _open_mode_tag(mode):
    if isinstance(mode, int):
        return mode
    if isinstance(mode, tuple) and len(mode) >= 1:
        return mode[0]
    raise ValueError(f"invalid OpenMode value: {mode!r}")

def _open_mode_name(mode_tag: int) -> str:
    if mode_tag == 0:
        return "Read"
    if mode_tag == 1:
        return "WriteTruncate"
    if mode_tag == 2:
        return "Append"
    if mode_tag == 3:
        return "CreateNew"
    return f"Unknown({mode_tag})"

def _string_preview(value) -> str:
    if isinstance(value, str):
        return value
    return "<invalid String>"

def _resolved_temp_dir_preview(dir_path: Optional[str]) -> str:
    if dir_path is not None and len(dir_path) > 0:
        return dir_path
    return tempfile.gettempdir()

def _stdio_tag(stdio):
    if isinstance(stdio, tuple) and len(stdio) >= 1:
        return stdio[0]
    raise ValueError(f"invalid Stdio value: {stdio!r}")

def _bosatsu_list_to_pylist(lst):
    out = []
    current = lst
    while isinstance(current, tuple) and len(current) >= 1 and current[0] == 1:
        out.append(current[1])
        current = current[2]
    return out

def _kind_from_lstat(st):
    mode = st.st_mode
    if _stat.S_ISREG(mode):
        return (0,)  # File
    if _stat.S_ISDIR(mode):
        return (1,)  # Dir
    if _stat.S_ISLNK(mode):
        return (2,)  # Symlink
    return (3,)      # Other

# Bosatsu/IO/Bytes externals
empty_Bytes = _BosatsuBytes(b"", 0, 0)

def from_List_Int(ints):
    values = _bosatsu_list_to_pylist(ints)
    data = bytes((_normalize_byte_int(v) for v in values))
    if len(data) == 0:
        return empty_Bytes
    return _BosatsuBytes(data, 0, len(data))

def from_Array_Int(ints):
    values = _array_to_pylist(ints)
    data = bytes((_normalize_byte_int(v) for v in values))
    if len(data) == 0:
        return empty_Bytes
    return _BosatsuBytes(data, 0, len(data))

def to_List_Int(bytes_value):
    b = _as_bosatsu_bytes(bytes_value)
    if b is None:
        raise ValueError(f"invalid Bytes value: {bytes_value!r}")
    return py_to_bosatsu_list(list(_bytes_view_slice(b)))

def to_Array_Int(bytes_value):
    b = _as_bosatsu_bytes(bytes_value)
    if b is None:
        raise ValueError(f"invalid Bytes value: {bytes_value!r}")
    view = _bytes_view_slice(b)
    return (tuple(view), 0, len(view))

def size_Bytes(bytes_value):
    b = _as_bosatsu_bytes(bytes_value)
    if b is None:
        raise ValueError(f"invalid Bytes value: {bytes_value!r}")
    return int(b.length)

def get_map_Bytes(bytes_value, idx, default, fn):
    b = _as_bosatsu_bytes(bytes_value)
    if b is None:
        raise ValueError(f"invalid Bytes value: {bytes_value!r}")
    i = int(idx)
    if 0 <= i < b.length:
        return fn(int(b.data[b.offset + i]))
    return default(())

def get_or_Bytes(bytes_value, idx, default):
    b = _as_bosatsu_bytes(bytes_value)
    if b is None:
        raise ValueError(f"invalid Bytes value: {bytes_value!r}")
    i = int(idx)
    if 0 <= i < b.length:
        return int(b.data[b.offset + i])
    return default(())

def foldl_Bytes(bytes_value, init, fn):
    b = _as_bosatsu_bytes(bytes_value)
    if b is None:
        raise ValueError(f"invalid Bytes value: {bytes_value!r}")
    acc = init
    start = b.offset
    end = b.offset + b.length
    idx = start
    while idx < end:
        acc = fn(acc, int(b.data[idx]))
        idx += 1
    return acc

def concat_all_Bytes(chunks):
    py_chunks = _bosatsu_list_to_pylist(chunks)
    if len(py_chunks) == 0:
        return empty_Bytes
    parts = []
    total = 0
    for chunk in py_chunks:
        b = _as_bosatsu_bytes(chunk)
        if b is None:
            raise ValueError(f"invalid Bytes chunk: {chunk!r}")
        view = _bytes_view_slice(b)
        parts.append(view)
        total += len(view)
    if total == 0:
        return empty_Bytes
    return _BosatsuBytes(b"".join(parts), 0, total)

def slice_Bytes(bytes_value, start, end):
    b = _as_bosatsu_bytes(bytes_value)
    if b is None:
        raise ValueError(f"invalid Bytes value: {bytes_value!r}")
    size = b.length
    start_i = int(start)
    end_i = int(end)
    if start_i < 0:
        start_i = 0
    if end_i > size:
        end_i = size
    if start_i < 0 or end_i < 0 or start_i > end_i or end_i > size:
        return empty_Bytes
    slice_len = end_i - start_i
    if slice_len <= 0:
        return empty_Bytes
    return _BosatsuBytes(b.data, b.offset + start_i, slice_len)

def starts_with_Bytes(bytes_value, prefix_value):
    b = _as_bosatsu_bytes(bytes_value)
    p = _as_bosatsu_bytes(prefix_value)
    if b is None or p is None:
        raise ValueError("invalid Bytes value")
    if p.length > b.length:
        return False
    start = b.offset
    end = b.offset + p.length
    p_start = p.offset
    p_end = p.offset + p.length
    return b.data[start:end] == p.data[p_start:p_end]

def ends_with_Bytes(bytes_value, suffix_value):
    b = _as_bosatsu_bytes(bytes_value)
    s = _as_bosatsu_bytes(suffix_value)
    if b is None or s is None:
        raise ValueError("invalid Bytes value")
    if s.length > b.length:
        return False
    start = b.offset + (b.length - s.length)
    end = b.offset + b.length
    s_start = s.offset
    s_end = s.offset + s.length
    return b.data[start:end] == s.data[s_start:s_end]

def find_Bytes(bytes_value, needle_value, start):
    b = _as_bosatsu_bytes(bytes_value)
    n = _as_bosatsu_bytes(needle_value)
    if b is None or n is None:
        raise ValueError("invalid Bytes value")

    start_i = int(start)
    if start_i < 0:
        start_i = 0
    if start_i > b.length:
        start_i = b.length

    if n.length == 0:
        return int(start_i)

    haystack_start = b.offset + start_i
    haystack_end = b.offset + b.length
    needle_bytes = n.data[n.offset:n.offset + n.length]
    found = b.data.find(needle_bytes, haystack_start, haystack_end)
    if found < 0:
        return -1
    return int(found - b.offset)

def utf8_bytes_from_String(str_value):
    data = str_value.encode("utf-8")
    if len(data) == 0:
        return empty_Bytes
    return _BosatsuBytes(data, 0, len(data))

def utf8_bytes_to_String(bytes_value):
    b = _as_bosatsu_bytes(bytes_value)
    if b is None:
        raise ValueError("invalid Bytes value")
    view = _bytes_view_slice(b)
    try:
        return _some(view.decode("utf-8"))
    except UnicodeDecodeError:
        return _none

def utf8_Char_at(bytes_value, idx):
    b = _as_bosatsu_bytes(bytes_value)
    if b is None:
        raise ValueError("invalid Bytes value")

    i = int(idx)
    if i < 0 or i >= b.length:
        return _none

    start = b.offset + i
    end = b.offset + b.length
    data = b.data
    b0 = int(data[start])

    if b0 <= 0x7F:
        codepoint = b0
    elif (b0 & 0xE0) == 0xC0:
        if start + 2 > end:
            return _none
        b1 = int(data[start + 1])
        if (b1 & 0xC0) != 0x80:
            return _none
        codepoint = ((b0 & 0x1F) << 6) | (b1 & 0x3F)
        if codepoint < 0x80:
            return _none
    elif (b0 & 0xF0) == 0xE0:
        if start + 3 > end:
            return _none
        b1 = int(data[start + 1])
        b2 = int(data[start + 2])
        if (b1 & 0xC0) != 0x80 or (b2 & 0xC0) != 0x80:
            return _none
        codepoint = ((b0 & 0x0F) << 12) | ((b1 & 0x3F) << 6) | (b2 & 0x3F)
        if codepoint < 0x800 or (0xD800 <= codepoint <= 0xDFFF):
            return _none
    elif (b0 & 0xF8) == 0xF0:
        if start + 4 > end:
            return _none
        b1 = int(data[start + 1])
        b2 = int(data[start + 2])
        b3 = int(data[start + 3])
        if ((b1 & 0xC0) != 0x80 or
                (b2 & 0xC0) != 0x80 or
                (b3 & 0xC0) != 0x80):
            return _none
        codepoint = ((b0 & 0x07) << 18) | ((b1 & 0x3F) << 12) | ((b2 & 0x3F) << 6) | (b3 & 0x3F)
        if codepoint < 0x10000 or codepoint > 0x10FFFF:
            return _none
    else:
        return _none

    return _some(chr(codepoint))

# Bosatsu/IO/Core externals
path_sep = os.sep
stdin_handle = _CoreHandle(sys.stdin, readable=True, writable=False, closeable=False)
stdout_handle = _CoreHandle(sys.stdout, readable=False, writable=True, closeable=False)
stderr_handle = _CoreHandle(sys.stderr, readable=False, writable=True, closeable=False)

def read_utf8(handle, max_chars):
    def fn():
        if max_chars <= 0:
            return raise_error(_invalid_argument(f"read_utf8 max_chars must be > 0, got {max_chars}"))
        h = _as_handle(handle)
        if h is None:
            return raise_error(_bad_file_descriptor("read_utf8 on non-handle value"))
        if h.closed:
            return raise_error(_bad_file_descriptor("reading closed handle"))
        if not h.readable:
            return raise_error(_bad_file_descriptor("reading from non-readable handle"))

        try:
            chunk = h.stream.read(max_chars)
        except OSError as exc:
            return raise_error(_ioerror_from_errno(exc.errno, "reading utf8"))

        if chunk is None or chunk == "":
            return pure(_none)

        if isinstance(chunk, bytes):
            try:
                chunk = chunk.decode("utf-8")
            except UnicodeDecodeError:
                return raise_error(_ioerr(_IOERR_INVALID_UTF8, "decoding bytes from handle"))
        elif not isinstance(chunk, str):
            chunk = str(chunk)

        if chunk == "":
            return pure(_none)
        return pure(_some(chunk))

    return effect(fn)

def write_utf8(handle, text):
    def fn():
        h = _as_handle(handle)
        if h is None:
            return raise_error(_bad_file_descriptor("write_utf8 on non-handle value"))
        if h.closed:
            return raise_error(_bad_file_descriptor("writing closed handle"))
        if not h.writable:
            return raise_error(_bad_file_descriptor("writing to non-writable handle"))
        try:
            try:
                h.stream.write(text)
            except TypeError:
                h.stream.write(text.encode("utf-8"))
            return _pure_unit
        except OSError as exc:
            return raise_error(_ioerror_from_errno(exc.errno, "writing utf8"))

    return effect(fn)

def _binary_reader_stream(handle: _CoreHandle):
    stream = handle.stream
    if hasattr(stream, "buffer"):
        stream = stream.buffer
    if hasattr(stream, "read"):
        return stream
    return None

def _binary_writer_stream(handle: _CoreHandle):
    stream = handle.stream
    if hasattr(stream, "buffer"):
        stream = stream.buffer
    if hasattr(stream, "write"):
        return stream
    return None

def _read_bytes_once(handle: _CoreHandle, max_bytes: int, context: str):
    stream = _binary_reader_stream(handle)
    if stream is None:
        return (False, _bad_file_descriptor("reading from non-binary handle"))
    try:
        chunk = stream.read(max_bytes)
    except OSError as exc:
        return (False, _ioerror_from_errno(exc.errno, context))

    if chunk is None:
        return (True, None)
    if isinstance(chunk, str):
        chunk = chunk.encode("utf-8")
    if not isinstance(chunk, (bytes, bytearray, memoryview)):
        chunk = bytes(chunk)

    data = bytes(chunk)
    if len(data) == 0:
        return (True, None)
    return (True, _BosatsuBytes(data, 0, len(data)))

def _write_bytes_all(handle: _CoreHandle, bytes_value: _BosatsuBytes, context: str):
    stream = _binary_writer_stream(handle)
    if stream is None:
        return (False, _bad_file_descriptor("writing to non-binary handle"))

    data = _bytes_view_slice(bytes_value)
    offset = 0
    total = len(data)
    while offset < total:
        try:
            wrote = stream.write(data[offset:])
        except OSError as exc:
            return (False, _ioerror_from_errno(exc.errno, context))

        if wrote is None:
            wrote = total - offset
        wrote_i = int(wrote)
        if wrote_i <= 0:
            return (False, _ioerr(_IOERR_BROKEN_PIPE, context))
        offset += wrote_i

    return (True, None)

def read_bytes(handle, max_bytes):
    def fn():
        max_bytes_i = int(max_bytes)
        if max_bytes_i <= 0:
            return raise_error(_invalid_argument(f"read_bytes max_bytes must be > 0, got {max_bytes_i}"))

        h = _as_handle(handle)
        if h is None:
            return raise_error(_bad_file_descriptor("read_bytes on non-handle value"))
        if h.closed:
            return raise_error(_bad_file_descriptor("reading closed handle"))
        if not h.readable:
            return raise_error(_bad_file_descriptor("reading from non-readable handle"))

        ok, result = _read_bytes_once(h, max_bytes_i, "reading bytes")
        if not ok:
            return raise_error(result)
        if result is None:
            return pure(_none)
        return pure(_some(result))

    return effect(fn)

def write_bytes(handle, bytes_value):
    def fn():
        h = _as_handle(handle)
        if h is None:
            return raise_error(_bad_file_descriptor("write_bytes on non-handle value"))
        if h.closed:
            return raise_error(_bad_file_descriptor("writing closed handle"))
        if not h.writable:
            return raise_error(_bad_file_descriptor("writing to non-writable handle"))

        b = _as_bosatsu_bytes(bytes_value)
        if b is None:
            return raise_error(_invalid_argument("write_bytes expected Bytes"))

        ok, err = _write_bytes_all(h, b, "writing bytes")
        if not ok:
            return raise_error(err)
        return _pure_unit

    return effect(fn)

def read_all_bytes(handle, chunk_size):
    def fn():
        chunk_size_i = int(chunk_size)
        if chunk_size_i <= 0:
            return raise_error(_invalid_argument(f"read_all_bytes chunk_size must be > 0, got {chunk_size_i}"))

        h = _as_handle(handle)
        if h is None:
            return raise_error(_bad_file_descriptor("read_all_bytes on non-handle value"))
        if h.closed:
            return raise_error(_bad_file_descriptor("reading closed handle"))
        if not h.readable:
            return raise_error(_bad_file_descriptor("reading from non-readable handle"))

        data = bytearray()
        while True:
            ok, result = _read_bytes_once(h, chunk_size_i, "reading bytes")
            if not ok:
                return raise_error(result)
            if result is None:
                break
            data.extend(_bytes_view_slice(result))

        if len(data) == 0:
            return pure(empty_Bytes)
        return pure(_BosatsuBytes(bytes(data), 0, len(data)))

    return effect(fn)

def copy_bytes(src, dst, chunk_size, max_total):
    def fn():
        chunk_size_i = int(chunk_size)
        if chunk_size_i <= 0:
            return raise_error(_invalid_argument(f"copy_bytes chunk_size must be > 0, got {chunk_size_i}"))

        ok_opt, max_total_i = _as_optional_int(max_total)
        if not ok_opt:
            return raise_error(_invalid_argument("copy_bytes max_total must be Option[Int]"))
        if max_total_i is not None and max_total_i < 0:
            return raise_error(_invalid_argument(f"copy_bytes max_total must be >= 0, got {max_total_i}"))
        if max_total_i == 0:
            return pure(0)

        src_h = _as_handle(src)
        if src_h is None:
            return raise_error(_bad_file_descriptor("copy_bytes source is not a handle"))
        if src_h.closed:
            return raise_error(_bad_file_descriptor("reading closed handle"))
        if not src_h.readable:
            return raise_error(_bad_file_descriptor("reading from non-readable handle"))

        dst_h = _as_handle(dst)
        if dst_h is None:
            return raise_error(_bad_file_descriptor("copy_bytes destination is not a handle"))
        if dst_h.closed:
            return raise_error(_bad_file_descriptor("writing closed handle"))
        if not dst_h.writable:
            return raise_error(_bad_file_descriptor("writing to non-writable handle"))

        copied = 0
        while True:
            if max_total_i is not None:
                remaining = max_total_i - copied
                if remaining <= 0:
                    break
                to_read = remaining if remaining < chunk_size_i else chunk_size_i
            else:
                to_read = chunk_size_i

            ok_read, chunk = _read_bytes_once(src_h, to_read, "reading bytes")
            if not ok_read:
                return raise_error(chunk)
            if chunk is None:
                break

            ok_write, err = _write_bytes_all(dst_h, chunk, "writing bytes")
            if not ok_write:
                return raise_error(err)

            copied += int(chunk.length)

        return pure(copied)

    return effect(fn)

def flush_handle(handle):
    def fn():
        h = _as_handle(handle)
        if h is None:
            return raise_error(_bad_file_descriptor("flush on non-handle value"))
        if h.closed:
            return raise_error(_bad_file_descriptor("flushing closed handle"))
        if not h.writable:
            return _pure_unit
        try:
            h.stream.flush()
            return _pure_unit
        except OSError as exc:
            return raise_error(_ioerror_from_errno(exc.errno, "flushing handle"))

    return effect(fn)

def close_handle(handle):
    def fn():
        h = _as_handle(handle)
        if h is None:
            return raise_error(_bad_file_descriptor("close on non-handle value"))
        if h.closed:
            return _pure_unit
        if not h.closeable:
            h.closed = True
            return _pure_unit
        try:
            h.stream.close()
            h.closed = True
            return _pure_unit
        except OSError as exc:
            return raise_error(_ioerror_from_errno(exc.errno, "closing handle"))

    return effect(fn)

def open_file(path, mode):
    def fn():
        try:
            path_s = _to_path_string(path)
            mode_tag = _open_mode_tag(mode)
        except ValueError as exc:
            return raise_error(_invalid_argument(str(exc)))

        mode_name = _open_mode_name(mode_tag)
        call_context = f"open_file(path={path_s}, mode={mode_name})"
        try:
            if mode_tag == 0:
                stream = open(path_s, "r", encoding="utf-8", newline="")
                return pure(_CoreHandle(stream, readable=True, writable=False, closeable=True))
            if mode_tag == 1:
                stream = open(path_s, "w", encoding="utf-8", newline="")
                return pure(_CoreHandle(stream, readable=False, writable=True, closeable=True))
            if mode_tag == 2:
                stream = open(path_s, "a", encoding="utf-8", newline="")
                return pure(_CoreHandle(stream, readable=False, writable=True, closeable=True))
            if mode_tag == 3:
                stream = open(path_s, "x", encoding="utf-8", newline="")
                return pure(_CoreHandle(stream, readable=False, writable=True, closeable=True))
            return raise_error(_invalid_argument(f"{call_context}: invalid OpenMode value"))
        except OSError as exc:
            return raise_error(_ioerror_from_errno(exc.errno, f"{call_context}: opening file failed"))

    return effect(fn)

def create_temp_file(dir_opt, prefix, suffix):
    def fn():
        prefix_preview = _string_preview(prefix)
        suffix_preview = _string_preview(suffix)
        ok_dir, dir_path = _as_optional_path(dir_opt)
        if not ok_dir:
            return raise_error(
                _invalid_argument(
                    f"create_temp_file(dir=<invalid Path option>, prefix={prefix_preview}, suffix={suffix_preview}): invalid temp file dir"
                )
            )

        try:
            prefix_s = str(prefix)
        except Exception:
            return raise_error(
                _invalid_argument(
                    f"create_temp_file(dir={_resolved_temp_dir_preview(dir_path)}, prefix=<invalid String>, suffix={suffix_preview}): invalid temp file prefix"
                )
            )
        try:
            suffix_s = str(suffix)
        except Exception:
            return raise_error(
                _invalid_argument(
                    f"create_temp_file(dir={_resolved_temp_dir_preview(dir_path)}, prefix={prefix_s}, suffix=<invalid String>): invalid temp file suffix"
                )
            )

        call_context = (
            f"create_temp_file(dir={_resolved_temp_dir_preview(dir_path)}, prefix={prefix_s}, suffix={suffix_s})"
        )
        if not _is_valid_temp_name_part(prefix_s):
            return raise_error(_invalid_argument(f"{call_context}: invalid temp file prefix"))
        if not _is_valid_temp_name_part(suffix_s):
            return raise_error(_invalid_argument(f"{call_context}: invalid temp file suffix"))

        try:
            fd, path_s = tempfile.mkstemp(
                suffix=suffix_s,
                prefix=_normalize_temp_prefix(prefix_s),
                dir=dir_path
            )
        except OSError as exc:
            return raise_error(_ioerror_from_errno(exc.errno, f"{call_context}: tempfile.mkstemp failed"))

        try:
            stream = os.fdopen(fd, "w", encoding="utf-8", newline="")
            handle = _CoreHandle(stream, readable=False, writable=True, closeable=True)
            return pure((_normalize_path(path_s), handle))
        except OSError as exc:
            try:
                os.close(fd)
            except OSError:
                pass
            try:
                os.unlink(path_s)
            except OSError:
                pass
            return raise_error(
                _ioerror_from_errno(
                    exc.errno,
                    f"{call_context}: os.fdopen(path={_normalize_path(path_s)}) failed"
                )
            )

    return effect(fn)

def create_temp_dir(dir_opt, prefix):
    def fn():
        prefix_preview = _string_preview(prefix)
        ok_dir, dir_path = _as_optional_path(dir_opt)
        if not ok_dir:
            return raise_error(
                _invalid_argument(
                    f"create_temp_dir(dir=<invalid Path option>, prefix={prefix_preview}): invalid temp dir"
                )
            )

        try:
            prefix_s = str(prefix)
        except Exception:
            return raise_error(
                _invalid_argument(
                    f"create_temp_dir(dir={_resolved_temp_dir_preview(dir_path)}, prefix=<invalid String>): invalid temp dir prefix"
                )
            )

        call_context = f"create_temp_dir(dir={_resolved_temp_dir_preview(dir_path)}, prefix={prefix_s})"
        if not _is_valid_temp_name_part(prefix_s):
            return raise_error(_invalid_argument(f"{call_context}: invalid temp dir prefix"))

        try:
            path_s = tempfile.mkdtemp(
                prefix=_normalize_temp_prefix(prefix_s),
                dir=dir_path
            )
            return pure(_normalize_path(path_s))
        except OSError as exc:
            return raise_error(_ioerror_from_errno(exc.errno, f"{call_context}: tempfile.mkdtemp failed"))

    return effect(fn)

def list_dir(path):
    def fn():
        try:
            path_s = _to_path_string(path)
        except ValueError as exc:
            return raise_error(_invalid_argument(str(exc)))

        try:
            children = []
            for name in os.listdir(path_s):
                child = _normalize_path(os.path.join(path_s, name))
                children.append(child)
            children.sort()
            return pure(py_to_bosatsu_list(children))
        except OSError as exc:
            return raise_error(_ioerror_from_errno(exc.errno, f"listing directory: {path_s}"))

    return effect(fn)

def stat_path(path):
    def fn():
        try:
            path_s = _to_path_string(path)
        except ValueError as exc:
            return raise_error(_invalid_argument(str(exc)))

        try:
            st = os.lstat(path_s)
        except FileNotFoundError:
            return pure(_none)
        except OSError as exc:
            return raise_error(_ioerror_from_errno(exc.errno, f"stating path: {path_s}"))

        kind = _kind_from_lstat(st)
        mtime_ns = int(getattr(st, "st_mtime_ns", int(st.st_mtime * 1_000_000_000)))
        file_stat = (kind, int(st.st_size), int(mtime_ns))
        return pure(_some(file_stat))

    return effect(fn)

def mkdir_path(path, recursive):
    def fn():
        try:
            path_s = _to_path_string(path)
        except ValueError as exc:
            return raise_error(_invalid_argument(str(exc)))

        try:
            if _to_bool(recursive):
                os.makedirs(path_s)
            else:
                os.mkdir(path_s)
            return _pure_unit
        except OSError as exc:
            return raise_error(_ioerror_from_errno(exc.errno, f"creating directory: {path_s}"))

    return effect(fn)

def remove_path(path, recursive):
    def fn():
        try:
            path_s = _to_path_string(path)
        except ValueError as exc:
            return raise_error(_invalid_argument(str(exc)))

        recursive_flag = _to_bool(recursive)
        try:
            if recursive_flag:
                if os.path.isdir(path_s) and not os.path.islink(path_s):
                    shutil.rmtree(path_s)
                else:
                    os.remove(path_s)
            else:
                if os.path.isdir(path_s) and not os.path.islink(path_s):
                    os.rmdir(path_s)
                else:
                    os.remove(path_s)
            return _pure_unit
        except OSError as exc:
            return raise_error(_ioerror_from_errno(exc.errno, f"removing path: {path_s}"))

    return effect(fn)

def rename_path(path_from, path_to):
    def fn():
        try:
            from_s = _to_path_string(path_from)
            to_s = _to_path_string(path_to)
        except ValueError as exc:
            return raise_error(_invalid_argument(str(exc)))

        try:
            os.rename(from_s, to_s)
            return _pure_unit
        except OSError as exc:
            return raise_error(_ioerror_from_errno(exc.errno, f"renaming path: {from_s} -> {to_s}"))

    return effect(fn)

def get_env(name):
    def fn():
        value = os.environ.get(name)
        if value is None:
            return pure(_none)
        return pure(_some(value))

    return effect(fn)

def _stdio_to_popen_arg(stdio, stream_name: str):
    tag = _stdio_tag(stdio)
    if tag == 0:  # Inherit
        return (None, None)
    if tag == 1:  # Pipe
        return (subprocess.PIPE, "pipe")
    if tag == 2:  # Null
        return (subprocess.DEVNULL, None)
    if tag == 3:  # UseHandle
        if len(stdio) < 2:
            raise ValueError(f"invalid UseHandle for {stream_name}")
        h = _as_handle(stdio[1])
        if h is None:
            raise ValueError(f"invalid handle for {stream_name}")
        if h.closed:
            raise ValueError(f"closed handle for {stream_name}")
        if stream_name == "stdin" and (not h.readable):
            raise ValueError("stdin handle must be readable")
        if stream_name != "stdin" and (not h.writable):
            raise ValueError(f"{stream_name} handle must be writable")
        return (h.stream, None)
    raise ValueError(f"unknown Stdio tag: {tag}")

def spawn_process(cmd, args, stdio):
    def fn():
        if not isinstance(stdio, tuple) or len(stdio) < 3:
            return raise_error(_invalid_argument("invalid StdioConfig value"))

        try:
            py_args = [str(a) for a in _bosatsu_list_to_pylist(args)]
            stdin_arg, stdin_mode = _stdio_to_popen_arg(stdio[0], "stdin")
            stdout_arg, stdout_mode = _stdio_to_popen_arg(stdio[1], "stdout")
            stderr_arg, stderr_mode = _stdio_to_popen_arg(stdio[2], "stderr")
        except ValueError as exc:
            return raise_error(_invalid_argument(str(exc)))

        try:
            proc = subprocess.Popen(
                [cmd, *py_args],
                stdin=stdin_arg,
                stdout=stdout_arg,
                stderr=stderr_arg,
                text=True,
                encoding="utf-8",
            )
        except OSError as exc:
            return raise_error(_ioerror_from_errno(exc.errno, f"spawning process: {cmd}"))

        process_value = _CoreProcess(proc)
        spawn_stdin = _none
        spawn_stdout = _none
        spawn_stderr = _none

        if stdin_mode == "pipe" and proc.stdin is not None:
            spawn_stdin = _some(_CoreHandle(proc.stdin, readable=False, writable=True, closeable=True))
        if stdout_mode == "pipe" and proc.stdout is not None:
            spawn_stdout = _some(_CoreHandle(proc.stdout, readable=True, writable=False, closeable=True))
        if stderr_mode == "pipe" and proc.stderr is not None:
            spawn_stderr = _some(_CoreHandle(proc.stderr, readable=True, writable=False, closeable=True))

        # SpawnResult(proc, stdin, stdout, stderr)
        return pure((process_value, spawn_stdin, spawn_stdout, spawn_stderr))

    return effect(fn)

def wait_process(proc_value):
    def fn():
        if not isinstance(proc_value, _CoreProcess):
            return raise_error(_invalid_argument("wait expects a process handle"))
        if proc_value.exit_code is not None:
            return pure(int(proc_value.exit_code))
        try:
            code = proc_value.process.wait()
            proc_value.exit_code = int(code)
            return pure(int(code))
        except OSError as exc:
            return raise_error(_ioerror_from_errno(exc.errno, "waiting on process"))

    return effect(fn)

def sleep_for(duration):
    def fn():
        try:
            nanos = _to_duration_nanos(duration)
        except ValueError as exc:
            return raise_error(_invalid_argument(str(exc)))
        if nanos < 0:
            return raise_error(_invalid_argument(f"sleep duration must be >= 0, got {nanos}"))
        try:
            time.sleep(nanos / 1_000_000_000.0)
            return _pure_unit
        except OSError as exc:
            return raise_error(_ioerror_from_errno(exc.errno, "sleep"))

    return effect(fn)

now_wall = effect(lambda: pure(int(time.time_ns())))
now_mono = effect(lambda: pure(int(time.monotonic_ns())))


def println(s):
  def fn():
    try:
      sys.stdout.write(s)
      sys.stdout.write("\n")
      sys.stdout.flush()
      return _pure_unit
    except OSError as exc:
      return raise_error(_ioerror_from_errno(exc.errno, "writing to stdout"))

  return effect(fn)

def print_stdout(s):
  def fn():
    try:
      sys.stdout.write(s)
      sys.stdout.flush()
      return _pure_unit
    except OSError as exc:
      return raise_error(_ioerror_from_errno(exc.errno, "writing to stdout"))

  return effect(fn)

def print_err(s):
  def fn():
    try:
      sys.stderr.write(s)
      sys.stderr.flush()
      return _pure_unit
    except OSError as exc:
      return raise_error(_ioerror_from_errno(exc.errno, "writing to stderr"))

  return effect(fn)

def print_errln(s):
  def fn():
    try:
      sys.stderr.write(s)
      sys.stderr.write("\n")
      sys.stderr.flush()
      return _pure_unit
    except OSError as exc:
      return raise_error(_ioerror_from_errno(exc.errno, "writing to stderr"))

  return effect(fn)

def _read_utf8_chunk(requested: int) -> Tuple[bool, Union[str, tuple]]:
    """
    Read from standard input (or another binary stream) and return the shortest
    valid UTF-8 string whose byte length is:

      - >= 1 (n=0 behaves like n=1)
      - <= requested + 4

    Behavior:
    - Tries to read up to `requested` bytes first.
    - If decoding fails because we cut a code point at the end, we read up to
      4 more bytes *one at a time* to complete the last character.
    - We never drop bytes once read; if we return a string, it encodes back to
      exactly the bytes we consumed from the stream.
    - Any error (I/O error, still invalid UTF-8 after up to 4 extra bytes)
      returns (False, IOError).
    - If EOF is reached before `requested` bytes but the bytes we did get form
      valid UTF-8, we return that string.
    - If we immediately see EOF, we return "" (the only way to get empty string).
    """

    if requested < 0:
        return (False, _ioerr(
            _IOERR_INVALID_ARGUMENT,
            f"read_stdin_utf8_bytes negative argument: {requested}"
        ))
    if requested == 0:
        # n=0 behaves like n=1 (there is no empty read command)
        requested = 1

    # Use the raw bytes interface to stdin
    stream = getattr(sys.stdin, "buffer", sys.stdin)

    buf = bytearray()

    # Helper: try to decode the current buffer as UTF-8
    def decode_buf() -> Optional[str]:
        try:
            return bytes(buf).decode("utf-8")
        except UnicodeDecodeError:
            return None

    # 1. Try to read up to `requested` bytes
    while len(buf) < requested:
        to_read = requested - len(buf)
        try:
            chunk = stream.read(to_read)
        except OSError as exc:
            return (False, _ioerror_from_errno(exc.errno, "reading from stdin"))

        if chunk is None:
            return (False, _ioerror_other("reading from stdin", 0, "unexpected None from read"))

        if not chunk:  # EOF
            break

        if not isinstance(chunk, (bytes, bytearray)):
            # Stream wasn't binary; this is a misuse, treat as error
            return (False, _ioerror_other("reading from stdin", 0, "stdin is not a binary stream"))

        buf.extend(chunk)

    # 2. First attempt to decode whatever we have
    s = decode_buf()
    if s is not None:
        # Covers:
        #   - Got exactly `requested` bytes and they’re valid
        #   - Got fewer than `requested` bytes due to EOF but still valid
        #   - requested == 0 -> empty buffer -> ""
        return (True, s)

    # 3. Decode failed.
    # If we didn't even reach `requested`, we hit EOF with invalid/truncated
    # UTF-8; nothing more to read, so treat as error.
    if len(buf) < requested:
        return (False, _ioerr(_IOERR_INVALID_UTF8, "decoding bytes from stdin"))

    # 4. We have at least `requested` bytes, but they don't decode yet.
    #    Assume we may have cut the last code point. Read up to 4 more bytes,
    #    *one at a time*, retrying decode each time. This guarantees:
    #      - We never exceed requested + 4 bytes.
    #      - We stop as soon as we have the shortest valid UTF-8 prefix.
    extra = 0
    while extra < 4:
        try:
            chunk = stream.read(1)
        except OSError as exc:
            return (False, _ioerror_from_errno(exc.errno, "reading from stdin"))

        if chunk is None:
            return (False, _ioerror_other("reading from stdin", 0, "unexpected None from read"))

        if not chunk:  # EOF
            break

        if not isinstance(chunk, (bytes, bytearray)):
            return (False, _ioerror_other("reading from stdin", 0, "stdin is not a binary stream"))

        buf.extend(chunk)
        extra += 1

        s = decode_buf()
        if s is not None:
            return (True, s)

    # Still invalid after up to 4 extra bytes → true UTF-8 error
    return (False, _ioerr(_IOERR_INVALID_UTF8, "decoding bytes from stdin"))

def read_stdin_utf8_bytes(cnt):
  def fn():
    ok, value = _read_utf8_chunk(cnt)
    if ok:
      return pure(value)
    else:
      return raise_error(value)

  return effect(fn)

def py_to_bosatsu_list(pylist):
  result = (0,)
  l = len(pylist)
  for idx in range(l):
    result = (1, pylist[l - idx - 1], result)
  return result

def step_fix(arg, fixfn):
  # this is just apply_fix(a, fixfn)
  fixed = lambda a: (4, a, fixfn)
  return fixfn(fixed)(arg)

def _prog_from_main(main):
  args = py_to_bosatsu_list(sys.argv[1:])
  if callable(main):
    return main(args)
  if isinstance(main, tuple) and len(main) > 0 and callable(main[0]):
    return main[0](args)
  return main

# main: List[String] -> Prog[String, Int]
def run(main):
  # the stack ADT:
  done = (0,)
  def fmstep(fn, stack): return (1, fn, stack)
  def recstep(fn, stack): return (2, fn, stack)

  arg = _prog_from_main(main)
  stack = done
  while True:
    prog_tag = arg[0]
    if prog_tag == 2:
      # flat_map (first because it is most common)
      stack = fmstep(arg[2], stack)
      arg = arg[1]
    elif prog_tag == 0:
      # pure
      item = arg[1]
      stack_tag = stack[0]
      if stack_tag == 0:
        #done, the result must be an int
        sys.exit(item)
      elif stack_tag == 1:
        #fmstep
        fn = stack[1]
        arg = fn(item)
        stack = stack[2]
      elif stack_tag == 2:
        # recstep, but this isn't an error
        stack = stack[2]
    elif prog_tag == 5:
      # effect
      arg = arg[1]()
    elif prog_tag == 1:
      # raise
      err = arg[1]
      stack_tag = stack[0]
      if stack_tag == 0:
        #done, the top error must be a string
        raise Exception(err) 
      elif stack_tag == 1:
        #fmstep, but this is an error, just pop
        stack = stack[2]
      elif stack_tag == 2:
        # recstep we can recover
        recfn = stack[1]
        arg = recfn(err)
        stack = stack[2]
    elif prog_tag == 3:
      # recover
      stack = recstep(arg[2], stack)
      arg = arg[1]
    elif prog_tag == 4:
      # apply_fix
      arg = step_fix(arg[1], arg[2])
    else:
      raise Exception(f"invalid Prog tag: {prog_tag}")
