# Prog is an ADT with the following values:
# Pure(a) => (0, a)
# Raise(e) => (1, e)
# FlatMap(p, f) => (2, p, f)
# Recover(p, f) => (3, p, f)
# ApplyFix(a, f) => (4, a, f)
# ReadEnv() => (5, )
# RemapEnv(p, f) => (6, p, f)

import errno as _errno
import os
import sys
from typing import Optional, Tuple, Union

def pure(a): return (0, a)
def raise_error(e): return (1, e)
def flat_map(p, f): return (2, p, f)
def recover(p, f): return (3, p, f)
def apply_fix(a, f): return (4, a, f)
read_env = (5,)
def remap_env(p, f): return (6, p, f) 
# this is a thunk we run
def effect(f): return (7, f)

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
        return (False, _ioerr(_IOERR_INVALID_ARGUMENT, "read_stdin_utf8_bytes argument"))
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

# p: Prog[List[String], String, Int]
def run(arg):
  # the stack ADT:
  done = (0,)
  def fmstep(fn, stack): return (1, fn, stack)
  def recstep(fn, stack): return (2, fn, stack)
  def restore(env, stack): return (3, env, stack)

  env = py_to_bosatsu_list(sys.argv[1:])
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
      elif stack_tag == 3:
        # restore
        env = stack[1]
        stack = stack[2]
    elif prog_tag == 7:
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
      elif stack_tag == 3:
        # restore
        env = stack[1]
        stack = stack[2]
    elif prog_tag == 3:
      # recover
      stack = recstep(arg[2], stack)
      arg = arg[1]
    elif prog_tag == 4:
      # apply_fix
      arg = step_fix(arg[1], arg[2])
    elif prog_tag == 5:
      # read_env
      arg = pure(env)
    elif prog_tag == 6:
      # remap_env
      stack = restore(env, stack)
      env = arg[2](env)
      arg = arg[1]
