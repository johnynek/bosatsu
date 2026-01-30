# Prog is an ADT with the following values:
# Pure(a) => (0, a)
# Raise(e) => (1, e)
# FlatMap(p, f) => (2, p, f)
# Recover(p, f) => (3, p, f)
# ApplyFix(a, f) => (4, a, f)
# ReadEnv() => (5, )
# RemapEnv(p, f) => (6, p, f)

import sys
from typing import Optional

def pure(a): return (0, a)

# Stubs for Bosatsu/IO and Bosatsu/UI - these are JS-only APIs
def io_stub(*args):
    raise NotImplementedError("Bosatsu/IO externals are not supported in Python")

def ui_stub(*args):
    raise NotImplementedError("Bosatsu/UI externals are not supported in Python")
def raise_error(e): return (1, e)
def flat_map(p, f): return (2, p, f)
def recover(p, f): return (3, p, f)
def apply_fix(a, f): return (4, a, f)
read_env = (5,)
def remap_env(p, f): return (6, p, f) 
# this is a thunk we run
def effect(f): return (7, f)

_pure_unit = pure(())

def println(s):
  def fn():
    print(s)
    return _pure_unit

  return effect(fn)

def print_stdout(s):
  def fn():
    sys.stdout.write(s)
    sys.stdout.flush()
    return _pure_unit

  return effect(fn)

def _read_utf8_chunk(requested: int) -> Optional[str]:
    """
    Read from standard input (or another binary stream) and return the shortest
    valid UTF-8 string whose byte length is:

      - >= 0
      - <= requested + 4

    Behavior:
    - Tries to read up to `requested` bytes first.
    - If decoding fails because we cut a code point at the end, we read up to
      4 more bytes *one at a time* to complete the last character.
    - We never drop bytes once read; if we return a string, it encodes back to
      exactly the bytes we consumed from the stream.
    - Any error (I/O error, still invalid UTF-8 after up to 4 extra bytes)
      returns None.
    - If EOF is reached before `requested` bytes but the bytes we did get form
      valid UTF-8, we return that string.
    - If we immediately see EOF, we return "".
    """

    if requested < 0:
        return None

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
        except OSError:
            return None

        if chunk is None:
            return None  # unexpected, treat as error

        if not chunk:  # EOF
            break

        if not isinstance(chunk, (bytes, bytearray)):
            # Stream wasn't binary; this is a misuse, treat as error
            return None

        buf.extend(chunk)

    # 2. First attempt to decode whatever we have
    s = decode_buf()
    if s is not None:
        # Covers:
        #   - Got exactly `requested` bytes and they’re valid
        #   - Got fewer than `requested` bytes due to EOF but still valid
        #   - requested == 0 -> empty buffer -> ""
        return s

    # 3. Decode failed.
    # If we didn't even reach `requested`, we hit EOF with invalid/truncated
    # UTF-8; nothing more to read, so treat as error.
    if len(buf) < requested:
        return None

    # 4. We have at least `requested` bytes, but they don't decode yet.
    #    Assume we may have cut the last code point. Read up to 4 more bytes,
    #    *one at a time*, retrying decode each time. This guarantees:
    #      - We never exceed requested + 4 bytes.
    #      - We stop as soon as we have the shortest valid UTF-8 prefix.
    extra = 0
    while extra < 4:
        try:
            chunk = stream.read(1)
        except OSError:
            return None

        if chunk is None:
            return None

        if not chunk:  # EOF
            break

        if not isinstance(chunk, (bytes, bytearray)):
            return None

        buf.extend(chunk)
        extra += 1

        s = decode_buf()
        if s is not None:
            return s

    # Still invalid after up to 4 extra bytes → true UTF-8 error
    return None

def read_stdin_utf8_bytes(cnt):
  def fn():
    string_or_none = _read_utf8_chunk(cnt)
    if string_or_none is None:
      return raise_error(())
    else:
      return pure(string_or_none)

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