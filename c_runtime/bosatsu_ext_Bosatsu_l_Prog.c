#include "bosatsu_runtime.h"

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

/*
# Prog is an ADT with the following values:
# Pure(a) => (0, a)
# Raise(e) => (1, e)
# FlatMap(p, f) => (2, p, f)
# Recover(p, f) => (3, p, f)
# ApplyFix(a, f) => (4, a, f)
# ReadEnv() => (5, )
# RemapEnv(p, f) => (6, p, f)
# Effect(arg: BValue, f: BValue => BValue) => (7, f)
*/

// Bosatsu/IOError enum variant indices (must match Bosatsu/IOError.bosatsu)
enum
{
  BSTS_IOERR_NotFound = 0,
  BSTS_IOERR_AccessDenied = 1,
  BSTS_IOERR_AlreadyExists = 2,
  BSTS_IOERR_NotDirectory = 3,
  BSTS_IOERR_IsDirectory = 4,
  BSTS_IOERR_NotEmpty = 5,
  BSTS_IOERR_TooManyOpenFiles = 6,
  BSTS_IOERR_ReadOnlyFileSystem = 7,
  BSTS_IOERR_CrossDeviceLink = 8,
  BSTS_IOERR_NoSpace = 9,
  BSTS_IOERR_QuotaExceeded = 10,
  BSTS_IOERR_NameTooLong = 11,
  BSTS_IOERR_InvalidArgument = 12,
  BSTS_IOERR_InvalidUtf8 = 13,
  BSTS_IOERR_BadFileDescriptor = 14,
  BSTS_IOERR_Interrupted = 15,
  BSTS_IOERR_WouldBlock = 16,
  BSTS_IOERR_TimedOut = 17,
  BSTS_IOERR_BrokenPipe = 18,
  BSTS_IOERR_Unsupported = 19,
  BSTS_IOERR_Other = 20
};

static BValue bsts_ioerror_other(int code, const char *msg)
{
  if (!msg)
  {
    msg = "unknown error";
  }
  size_t len = strlen(msg);
  BValue msgv = bsts_string_from_utf8_bytes_copy(len, (char *)msg);
  BValue codev = bsts_integer_from_int(code);
  return alloc_enum2(BSTS_IOERR_Other, codev, msgv);
}

static BValue bsts_ioerror_from_errno(int err)
{
  switch (err)
  {
#ifdef ENOENT
  case ENOENT:
    return alloc_enum0(BSTS_IOERR_NotFound);
#endif
#ifdef EACCES
  case EACCES:
    return alloc_enum0(BSTS_IOERR_AccessDenied);
#endif
#ifdef EEXIST
  case EEXIST:
    return alloc_enum0(BSTS_IOERR_AlreadyExists);
#endif
#ifdef ENOTDIR
  case ENOTDIR:
    return alloc_enum0(BSTS_IOERR_NotDirectory);
#endif
#ifdef EISDIR
  case EISDIR:
    return alloc_enum0(BSTS_IOERR_IsDirectory);
#endif
#ifdef ENOTEMPTY
  case ENOTEMPTY:
    return alloc_enum0(BSTS_IOERR_NotEmpty);
#endif
#ifdef EMFILE
  case EMFILE:
    return alloc_enum0(BSTS_IOERR_TooManyOpenFiles);
#endif
#ifdef EROFS
  case EROFS:
    return alloc_enum0(BSTS_IOERR_ReadOnlyFileSystem);
#endif
#ifdef EXDEV
  case EXDEV:
    return alloc_enum0(BSTS_IOERR_CrossDeviceLink);
#endif
#ifdef ENOSPC
  case ENOSPC:
    return alloc_enum0(BSTS_IOERR_NoSpace);
#endif
#ifdef EDQUOT
  case EDQUOT:
    return alloc_enum0(BSTS_IOERR_QuotaExceeded);
#endif
#ifdef ENAMETOOLONG
  case ENAMETOOLONG:
    return alloc_enum0(BSTS_IOERR_NameTooLong);
#endif
#ifdef EINVAL
  case EINVAL:
    return alloc_enum0(BSTS_IOERR_InvalidArgument);
#endif
#ifdef EILSEQ
  case EILSEQ:
    return alloc_enum0(BSTS_IOERR_InvalidUtf8);
#endif
#ifdef EBADF
  case EBADF:
    return alloc_enum0(BSTS_IOERR_BadFileDescriptor);
#endif
#ifdef EINTR
  case EINTR:
    return alloc_enum0(BSTS_IOERR_Interrupted);
#endif
#ifdef EAGAIN
  case EAGAIN:
    return alloc_enum0(BSTS_IOERR_WouldBlock);
#endif
#if defined(EWOULDBLOCK) && (!defined(EAGAIN) || (EWOULDBLOCK != EAGAIN))
  case EWOULDBLOCK:
    return alloc_enum0(BSTS_IOERR_WouldBlock);
#endif
#ifdef ETIMEDOUT
  case ETIMEDOUT:
    return alloc_enum0(BSTS_IOERR_TimedOut);
#endif
#ifdef EPIPE
  case EPIPE:
    return alloc_enum0(BSTS_IOERR_BrokenPipe);
#endif
#ifdef EOPNOTSUPP
  case EOPNOTSUPP:
    return alloc_enum0(BSTS_IOERR_Unsupported);
#endif
#ifdef ENOTSUP
  case ENOTSUP:
    return alloc_enum0(BSTS_IOERR_Unsupported);
#endif
  default:
    return bsts_ioerror_other(err, strerror(err));
  }
}

static BValue bsts_ioerror_from_errno_default(int err)
{
  if (err == 0)
  {
#ifdef EIO
    err = EIO;
#else
    return bsts_ioerror_other(0, "unknown error");
#endif
  }
  return bsts_ioerror_from_errno(err);
}

static inline BValue bsts_ioerror_invalid_utf8()
{
  return alloc_enum0(BSTS_IOERR_InvalidUtf8);
}

static inline BValue bsts_ioerror_invalid_argument()
{
  return alloc_enum0(BSTS_IOERR_InvalidArgument);
}


BValue ___bsts_g_Bosatsu_l_Prog_l_pure(BValue a)
{
  return alloc_enum1(0, a);
}

BValue ___bsts_g_Bosatsu_l_Prog_l_raise__error(BValue a)
{
  return alloc_enum1(1, a);
}

BValue ___bsts_g_Bosatsu_l_Prog_l_apply__fix(BValue a, BValue f)
{
  return alloc_enum2(4, a, f);
}

BValue ___bsts_g_Bosatsu_l_Prog_l_flat__map(BValue p, BValue f)
{
  return alloc_enum2(2, p, f);
}

BValue bsts_print_effect(BValue a)
{
  char *bytes = bsts_string_utf8_bytes(a);
  size_t len = bsts_string_utf8_len(a);
  if (len > 0)
  {
    errno = 0;
    size_t written = fwrite(bytes, 1, len, stdout);
    if (written < len)
    {
      return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
          bsts_ioerror_from_errno_default(errno));
    }
  }
  if (fflush(stdout) != 0)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno));
  }
  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_unit_value());
}

BValue bsts_println_effect(BValue a)
{
  char *bytes = bsts_string_utf8_bytes(a);
  size_t len = bsts_string_utf8_len(a);
  if (len > 0)
  {
    errno = 0;
    size_t written = fwrite(bytes, 1, len, stdout);
    if (written < len)
    {
      return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
          bsts_ioerror_from_errno_default(errno));
    }
  }
  errno = 0;
  size_t wrote_newline = fwrite("\n", 1, 1, stdout);
  if (wrote_newline != 1)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno));
  }
  if (fflush(stdout) != 0)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno));
  }
  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_unit_value());
}

BValue ___bsts_g_Bosatsu_l_Prog_l_print(BValue a)
{
  return alloc_enum2(7, a, alloc_boxed_pure_fn1(bsts_print_effect));
}

BValue ___bsts_g_Bosatsu_l_Prog_l_println(BValue a)
{
  return alloc_enum2(7, a, alloc_boxed_pure_fn1(bsts_println_effect));
}

BValue ___bsts_g_Bosatsu_l_Prog_l_read__env()
{
  return alloc_enum0(5);
}

/* Helper: check that the first len bytes form a sequence of whole UTF-8 code points. */
static _Bool
bsts_utf8_is_valid_prefix(const char *data, int len)
{
  int offset = 0;

  while (offset < len)
  {
    int cp_len = bsts_utf8_code_point_bytes(data, offset, len);
    if (cp_len <= 0)
    {
      return 0; // invalid or incomplete code point
    }
    offset += cp_len;
  }
  return offset == len;
}

BValue bsts_read_stdin_utf8_bytes_effect(BValue size)
{
  int requested = (int)bsts_integer_to_int32(size);
  if (requested < 0)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_invalid_argument());
  }
  if (requested == 0)
  {
    // n=0 behaves like n=1 (there is no empty read command)
    requested = 1;
  }

  /* We will never read more than requested + 4 bytes. */
  int capacity = requested + 4;
  char *buf = (char *)malloc((size_t)capacity + 1); // +1 for NUL
  if (!buf)
  {
    int err = 0;
#ifdef ENOMEM
    err = ENOMEM;
#endif
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_other(err, "out of memory"));
  }

  int len = 0;

  /*
   * First: try to read up to `requested` bytes in bulk.
   * We stop either when len == requested, or on EOF/error.
   */
  while (len < requested)
  {
    size_t to_read = (size_t)(requested - len);
    size_t n = fread(buf + len, 1, to_read, stdin);
    if (n == 0)
    {
      if (ferror(stdin))
      {
        free(buf);
        return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
            bsts_ioerror_from_errno_default(errno));
      }
      /* EOF */
      break;
    }
    len += (int)n;
    /* If fread short-reads without setting error, it must be EOF; loop will check again. */
    if (n < to_read && feof(stdin))
    {
      break;
    }
  }

  /* Case: EOF before reading any bytes at all -> empty string. */
  if (len == 0 && feof(stdin))
  {
    free(buf);
    return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_string_from_utf8_bytes_static(0, ""));
  }

  /*
   * If len < requested, we hit EOF after reading some bytes.
   * We return exactly what we have, but only if it’s a valid UTF-8 prefix.
   */
  if (len < requested)
  {
    if (!bsts_utf8_is_valid_prefix(buf, len))
    {
      free(buf);
      return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
          bsts_ioerror_invalid_utf8());
    }
    buf[len] = '\0';
    BValue result = ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_string_from_utf8_bytes_copy(len, buf));
    free(buf);
    return result;
  }

  /*
   * len == requested: we may have cut through a multi-byte code point.
   * Add up to 4 extra bytes (total capacity) until the prefix becomes valid
   * and we satisfy the UTF-8 boundary constraint.
   */
  while (1)
  {
    if (bsts_utf8_is_valid_prefix(buf, len))
    {
      /* We have a valid UTF-8 string and len >= requested. */
      break;
    }

    if (len >= capacity)
    {
      /* We've already added 4 extra bytes and still not valid -> invalid UTF-8. */
      free(buf);
      return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
          bsts_ioerror_invalid_utf8());
    }

    int ch = fgetc(stdin);
    if (ch == EOF)
    {
      if (ferror(stdin))
      {
        free(buf);
        return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
            bsts_ioerror_from_errno_default(errno));
      }
      /* EOF but current bytes don't end on a code point boundary -> invalid. */
      free(buf);
      return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
          bsts_ioerror_invalid_utf8());
    }

    buf[len++] = (char)ch;
  }

  buf[len] = '\0';
  BValue result = ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_string_from_utf8_bytes_copy(len, buf));
  free(buf);
  return result;
}

BValue ___bsts_g_Bosatsu_l_Prog_l_read__stdin__utf8__bytes(BValue a)
{
  return alloc_enum2(7, a, alloc_boxed_pure_fn1(bsts_read_stdin_utf8_bytes_effect));
}

BValue ___bsts_g_Bosatsu_l_Prog_l_recover(BValue p, BValue f)
{
  return alloc_enum2(3, p, f);
}

BValue ___bsts_g_Bosatsu_l_Prog_l_remap__env(BValue f, BValue p)
{
  return alloc_enum2(6, f, p);
}

BValue bsts_prog_step_fix_closure(BValue *slots, BValue a)
{
  return ___bsts_g_Bosatsu_l_Prog_l_apply__fix(a, slots[0]);
}

BValue bsts_prog_step_fix(BValue arg, BValue fixfn)
{
  /*
  # this is just apply_fix(a, fixfn)
  fixed = lambda a: (4, a, fixfn)
  return fixfn(fixed)(arg)
  */
  BValue captures[1] = {fixfn};
  BValue fixed = alloc_closure1(1, captures, bsts_prog_step_fix_closure);
  BValue ap1 = call_fn1(fixfn, fixed);
  return call_fn1(ap1, arg);
}

int bsts_Bosatsu_Prog_run_main(BValue prog, int argc, char **argv)
{
  BValue arg_list = alloc_enum0(0);
  for (int i = argc; i > 0; i--)
  {
    // TODO
    // we are assuming this null terminated string is utf8
    // but we should check that is is valid
    BValue arg = bsts_string_from_utf8_bytes_static_null_term(argv[i - 1]);
    arg_list = alloc_enum2(1, arg, arg_list);
  }
  /*
  # the stack ADT:
  done = (0,)
  def fmstep(fn, stack): return (1, fn, stack)
  def recstep(fn, stack): return (2, fn, stack)
  def restore(env, stack): return (3, env, stack)
  */
  BValue stack = alloc_enum0(0);
  BValue env = arg_list;
  BValue arg = prog;
  while (1)
  {
    switch (get_variant(arg))
    {
    case 0:
    {
      // pure
      BValue item = get_enum_index(arg, 0);
      _Bool search_stack = 1;
      while (search_stack)
      {
        switch (get_variant(stack))
        {
        case 0:
          // done, the result must be an int
          return (int)bsts_integer_to_int32(item);
        case 1:
        {
          // fmstep
          BValue fn = get_enum_index(stack, 0);
          stack = get_enum_index(stack, 1);
          arg = call_fn1(fn, item);
          search_stack = 0;
          break;
        }
        case 2:
          // recstep, but this isn't an error
          stack = get_enum_index(stack, 1);
          break;
        case 3:
          // restore
          env = get_enum_index(stack, 0);
          stack = get_enum_index(stack, 1);
          break;
        }
      }
      break;
    }
    case 1:
    {
      // raise
      BValue error = get_enum_index(arg, 0);
      _Bool search_stack = 1;
      while (search_stack)
      {
        switch (get_variant(stack))
        {
        case 0:
          // done, the result must be an int
          printf("unexpected top error");
          return 1;
        case 1:
          // fmstep, but we have an error
          stack = get_enum_index(stack, 2);
          break;
        case 2:
        {
          // recstep which will handle this error
          BValue fn = get_enum_index(stack, 0);
          stack = get_enum_index(stack, 1);
          arg = call_fn1(fn, error);
          search_stack = 0;
          break;
        }
        case 3:
          // restore
          env = get_enum_index(stack, 0);
          stack = get_enum_index(stack, 1);
          break;
        }
      }
      break;
    }
    case 2:
    {
      // flat_map
      BValue flatmap_fn = get_enum_index(arg, 1);
      arg = get_enum_index(arg, 0);
      stack = alloc_enum2(1, flatmap_fn, stack);
      break;
    }
    case 3:
    {
      // push recover onto stack
      BValue recover_fn = get_enum_index(arg, 1);
      arg = get_enum_index(arg, 0);
      stack = alloc_enum2(2, recover_fn, stack);
      break;
    }
    case 4:
      // apply_fix
      arg = bsts_prog_step_fix(get_enum_index(arg, 0), get_enum_index(arg, 1));
      break;
    case 5:
      // ReadEnv() => (5, )
      arg = ___bsts_g_Bosatsu_l_Prog_l_pure(env);
      break;
    case 6:
    {
      // RemapEnv(f, p) => (6, f, p)
      stack = alloc_enum2(3, env, stack);
      BValue remap_fn = get_enum_index(arg, 1);
      env = call_fn1(remap_fn, env);
      arg = get_enum_index(arg, 0);
      break;
    }
    case 7:
    {
      // Effect(arg: BValue, f: BValue => BValue) => (7, f)
      BValue earg = get_enum_index(arg, 0);
      BValue efn = get_enum_index(arg, 1);
      arg = call_fn1(efn, earg);
      break;
    }
    }
  }
  return 0;
}
