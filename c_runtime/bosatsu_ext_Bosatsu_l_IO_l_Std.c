#include "bosatsu_ext_Bosatsu_l_IO_l_Std.h"
#include "bosatsu_ext_Bosatsu_l_Prog.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Bosatsu/IO/Error enum variant indices (must match Bosatsu/IO/Error.bosatsu)
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

static BValue bsts_ioerror_context(const char *context)
{
  if (!context)
  {
    context = "";
  }
  size_t len = strlen(context);
  return bsts_string_from_utf8_bytes_copy(len, (char *)context);
}

static BValue bsts_ioerror_known(int variant, const char *context)
{
  return alloc_enum1(variant, bsts_ioerror_context(context));
}

static BValue bsts_ioerror_other(const char *context, int code, const char *msg)
{
  if (!msg)
  {
    msg = "unknown error";
  }
  BValue ctxv = bsts_ioerror_context(context);
  size_t len = strlen(msg);
  BValue msgv = bsts_string_from_utf8_bytes_copy(len, (char *)msg);
  BValue codev = bsts_integer_from_int(code);
  return alloc_enum3(BSTS_IOERR_Other, ctxv, codev, msgv);
}

static BValue bsts_ioerror_from_errno(int err, const char *context)
{
  switch (err)
  {
#ifdef ENOENT
  case ENOENT:
    return bsts_ioerror_known(BSTS_IOERR_NotFound, context);
#endif
#ifdef EACCES
  case EACCES:
    return bsts_ioerror_known(BSTS_IOERR_AccessDenied, context);
#endif
#ifdef EEXIST
  case EEXIST:
    return bsts_ioerror_known(BSTS_IOERR_AlreadyExists, context);
#endif
#ifdef ENOTDIR
  case ENOTDIR:
    return bsts_ioerror_known(BSTS_IOERR_NotDirectory, context);
#endif
#ifdef EISDIR
  case EISDIR:
    return bsts_ioerror_known(BSTS_IOERR_IsDirectory, context);
#endif
#ifdef ENOTEMPTY
  case ENOTEMPTY:
    return bsts_ioerror_known(BSTS_IOERR_NotEmpty, context);
#endif
#ifdef EMFILE
  case EMFILE:
    return bsts_ioerror_known(BSTS_IOERR_TooManyOpenFiles, context);
#endif
#ifdef EROFS
  case EROFS:
    return bsts_ioerror_known(BSTS_IOERR_ReadOnlyFileSystem, context);
#endif
#ifdef EXDEV
  case EXDEV:
    return bsts_ioerror_known(BSTS_IOERR_CrossDeviceLink, context);
#endif
#ifdef ENOSPC
  case ENOSPC:
    return bsts_ioerror_known(BSTS_IOERR_NoSpace, context);
#endif
#ifdef EDQUOT
  case EDQUOT:
    return bsts_ioerror_known(BSTS_IOERR_QuotaExceeded, context);
#endif
#ifdef ENAMETOOLONG
  case ENAMETOOLONG:
    return bsts_ioerror_known(BSTS_IOERR_NameTooLong, context);
#endif
#ifdef EINVAL
  case EINVAL:
    return bsts_ioerror_known(BSTS_IOERR_InvalidArgument, context);
#endif
#ifdef EILSEQ
  case EILSEQ:
    return bsts_ioerror_known(BSTS_IOERR_InvalidUtf8, context);
#endif
#ifdef EBADF
  case EBADF:
    return bsts_ioerror_known(BSTS_IOERR_BadFileDescriptor, context);
#endif
#ifdef EINTR
  case EINTR:
    return bsts_ioerror_known(BSTS_IOERR_Interrupted, context);
#endif
#ifdef EAGAIN
  case EAGAIN:
    return bsts_ioerror_known(BSTS_IOERR_WouldBlock, context);
#endif
#if defined(EWOULDBLOCK) && (!defined(EAGAIN) || (EWOULDBLOCK != EAGAIN))
  case EWOULDBLOCK:
    return bsts_ioerror_known(BSTS_IOERR_WouldBlock, context);
#endif
#ifdef ETIMEDOUT
  case ETIMEDOUT:
    return bsts_ioerror_known(BSTS_IOERR_TimedOut, context);
#endif
#ifdef EPIPE
  case EPIPE:
    return bsts_ioerror_known(BSTS_IOERR_BrokenPipe, context);
#endif
#ifdef EOPNOTSUPP
  case EOPNOTSUPP:
    return bsts_ioerror_known(BSTS_IOERR_Unsupported, context);
#endif
#if defined(ENOTSUP) && (!defined(EOPNOTSUPP) || (ENOTSUP != EOPNOTSUPP))
  case ENOTSUP:
    return bsts_ioerror_known(BSTS_IOERR_Unsupported, context);
#endif
  default:
    return bsts_ioerror_other(context, err, strerror(err));
  }
}

static BValue bsts_ioerror_from_errno_default(int err, const char *context)
{
  if (err == 0)
  {
#ifdef EIO
    err = EIO;
#else
    return bsts_ioerror_other(context, 0, "unknown error");
#endif
  }
  return bsts_ioerror_from_errno(err, context);
}

static inline BValue bsts_ioerror_invalid_utf8(const char *context)
{
  return bsts_ioerror_known(BSTS_IOERR_InvalidUtf8, context);
}

static inline BValue bsts_ioerror_invalid_argument(const char *context)
{
  return bsts_ioerror_known(BSTS_IOERR_InvalidArgument, context);
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
          bsts_ioerror_from_errno_default(errno, "writing to stdout"));
    }
  }
  if (fflush(stdout) != 0)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, "flushing stdout"));
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
          bsts_ioerror_from_errno_default(errno, "writing to stdout"));
    }
  }
  errno = 0;
  size_t wrote_newline = fwrite("\n", 1, 1, stdout);
  if (wrote_newline != 1)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, "writing newline to stdout"));
  }
  if (fflush(stdout) != 0)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, "flushing stdout"));
  }
  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_unit_value());
}

BValue bsts_print_err_effect(BValue a)
{
  char *bytes = bsts_string_utf8_bytes(a);
  size_t len = bsts_string_utf8_len(a);
  if (len > 0)
  {
    errno = 0;
    size_t written = fwrite(bytes, 1, len, stderr);
    if (written < len)
    {
      return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
          bsts_ioerror_from_errno_default(errno, "writing to stderr"));
    }
  }
  if (fflush(stderr) != 0)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, "flushing stderr"));
  }
  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_unit_value());
}

BValue bsts_print_errln_effect(BValue a)
{
  char *bytes = bsts_string_utf8_bytes(a);
  size_t len = bsts_string_utf8_len(a);
  if (len > 0)
  {
    errno = 0;
    size_t written = fwrite(bytes, 1, len, stderr);
    if (written < len)
    {
      return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
          bsts_ioerror_from_errno_default(errno, "writing to stderr"));
    }
  }
  errno = 0;
  size_t wrote_newline = fwrite("\n", 1, 1, stderr);
  if (wrote_newline != 1)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, "writing newline to stderr"));
  }
  if (fflush(stderr) != 0)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, "flushing stderr"));
  }
  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_unit_value());
}

static BValue bsts_io_std_print_prog(BValue a)
{
  return alloc_enum2(7, a, alloc_boxed_pure_fn1(bsts_print_effect));
}

static BValue bsts_io_std_println_prog(BValue a)
{
  return alloc_enum2(7, a, alloc_boxed_pure_fn1(bsts_println_effect));
}

static BValue bsts_io_std_print_err_prog(BValue a)
{
  return alloc_enum2(7, a, alloc_boxed_pure_fn1(bsts_print_err_effect));
}

static BValue bsts_io_std_print_errln_prog(BValue a)
{
  return alloc_enum2(7, a, alloc_boxed_pure_fn1(bsts_print_errln_effect));
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
    char context[96];
    snprintf(context, sizeof(context),
             "read_stdin_utf8_bytes negative argument: %d",
             requested);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_invalid_argument(context));
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
        bsts_ioerror_other("reading from stdin", err, "out of memory"));
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
            bsts_ioerror_from_errno_default(errno, "reading from stdin"));
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
          bsts_ioerror_invalid_utf8("decoding bytes from stdin"));
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
          bsts_ioerror_invalid_utf8("decoding bytes from stdin"));
    }

    int ch = fgetc(stdin);
    if (ch == EOF)
    {
      if (ferror(stdin))
      {
        free(buf);
        return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
            bsts_ioerror_from_errno_default(errno, "reading from stdin"));
      }
      /* EOF but current bytes don't end on a code point boundary -> invalid. */
      free(buf);
      return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
          bsts_ioerror_invalid_utf8("decoding bytes from stdin"));
    }

    buf[len++] = (char)ch;
  }

  buf[len] = '\0';
  BValue result = ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_string_from_utf8_bytes_copy(len, buf));
  free(buf);
  return result;
}

static BValue bsts_io_std_read_stdin_utf8_bytes_prog(BValue a)
{
  return alloc_enum2(7, a, alloc_boxed_pure_fn1(bsts_read_stdin_utf8_bytes_effect));
}

BValue ___bsts_g_Bosatsu_l_IO_l_Std_l_print__impl(BValue a)
{
  return bsts_io_std_print_prog(a);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Std_l_println__impl(BValue a)
{
  return bsts_io_std_println_prog(a);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Std_l_print__err__impl(BValue a)
{
  return bsts_io_std_print_err_prog(a);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Std_l_print__errln__impl(BValue a)
{
  return bsts_io_std_print_errln_prog(a);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Std_l_read__stdin__utf8__bytes__impl(BValue a)
{
  return bsts_io_std_read_stdin_utf8_bytes_prog(a);
}
