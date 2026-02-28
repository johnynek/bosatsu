#include "bosatsu_ext_Bosatsu_l_IO_l_Core.h"
#include "bosatsu_ext_Bosatsu_l_IO_l_Bytes.h"
#include "bosatsu_ext_Bosatsu_l_Prog.h"

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <gc.h>
#include <limits.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>

#if defined(__APPLE__) || defined(__linux__)
int mkstemps(char *template, int suffixlen);
#endif

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

typedef enum
{
  BSTS_HANDLE_STDIN = 0,
  BSTS_HANDLE_STDOUT = 1,
  BSTS_HANDLE_STDERR = 2,
  BSTS_HANDLE_FILE = 3
} BSTS_Handle_Kind;

typedef struct
{
  BSTS_Handle_Kind kind;
  FILE *file;
  int readable;
  int writable;
  int close_on_close;
  int closed;
} BSTS_Core_Handle;

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

static inline BValue bsts_ioerror_invalid_argument(const char *context)
{
  return bsts_ioerror_known(BSTS_IOERR_InvalidArgument, context);
}

static inline BValue bsts_ioerror_invalid_utf8(const char *context)
{
  return bsts_ioerror_known(BSTS_IOERR_InvalidUtf8, context);
}

static inline BValue bsts_ioerror_bad_fd(const char *context)
{
  return bsts_ioerror_known(BSTS_IOERR_BadFileDescriptor, context);
}

static inline BValue bsts_ioerror_unsupported(const char *context)
{
  return bsts_ioerror_known(BSTS_IOERR_Unsupported, context);
}

static BValue bsts_prog_effect1(BValue a, BValue (*fn)(BValue))
{
  return alloc_enum2(5, a, alloc_boxed_pure_fn1(fn));
}

static BValue bsts_prog_effect2(BValue a, BValue b, BValue (*fn)(BValue))
{
  return alloc_enum2(5, alloc_struct2(a, b), alloc_boxed_pure_fn1(fn));
}

static BValue bsts_prog_effect3(BValue a, BValue b, BValue c, BValue (*fn)(BValue))
{
  return alloc_enum2(5, alloc_struct3(a, b, c), alloc_boxed_pure_fn1(fn));
}

static BValue bsts_prog_effect4(BValue a, BValue b, BValue c, BValue d, BValue (*fn)(BValue))
{
  return alloc_enum2(5, alloc_struct4(a, b, c, d), alloc_boxed_pure_fn1(fn));
}

static BValue bsts_option_none(void)
{
  return alloc_enum0(0);
}

static BValue bsts_option_some(BValue v)
{
  return alloc_enum1(1, v);
}

static void bsts_core_handle_free(void *ptr)
{
  (void)ptr;
}

static BSTS_Core_Handle *bsts_core_unbox_handle(BValue handle)
{
  return (BSTS_Core_Handle *)get_external(handle);
}

static BValue bsts_core_make_handle(
    BSTS_Handle_Kind kind,
    FILE *file,
    int readable,
    int writable,
    int close_on_close)
{
  BSTS_Core_Handle *h = (BSTS_Core_Handle *)GC_malloc(sizeof(BSTS_Core_Handle));
  if (h == NULL)
  {
    perror("GC_malloc failure in bsts_core_make_handle");
    abort();
  }
  h->kind = kind;
  h->file = file;
  h->readable = readable;
  h->writable = writable;
  h->close_on_close = close_on_close;
  h->closed = 0;
  return alloc_external(h, bsts_core_handle_free);
}

static BValue bsts_integer_from_int64_local(int64_t value)
{
  if ((value >= INT32_MIN) && (value <= INT32_MAX))
  {
    return bsts_integer_from_int((int32_t)value);
  }

  uint64_t mag;
  _Bool pos;
  if (value >= 0)
  {
    pos = 1;
    mag = (uint64_t)value;
  }
  else
  {
    pos = 0;
    mag = (uint64_t)(-(value + 1));
    mag += 1;
  }

  uint32_t words[2];
  words[0] = (uint32_t)(mag & 0xFFFFFFFFULL);
  words[1] = (uint32_t)((mag >> 32) & 0xFFFFFFFFULL);
  size_t size = (words[1] == 0U) ? 1U : 2U;
  return bsts_integer_from_words_copy(pos, size, words);
}

static char *bsts_string_to_cstr(BValue str)
{
  BSTS_String_View view = bsts_string_view_ref(&str);
  char *out = (char *)malloc(view.len + 1);
  if (!out)
  {
#ifdef ENOMEM
    errno = ENOMEM;
#else
    errno = 0;
#endif
    return NULL;
  }
  if (view.len > 0)
  {
    memcpy(out, view.bytes, view.len);
  }
  out[view.len] = '\0';
  return out;
}

static char *bsts_path_to_cstr(BValue path)
{
  // Struct-1 values are represented as identity in C codegen.
  return bsts_string_to_cstr(path);
}

static BValue bsts_path_from_cstr(const char *path)
{
  size_t len = strlen(path);
  return bsts_string_from_utf8_bytes_copy(len, path);
}

static _Bool bsts_utf8_is_valid_prefix(const char *data, int len)
{
  int offset = 0;
  while (offset < len)
  {
    int cp_len = bsts_utf8_code_point_bytes(data, offset, len);
    if (cp_len <= 0)
    {
      return 0;
    }
    offset += cp_len;
  }
  return offset == len;
}

static int bsts_int_arg_positive(BValue value, int *out)
{
  BValue zero = bsts_integer_from_int(0);
  if (bsts_integer_cmp(value, zero) <= 0)
  {
    return 0;
  }
  BValue max_int = bsts_integer_from_int(INT_MAX);
  if (bsts_integer_cmp(value, max_int) > 0)
  {
    *out = INT_MAX;
  }
  else
  {
    *out = (int)bsts_integer_to_int32(value);
  }
  return 1;
}

static int bsts_option_int(BValue option, _Bool *is_some, BValue *out_value)
{
  ENUM_TAG tag = get_variant(option);
  if (tag == 0)
  {
    *is_some = 0;
    *out_value = bsts_integer_from_int(0);
    return 1;
  }
  if (tag == 1)
  {
    *is_some = 1;
    *out_value = get_enum_index(option, 0);
    return 1;
  }
  return 0;
}

static int bsts_option_path(BValue option, _Bool *is_some, char **out_path)
{
  ENUM_TAG tag = get_variant(option);
  if (tag == 0)
  {
    *is_some = 0;
    *out_path = NULL;
    return 1;
  }
  if (tag == 1)
  {
    BValue path_value = get_enum_index(option, 0);
    // bsts_path_to_cstr allocates with malloc; caller owns *out_path and must free it.
    char *path = bsts_path_to_cstr(path_value);
    if (!path)
    {
      return 0;
    }
    *is_some = 1;
    *out_path = path;
    return 1;
  }
  return 0;
}

static int bsts_temp_name_part_valid(const char *part)
{
  if (!part)
  {
    return 0;
  }

  const unsigned char *p = (const unsigned char *)part;
  while (*p != '\0')
  {
    if ((*p < 32U) || (*p == (unsigned char)'/') || (*p == (unsigned char)'\\'))
    {
      return 0;
    }
    p++;
  }
  return 1;
}

static char *bsts_normalize_temp_prefix(const char *prefix)
{
  const char *source = prefix;
  if (!source || source[0] == '\0')
  {
    source = "tmp";
  }

  size_t src_len = strlen(source);
  size_t out_len = (src_len >= 3U) ? src_len : 3U;
  // Returned buffer is malloc-owned by caller; freed in create_temp_file/create_temp_dir paths.
  char *out = (char *)malloc(out_len + 1U);
  if (!out)
  {
#ifdef ENOMEM
    errno = ENOMEM;
#else
    errno = 0;
#endif
    return NULL;
  }

  memcpy(out, source, src_len);
  for (size_t i = src_len; i < out_len; i++)
  {
    out[i] = '_';
  }
  out[out_len] = '\0';
  return out;
}

static const char *bsts_nonempty_env(const char *name)
{
  const char *value = getenv(name);
  if (value && value[0] != '\0')
  {
    return value;
  }
  return NULL;
}

static const char *bsts_default_tmp_dir(void)
{
  const char *env_tmp = bsts_nonempty_env("TMPDIR");
  if (!env_tmp)
  {
    env_tmp = bsts_nonempty_env("TMP");
  }
  if (!env_tmp)
  {
    env_tmp = bsts_nonempty_env("TEMP");
  }
  if (env_tmp)
  {
    return env_tmp;
  }
#ifdef P_tmpdir
  return P_tmpdir;
#else
  return "/tmp";
#endif
}

static char *bsts_make_temp_template(
    const char *dir,
    const char *prefix,
    const char *suffix)
{
  const char *base_dir = dir;
  if (!base_dir)
  {
    base_dir = bsts_default_tmp_dir();
  }

  size_t dir_len = strlen(base_dir);
  size_t prefix_len = strlen(prefix);
  size_t suffix_len = strlen(suffix);
  int needs_sep = (dir_len > 0U && base_dir[dir_len - 1U] != '/');
  size_t total_len = dir_len + (size_t)(needs_sep ? 1 : 0) + prefix_len + 6U + suffix_len;
  // Returned buffer is malloc-owned by caller; freed after mkstemp/mkstemps/mkdtemp.
  char *template = (char *)malloc(total_len + 1U);
  if (!template)
  {
#ifdef ENOMEM
    errno = ENOMEM;
#else
    errno = 0;
#endif
    return NULL;
  }

  size_t offset = 0U;
  if (dir_len > 0U)
  {
    memcpy(template + offset, base_dir, dir_len);
    offset += dir_len;
  }
  if (needs_sep)
  {
    template[offset++] = '/';
  }
  if (prefix_len > 0U)
  {
    memcpy(template + offset, prefix, prefix_len);
    offset += prefix_len;
  }
  memcpy(template + offset, "XXXXXX", 6U);
  offset += 6U;
  if (suffix_len > 0U)
  {
    memcpy(template + offset, suffix, suffix_len);
    offset += suffix_len;
  }
  template[offset] = '\0';
  return template;
}

static int bsts_join_path(char **out_path, const char *base, const char *name)
{
  size_t base_len = strlen(base);
  size_t name_len = strlen(name);
  int need_sep = (base_len > 0 && base[base_len - 1] != '/');
  size_t total = base_len + (size_t)(need_sep ? 1 : 0) + name_len;
  // Returned buffer is malloc-owned by caller; list_dir frees each successful join.
  char *joined = (char *)malloc(total + 1);
  if (!joined)
  {
#ifdef ENOMEM
    errno = ENOMEM;
#else
    errno = 0;
#endif
    return 0;
  }

  memcpy(joined, base, base_len);
  size_t offset = base_len;
  if (need_sep)
  {
    joined[offset++] = '/';
  }
  memcpy(joined + offset, name, name_len);
  joined[total] = '\0';
  *out_path = joined;
  return 1;
}

static void bsts_contextf(char *out, size_t out_size, const char *fmt, ...)
{
  if (out_size == 0U)
  {
    return;
  }
  va_list args;
  va_start(args, fmt);
  int written = vsnprintf(out, out_size, fmt, args);
  va_end(args);
  if (written < 0)
  {
    out[0] = '\0';
  }
}

static const char *bsts_open_mode_name(ENUM_TAG mode_tag)
{
  switch (mode_tag)
  {
  case 0:
    return "Read";
  case 1:
    return "WriteTruncate";
  case 2:
    return "Append";
  case 3:
    return "CreateNew";
  default:
    return "Unknown";
  }
}

static int bsts_cmp_cstr(const void *left, const void *right)
{
  const char *l = *(const char *const *)left;
  const char *r = *(const char *const *)right;
  return strcmp(l, r);
}

static int bsts_mkdirs(const char *path)
{
  char *copy = strdup(path);
  if (!copy)
  {
#ifdef ENOMEM
    errno = ENOMEM;
#else
    errno = 0;
#endif
    return -1;
  }

  size_t len = strlen(copy);
  if (len == 0)
  {
    free(copy);
    errno = EINVAL;
    return -1;
  }

  for (char *p = copy + 1; *p; p++)
  {
    if (*p == '/')
    {
      *p = '\0';
      if (strlen(copy) > 0)
      {
        if (mkdir(copy, 0777) != 0 && errno != EEXIST)
        {
          free(copy);
          return -1;
        }
      }
      *p = '/';
    }
  }

  if (mkdir(copy, 0777) != 0 && errno != EEXIST)
  {
    free(copy);
    return -1;
  }

  free(copy);
  return 0;
}

static int bsts_remove_recursive_impl(const char *path)
{
  struct stat st;
  if (lstat(path, &st) != 0)
  {
    return -1;
  }

  if (S_ISDIR(st.st_mode) && !S_ISLNK(st.st_mode))
  {
    DIR *dir = opendir(path);
    if (!dir)
    {
      return -1;
    }

    struct dirent *entry;
    while ((entry = readdir(dir)) != NULL)
    {
      const char *name = entry->d_name;
      if (strcmp(name, ".") == 0 || strcmp(name, "..") == 0)
      {
        continue;
      }

      char *child = NULL;
      if (!bsts_join_path(&child, path, name))
      {
        closedir(dir);
        return -1;
      }

      if (bsts_remove_recursive_impl(child) != 0)
      {
        free(child);
        closedir(dir);
        return -1;
      }
      free(child);
    }

    if (closedir(dir) != 0)
    {
      return -1;
    }

    return rmdir(path);
  }

  return unlink(path);
}

static BValue bsts_core_read_utf8_effect(BValue pair)
{
  BValue handle_value = get_struct_index(pair, 0);
  BValue max_chars_value = get_struct_index(pair, 1);

  BSTS_Core_Handle *handle = bsts_core_unbox_handle(handle_value);
  if (handle->closed)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_bad_fd("reading from closed handle"));
  }
  if (!handle->readable)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_bad_fd("reading from write-only handle"));
  }

  int max_chars = 0;
  if (!bsts_int_arg_positive(max_chars_value, &max_chars))
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_invalid_argument("read_utf8 max_chars must be > 0"));
  }

  char *buf = (char *)malloc((size_t)max_chars + 1);
  if (!buf)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, "allocating read buffer"));
  }

  errno = 0;
  size_t read_count = fread(buf, 1, (size_t)max_chars, handle->file);
  if (read_count == 0)
  {
    free(buf);
    if (feof(handle->file))
    {
      return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_option_none());
    }
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, "reading utf8"));
  }

  if (!bsts_utf8_is_valid_prefix(buf, (int)read_count))
  {
    free(buf);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_invalid_utf8("decoding bytes from handle"));
  }

  BValue text = bsts_string_from_utf8_bytes_copy(read_count, buf);
  free(buf);
  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_option_some(text));
}

static BValue bsts_core_write_utf8_effect(BValue pair)
{
  BValue handle_value = get_struct_index(pair, 0);
  BValue text_value = get_struct_index(pair, 1);

  BSTS_Core_Handle *handle = bsts_core_unbox_handle(handle_value);
  if (handle->closed)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_bad_fd("writing to closed handle"));
  }
  if (!handle->writable)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_bad_fd("writing to read-only handle"));
  }

  BSTS_String_View view = bsts_string_view_ref(&text_value);
  errno = 0;
  if (view.len > 0)
  {
    size_t wrote = fwrite(view.bytes, 1, view.len, handle->file);
    if (wrote < view.len)
    {
      return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
          bsts_ioerror_from_errno_default(errno, "writing utf8"));
    }
  }

  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_unit_value());
}

static BValue bsts_core_read_bytes_effect(BValue pair)
{
  BValue handle_value = get_struct_index(pair, 0);
  BValue max_bytes_value = get_struct_index(pair, 1);

  BSTS_Core_Handle *handle = bsts_core_unbox_handle(handle_value);
  if (handle->closed)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_bad_fd("reading from closed handle"));
  }
  if (!handle->readable)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_bad_fd("reading from write-only handle"));
  }

  int max_bytes = 0;
  if (!bsts_int_arg_positive(max_bytes_value, &max_bytes))
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_invalid_argument("read_bytes max_bytes must be > 0"));
  }

  uint8_t *buf = (uint8_t *)malloc((size_t)max_bytes);
  if (!buf)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, "allocating read buffer"));
  }

  errno = 0;
  size_t read_count = fread(buf, 1, (size_t)max_bytes, handle->file);
  if (read_count == 0)
  {
    free(buf);
    if (feof(handle->file))
    {
      return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_option_none());
    }
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, "reading bytes"));
  }

  uint8_t *data = (uint8_t *)GC_malloc_atomic(read_count);
  if (data == NULL)
  {
    free(buf);
    perror("GC_malloc_atomic failure in bsts_core_read_bytes_effect");
    abort();
  }
  memcpy(data, buf, read_count);
  free(buf);

  BValue bytes = bsts_bytes_wrap(data, 0, (int)read_count);
  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_option_some(bytes));
}

static BValue bsts_core_write_bytes_effect(BValue pair)
{
  BValue handle_value = get_struct_index(pair, 0);
  BValue bytes_value = get_struct_index(pair, 1);

  BSTS_Core_Handle *handle = bsts_core_unbox_handle(handle_value);
  if (handle->closed)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_bad_fd("writing to closed handle"));
  }
  if (!handle->writable)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_bad_fd("writing to read-only handle"));
  }

  BSTS_Bytes *bytes = bsts_bytes_unbox(bytes_value);
  errno = 0;
  if (bytes->len > 0)
  {
    size_t wrote = fwrite(bytes->data + bytes->offset, 1, (size_t)bytes->len, handle->file);
    if (wrote < (size_t)bytes->len)
    {
      return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
          bsts_ioerror_from_errno_default(errno, "writing bytes"));
    }
  }

  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_unit_value());
}

static BValue bsts_core_read_all_bytes_effect(BValue pair)
{
  BValue handle_value = get_struct_index(pair, 0);
  BValue chunk_size_value = get_struct_index(pair, 1);

  BSTS_Core_Handle *handle = bsts_core_unbox_handle(handle_value);
  if (handle->closed)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_bad_fd("reading from closed handle"));
  }
  if (!handle->readable)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_bad_fd("reading from write-only handle"));
  }

  int chunk_size = 0;
  if (!bsts_int_arg_positive(chunk_size_value, &chunk_size))
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_invalid_argument("read_all_bytes chunk_size must be > 0"));
  }

  uint8_t *chunk_buf = (uint8_t *)malloc((size_t)chunk_size);
  if (!chunk_buf)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, "allocating read buffer"));
  }

  uint8_t *acc = NULL;
  size_t total = 0;
  size_t cap = 0;

  while (1)
  {
    errno = 0;
    size_t read_count = fread(chunk_buf, 1, (size_t)chunk_size, handle->file);
    if (read_count == 0)
    {
      if (feof(handle->file))
      {
        break;
      }
      free(chunk_buf);
      free(acc);
      return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
          bsts_ioerror_from_errno_default(errno, "reading bytes"));
    }

    if (total > (size_t)INT_MAX - read_count)
    {
      free(chunk_buf);
      free(acc);
      return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
          bsts_ioerror_invalid_argument("read_all_bytes result too large"));
    }

    size_t needed = total + read_count;
    if (needed > cap)
    {
      size_t next_cap = cap == 0 ? needed : cap;
      while (next_cap < needed)
      {
        if (next_cap > (SIZE_MAX / 2))
        {
          next_cap = needed;
          break;
        }
        next_cap = next_cap * 2;
      }

      uint8_t *next = (uint8_t *)realloc(acc, next_cap);
      if (!next)
      {
        free(chunk_buf);
        free(acc);
        return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
            bsts_ioerror_from_errno_default(errno, "growing read buffer"));
      }
      acc = next;
      cap = next_cap;
    }

    memcpy(acc + total, chunk_buf, read_count);
    total += read_count;

    if ((read_count < (size_t)chunk_size) && feof(handle->file))
    {
      break;
    }
  }

  free(chunk_buf);

  if (total == 0)
  {
    free(acc);
    return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_bytes_empty());
  }

  uint8_t *data = (uint8_t *)GC_malloc_atomic(total);
  if (data == NULL)
  {
    free(acc);
    perror("GC_malloc_atomic failure in bsts_core_read_all_bytes_effect");
    abort();
  }
  memcpy(data, acc, total);
  free(acc);

  BValue bytes = bsts_bytes_wrap(data, 0, (int)total);
  return ___bsts_g_Bosatsu_l_Prog_l_pure(bytes);
}

static BValue bsts_core_copy_bytes_effect(BValue args4)
{
  BValue src_value = get_struct_index(args4, 0);
  BValue dst_value = get_struct_index(args4, 1);
  BValue chunk_size_value = get_struct_index(args4, 2);
  BValue max_total_value = get_struct_index(args4, 3);

  BSTS_Core_Handle *src = bsts_core_unbox_handle(src_value);
  BSTS_Core_Handle *dst = bsts_core_unbox_handle(dst_value);
  if (src->closed)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_bad_fd("reading from closed handle"));
  }
  if (!src->readable)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_bad_fd("reading from write-only handle"));
  }
  if (dst->closed)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_bad_fd("writing to closed handle"));
  }
  if (!dst->writable)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_bad_fd("writing to read-only handle"));
  }

  int chunk_size = 0;
  if (!bsts_int_arg_positive(chunk_size_value, &chunk_size))
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_invalid_argument("copy_bytes chunk_size must be > 0"));
  }

  _Bool has_limit = 0;
  BValue limit_value = bsts_integer_from_int(0);
  if (!bsts_option_int(max_total_value, &has_limit, &limit_value))
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_invalid_argument("copy_bytes max_total must be Option[Int]"));
  }

  BValue zero = bsts_integer_from_int(0);
  if (has_limit && (bsts_integer_cmp(limit_value, zero) < 0))
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_invalid_argument("copy_bytes max_total must be >= 0"));
  }
  if (has_limit && (bsts_integer_cmp(limit_value, zero) == 0))
  {
    return ___bsts_g_Bosatsu_l_Prog_l_pure(zero);
  }

  uint8_t *buf = (uint8_t *)malloc((size_t)chunk_size);
  if (!buf)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, "allocating copy buffer"));
  }

  BValue copied = zero;
  BValue chunk_size_int = bsts_integer_from_int(chunk_size);
  while (1)
  {
    int to_read = chunk_size;
    if (has_limit)
    {
      BValue remaining = bsts_integer_add(limit_value, bsts_integer_negate(copied));
      if (bsts_integer_cmp(remaining, zero) <= 0)
      {
        break;
      }
      if (bsts_integer_cmp(remaining, chunk_size_int) < 0)
      {
        to_read = (int)bsts_integer_to_int32(remaining);
      }
    }

    errno = 0;
    size_t read_count = fread(buf, 1, (size_t)to_read, src->file);
    if (read_count == 0)
    {
      if (feof(src->file))
      {
        break;
      }
      free(buf);
      return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
          bsts_ioerror_from_errno_default(errno, "reading bytes"));
    }

    errno = 0;
    size_t wrote = fwrite(buf, 1, read_count, dst->file);
    if (wrote < read_count)
    {
      free(buf);
      return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
          bsts_ioerror_from_errno_default(errno, "writing bytes"));
    }

    copied = bsts_integer_add(copied, bsts_integer_from_int((int32_t)read_count));
  }

  free(buf);
  return ___bsts_g_Bosatsu_l_Prog_l_pure(copied);
}

static BValue bsts_core_flush_effect(BValue handle_value)
{
  BSTS_Core_Handle *handle = bsts_core_unbox_handle(handle_value);
  if (handle->closed)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_bad_fd("flushing closed handle"));
  }

  if (!handle->writable)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_unit_value());
  }

  errno = 0;
  if (fflush(handle->file) != 0)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, "flushing handle"));
  }

  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_unit_value());
}

static BValue bsts_core_close_effect(BValue handle_value)
{
  BSTS_Core_Handle *handle = bsts_core_unbox_handle(handle_value);
  if (handle->closed)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_unit_value());
  }

  if (handle->close_on_close)
  {
    errno = 0;
    if (fclose(handle->file) != 0)
    {
      return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
          bsts_ioerror_from_errno_default(errno, "closing handle"));
    }
  }

  handle->closed = 1;
  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_unit_value());
}

static BValue bsts_core_open_file_effect(BValue pair)
{
  BValue path_value = get_struct_index(pair, 0);
  BValue mode_value = get_struct_index(pair, 1);

  char context[512];
  char *path = bsts_path_to_cstr(path_value);
  if (!path)
  {
    bsts_contextf(
        context,
        sizeof(context),
        "open_file(path=<invalid Path>)");
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, context));
  }

  ENUM_TAG mode_tag = get_variant(mode_value);
  const char *mode_name = bsts_open_mode_name(mode_tag);
  const char *open_mode = NULL;
  int readable = 0;
  int writable = 0;

  switch (mode_tag)
  {
  case 0: // Read
    open_mode = "rb";
    readable = 1;
    break;
  case 1: // WriteTruncate
    open_mode = "wb";
    writable = 1;
    break;
  case 2: // Append
    open_mode = "ab";
    writable = 1;
    break;
  case 3: // CreateNew
  {
    errno = 0;
    int fd = open(path, O_WRONLY | O_CREAT | O_EXCL, 0666);
    if (fd < 0)
    {
      bsts_contextf(
          context,
          sizeof(context),
          "open_file(path=%s, mode=%s): open(O_CREAT|O_EXCL) failed",
          path,
          mode_name);
      BValue err = bsts_ioerror_from_errno_default(errno, context);
      free(path);
      return ___bsts_g_Bosatsu_l_Prog_l_raise__error(err);
    }

    FILE *created_file = fdopen(fd, "wb");
    if (!created_file)
    {
      bsts_contextf(
          context,
          sizeof(context),
          "open_file(path=%s, mode=%s): fdopen(\"wb\") failed",
          path,
          mode_name);
      BValue err = bsts_ioerror_from_errno_default(errno, context);
      close(fd);
      free(path);
      return ___bsts_g_Bosatsu_l_Prog_l_raise__error(err);
    }

    free(path);
    BValue handle = bsts_core_make_handle(BSTS_HANDLE_FILE, created_file, 0, 1, 1);
    return ___bsts_g_Bosatsu_l_Prog_l_pure(handle);
  }
  default:
    bsts_contextf(
        context,
        sizeof(context),
        "open_file(path=%s, mode_tag=%d): invalid OpenMode value",
        path,
        mode_tag);
    free(path);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_invalid_argument(context));
  }

  errno = 0;
  FILE *file = fopen(path, open_mode);
  if (!file)
  {
    bsts_contextf(
        context,
        sizeof(context),
        "open_file(path=%s, mode=%s): fopen(%s) failed",
        path,
        mode_name,
        open_mode);
    BValue err = bsts_ioerror_from_errno_default(errno, context);
    free(path);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(err);
  }

  free(path);
  BValue handle = bsts_core_make_handle(BSTS_HANDLE_FILE, file, readable, writable, 1);
  return ___bsts_g_Bosatsu_l_Prog_l_pure(handle);
}

static BValue bsts_core_create_temp_file_effect(BValue args3)
{
  BValue dir_option = get_struct_index(args3, 0);
  BValue prefix_value = get_struct_index(args3, 1);
  BValue suffix_value = get_struct_index(args3, 2);
  ENUM_TAG dir_tag = get_variant(dir_option);
  const char *dir_tag_context = "<invalid dir option>";
  if (dir_tag == 0)
  {
    dir_tag_context = "<default-temp-dir>";
  }
  else if (dir_tag == 1)
  {
    dir_tag_context = "<provided-dir>";
  }

  char context[768];
  char *prefix_raw = bsts_string_to_cstr(prefix_value);
  if (!prefix_raw)
  {
    bsts_contextf(
        context,
        sizeof(context),
        "create_temp_file(dir=%s, prefix=<invalid String>, suffix=<unknown>): decoding prefix failed",
        dir_tag_context);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, context));
  }

  char *suffix_raw = bsts_string_to_cstr(suffix_value);
  if (!suffix_raw)
  {
    bsts_contextf(
        context,
        sizeof(context),
        "create_temp_file(dir=%s, prefix=%s, suffix=<invalid String>): decoding suffix failed",
        dir_tag_context,
        prefix_raw);
    free(prefix_raw);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, context));
  }

  if (!bsts_temp_name_part_valid(prefix_raw))
  {
    bsts_contextf(
        context,
        sizeof(context),
        "create_temp_file(dir=%s, prefix=%s, suffix=%s): invalid temp file prefix",
        dir_tag_context,
        prefix_raw,
        suffix_raw);
    free(prefix_raw);
    free(suffix_raw);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_invalid_argument(context));
  }
  if (!bsts_temp_name_part_valid(suffix_raw))
  {
    bsts_contextf(
        context,
        sizeof(context),
        "create_temp_file(dir=%s, prefix=%s, suffix=%s): invalid temp file suffix",
        dir_tag_context,
        prefix_raw,
        suffix_raw);
    free(prefix_raw);
    free(suffix_raw);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_invalid_argument(context));
  }

  _Bool has_dir = 0;
  char *dir_path = NULL;
  if (!bsts_option_path(dir_option, &has_dir, &dir_path))
  {
    bsts_contextf(
        context,
        sizeof(context),
        "create_temp_file(dir=<invalid Path option>, prefix=%s, suffix=%s): invalid temp file dir",
        prefix_raw,
        suffix_raw);
    free(prefix_raw);
    free(suffix_raw);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_invalid_argument(context));
  }
  const char *dir_for_context = has_dir ? dir_path : bsts_default_tmp_dir();

  char *prefix_norm = bsts_normalize_temp_prefix(prefix_raw);
  if (!prefix_norm)
  {
    bsts_contextf(
        context,
        sizeof(context),
        "create_temp_file(dir=%s, prefix=%s, suffix=%s): normalize temp prefix failed",
        dir_for_context,
        prefix_raw,
        suffix_raw);
    free(prefix_raw);
    free(suffix_raw);
    if (dir_path)
    {
      free(dir_path);
    }
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, context));
  }

  char *template_path = bsts_make_temp_template(
      has_dir ? dir_path : NULL,
      prefix_norm,
      suffix_raw);
  if (!template_path)
  {
    bsts_contextf(
        context,
        sizeof(context),
        "create_temp_file(dir=%s, prefix=%s, suffix=%s): build temp template failed",
        dir_for_context,
        prefix_raw,
        suffix_raw);
    free(prefix_raw);
    free(suffix_raw);
    free(prefix_norm);
    if (dir_path)
    {
      free(dir_path);
    }
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, context));
  }

  errno = 0;
  int fd = -1;
  if (suffix_raw[0] == '\0')
  {
    fd = mkstemp(template_path);
  }
  else
  {
#if defined(__APPLE__) || defined(__linux__)
    fd = mkstemps(template_path, (int)strlen(suffix_raw));
#else
    bsts_contextf(
        context,
        sizeof(context),
        "create_temp_file(dir=%s, prefix=%s, suffix=%s): suffix support requires mkstemps",
        dir_for_context,
        prefix_raw,
        suffix_raw);
    free(prefix_raw);
    free(suffix_raw);
    free(prefix_norm);
    free(template_path);
    if (dir_path)
    {
      free(dir_path);
    }
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_unsupported(context));
#endif
  }

  if (fd < 0)
  {
    bsts_contextf(
        context,
        sizeof(context),
        "create_temp_file(dir=%s, prefix=%s, suffix=%s): creating temp file from template=%s failed",
        dir_for_context,
        prefix_raw,
        suffix_raw,
        template_path);
    BValue err = bsts_ioerror_from_errno_default(errno, context);
    free(prefix_raw);
    free(suffix_raw);
    free(prefix_norm);
    free(template_path);
    if (dir_path)
    {
      free(dir_path);
    }
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(err);
  }

  FILE *file = fdopen(fd, "wb");
  if (!file)
  {
    bsts_contextf(
        context,
        sizeof(context),
        "create_temp_file(dir=%s, prefix=%s, suffix=%s): opening created path=%s with fdopen(\"wb\") failed",
        dir_for_context,
        prefix_raw,
        suffix_raw,
        template_path);
    BValue err = bsts_ioerror_from_errno_default(errno, context);
    close(fd);
    free(prefix_raw);
    free(suffix_raw);
    free(prefix_norm);
    free(template_path);
    if (dir_path)
    {
      free(dir_path);
    }
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(err);
  }

  BValue path_out = bsts_path_from_cstr(template_path);
  BValue handle_out = bsts_core_make_handle(BSTS_HANDLE_FILE, file, 0, 1, 1);
  BValue out = alloc_struct2(path_out, handle_out);

  // Release malloc-owned temporary buffers now that Bosatsu values/FILE* are built.
  free(prefix_raw);
  free(suffix_raw);
  free(prefix_norm);
  free(template_path);
  if (dir_path)
  {
    free(dir_path);
  }

  return ___bsts_g_Bosatsu_l_Prog_l_pure(out);
}

static BValue bsts_core_create_temp_dir_effect(BValue pair)
{
  BValue dir_option = get_struct_index(pair, 0);
  BValue prefix_value = get_struct_index(pair, 1);
  ENUM_TAG dir_tag = get_variant(dir_option);
  const char *dir_tag_context = "<invalid dir option>";
  if (dir_tag == 0)
  {
    dir_tag_context = "<default-temp-dir>";
  }
  else if (dir_tag == 1)
  {
    dir_tag_context = "<provided-dir>";
  }

  char context[768];
  char *prefix_raw = bsts_string_to_cstr(prefix_value);
  if (!prefix_raw)
  {
    bsts_contextf(
        context,
        sizeof(context),
        "create_temp_dir(dir=%s, prefix=<invalid String>): decoding prefix failed",
        dir_tag_context);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, context));
  }

  if (!bsts_temp_name_part_valid(prefix_raw))
  {
    bsts_contextf(
        context,
        sizeof(context),
        "create_temp_dir(dir=%s, prefix=%s): invalid temp dir prefix",
        dir_tag_context,
        prefix_raw);
    free(prefix_raw);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_invalid_argument(context));
  }

  _Bool has_dir = 0;
  char *dir_path = NULL;
  if (!bsts_option_path(dir_option, &has_dir, &dir_path))
  {
    bsts_contextf(
        context,
        sizeof(context),
        "create_temp_dir(dir=<invalid Path option>, prefix=%s): invalid temp dir",
        prefix_raw);
    free(prefix_raw);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_invalid_argument(context));
  }
  const char *dir_for_context = has_dir ? dir_path : bsts_default_tmp_dir();

  char *prefix_norm = bsts_normalize_temp_prefix(prefix_raw);
  if (!prefix_norm)
  {
    bsts_contextf(
        context,
        sizeof(context),
        "create_temp_dir(dir=%s, prefix=%s): normalize temp prefix failed",
        dir_for_context,
        prefix_raw);
    free(prefix_raw);
    if (dir_path)
    {
      free(dir_path);
    }
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, context));
  }

  char *template_path = bsts_make_temp_template(
      has_dir ? dir_path : NULL,
      prefix_norm,
      "");
  if (!template_path)
  {
    bsts_contextf(
        context,
        sizeof(context),
        "create_temp_dir(dir=%s, prefix=%s): build temp template failed",
        dir_for_context,
        prefix_raw);
    free(prefix_raw);
    free(prefix_norm);
    if (dir_path)
    {
      free(dir_path);
    }
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, context));
  }

  errno = 0;
  char *created = mkdtemp(template_path);
  if (!created)
  {
    bsts_contextf(
        context,
        sizeof(context),
        "create_temp_dir(dir=%s, prefix=%s): mkdtemp(template=%s) failed",
        dir_for_context,
        prefix_raw,
        template_path);
    BValue err = bsts_ioerror_from_errno_default(errno, context);
    free(prefix_raw);
    free(prefix_norm);
    free(template_path);
    if (dir_path)
    {
      free(dir_path);
    }
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(err);
  }

  BValue out = bsts_path_from_cstr(created);
  // Release malloc-owned temporary buffers now that the Bosatsu Path is built.
  free(prefix_raw);
  free(prefix_norm);
  free(template_path);
  if (dir_path)
  {
    free(dir_path);
  }
  return ___bsts_g_Bosatsu_l_Prog_l_pure(out);
}

static BValue bsts_core_list_dir_effect(BValue path_value)
{
  char *path = bsts_path_to_cstr(path_value);
  if (!path)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, "listing directory"));
  }

  DIR *dir = opendir(path);
  if (!dir)
  {
    BValue err = bsts_ioerror_from_errno_default(errno, "listing directory");
    free(path);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(err);
  }

  size_t cap = 16;
  size_t count = 0;
  char **items = (char **)malloc(sizeof(char *) * cap);
  if (!items)
  {
    closedir(dir);
    free(path);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, "allocating directory list"));
  }

  struct dirent *entry;
  while ((entry = readdir(dir)) != NULL)
  {
    const char *name = entry->d_name;
    if (strcmp(name, ".") == 0 || strcmp(name, "..") == 0)
    {
      continue;
    }

    if (count == cap)
    {
      size_t next_cap = cap * 2;
      char **next_items = (char **)realloc(items, sizeof(char *) * next_cap);
      if (!next_items)
      {
        for (size_t i = 0; i < count; i++)
        {
          free(items[i]);
        }
        free(items);
        closedir(dir);
        free(path);
        return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
            bsts_ioerror_from_errno_default(errno, "growing directory list"));
      }
      items = next_items;
      cap = next_cap;
    }

    char *joined = NULL;
    if (!bsts_join_path(&joined, path, name))
    {
      for (size_t i = 0; i < count; i++)
      {
        free(items[i]);
      }
      free(items);
      closedir(dir);
      free(path);
      return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
          bsts_ioerror_from_errno_default(errno, "joining child path"));
    }

    items[count++] = joined;
  }

  if (closedir(dir) != 0)
  {
    for (size_t i = 0; i < count; i++)
    {
      free(items[i]);
    }
    free(items);
    free(path);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, "closing directory"));
  }

  free(path);
  qsort(items, count, sizeof(char *), bsts_cmp_cstr);

  BValue result = alloc_enum0(0);
  for (size_t idx = count; idx > 0; idx--)
  {
    BValue child = bsts_path_from_cstr(items[idx - 1]);
    result = alloc_enum2(1, child, result);
  }

  for (size_t i = 0; i < count; i++)
  {
    free(items[i]);
  }
  free(items);

  return ___bsts_g_Bosatsu_l_Prog_l_pure(result);
}

static BValue bsts_core_stat_effect(BValue path_value)
{
  char *path = bsts_path_to_cstr(path_value);
  if (!path)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, "stating path"));
  }

  struct stat st;
  errno = 0;
  if (lstat(path, &st) != 0)
  {
    int err = errno;
    free(path);
#ifdef ENOENT
    if (err == ENOENT)
    {
      return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_option_none());
    }
#endif
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(err, "stating path"));
  }
  free(path);

  int kind_tag;
  if (S_ISREG(st.st_mode))
  {
    kind_tag = 0; // File
  }
  else if (S_ISDIR(st.st_mode))
  {
    kind_tag = 1; // Dir
  }
  else if (S_ISLNK(st.st_mode))
  {
    kind_tag = 2; // Symlink
  }
  else
  {
    kind_tag = 3; // Other
  }

  BValue kind = alloc_enum0((ENUM_TAG)kind_tag);
  BValue size_bytes = bsts_integer_from_int64_local((int64_t)st.st_size);

#if defined(__APPLE__)
  int64_t sec = (int64_t)st.st_mtimespec.tv_sec;
  long nsec = st.st_mtimespec.tv_nsec;
#else
  int64_t sec = (int64_t)st.st_mtim.tv_sec;
  long nsec = st.st_mtim.tv_nsec;
#endif

  BValue sec_i = bsts_integer_from_int64_local(sec);
  BValue billion = bsts_integer_from_int(1000000000);
  BValue nsec_i = bsts_integer_from_int((int32_t)nsec);
  BValue mtime = bsts_integer_add(bsts_integer_times(sec_i, billion), nsec_i);

  BValue stat_value = alloc_struct3(kind, size_bytes, mtime);
  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_option_some(stat_value));
}

static BValue bsts_core_mkdir_effect(BValue pair)
{
  BValue path_value = get_struct_index(pair, 0);
  BValue recursive_value = get_struct_index(pair, 1);

  char *path = bsts_path_to_cstr(path_value);
  if (!path)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, "creating directory"));
  }

  int recursive = (get_variant(recursive_value) == 1);
  errno = 0;
  int status = recursive ? bsts_mkdirs(path) : mkdir(path, 0777);
  if (status != 0)
  {
    BValue err = bsts_ioerror_from_errno_default(errno, "creating directory");
    free(path);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(err);
  }

  free(path);
  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_unit_value());
}

static BValue bsts_core_remove_effect(BValue pair)
{
  BValue path_value = get_struct_index(pair, 0);
  BValue recursive_value = get_struct_index(pair, 1);

  char *path = bsts_path_to_cstr(path_value);
  if (!path)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, "removing path"));
  }

  int recursive = (get_variant(recursive_value) == 1);
  errno = 0;
  int status = recursive ? bsts_remove_recursive_impl(path) : -1;

  if (!recursive)
  {
    struct stat st;
    if (lstat(path, &st) != 0)
    {
      status = -1;
    }
    else if (S_ISDIR(st.st_mode) && !S_ISLNK(st.st_mode))
    {
      status = rmdir(path);
    }
    else
    {
      status = unlink(path);
    }
  }

  if (status != 0)
  {
    BValue err = bsts_ioerror_from_errno_default(errno, "removing path");
    free(path);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(err);
  }

  free(path);
  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_unit_value());
}

static BValue bsts_core_rename_effect(BValue pair)
{
  BValue from_value = get_struct_index(pair, 0);
  BValue to_value = get_struct_index(pair, 1);

  char *from = bsts_path_to_cstr(from_value);
  if (!from)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, "renaming path"));
  }

  char *to = bsts_path_to_cstr(to_value);
  if (!to)
  {
    free(from);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, "renaming path"));
  }

  errno = 0;
  if (rename(from, to) != 0)
  {
    BValue err = bsts_ioerror_from_errno_default(errno, "renaming path");
    free(from);
    free(to);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(err);
  }

  free(from);
  free(to);
  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_unit_value());
}

static BValue bsts_core_get_env_effect(BValue name_value)
{
  char *name = bsts_string_to_cstr(name_value);
  if (!name)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, "reading environment"));
  }

  const char *value = getenv(name);
  free(name);

  if (!value)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_option_none());
  }

  BValue v = bsts_string_from_utf8_bytes_static_null_term(value);
  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_option_some(v));
}

static BValue bsts_core_spawn_effect(BValue args3)
{
  (void)args3;
  return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
      bsts_ioerror_unsupported("spawn is unsupported in c_runtime"));
}

static BValue bsts_core_wait_effect(BValue process)
{
  (void)process;
  return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
      bsts_ioerror_unsupported("wait is unsupported in c_runtime"));
}

static BValue bsts_core_now_wall_effect(BValue unit)
{
  (void)unit;
  struct timespec ts;
  if (clock_gettime(CLOCK_REALTIME, &ts) != 0)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, "reading wall clock"));
  }

  BValue sec_i = bsts_integer_from_int64_local((int64_t)ts.tv_sec);
  BValue billion = bsts_integer_from_int(1000000000);
  BValue nsec_i = bsts_integer_from_int((int32_t)ts.tv_nsec);
  BValue nanos = bsts_integer_add(bsts_integer_times(sec_i, billion), nsec_i);
  return ___bsts_g_Bosatsu_l_Prog_l_pure(nanos);
}

static BValue bsts_core_now_mono_effect(BValue unit)
{
  (void)unit;
  struct timespec ts;
  if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, "reading monotonic clock"));
  }

  BValue sec_i = bsts_integer_from_int64_local((int64_t)ts.tv_sec);
  BValue billion = bsts_integer_from_int(1000000000);
  BValue nsec_i = bsts_integer_from_int((int32_t)ts.tv_nsec);
  BValue nanos = bsts_integer_add(bsts_integer_times(sec_i, billion), nsec_i);
  return ___bsts_g_Bosatsu_l_Prog_l_pure(nanos);
}

static BValue bsts_core_sleep_effect(BValue duration)
{
  // Struct-1 values are represented as identity in C codegen.
  BValue nanos_value = duration;
  BValue zero = bsts_integer_from_int(0);
  if (bsts_integer_cmp(nanos_value, zero) < 0)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_invalid_argument("sleep duration must be >= 0"));
  }

  uint64_t nanos = bsts_integer_to_low_uint64(nanos_value);
  struct timespec ts;
  ts.tv_sec = (time_t)(nanos / 1000000000ULL);
  ts.tv_nsec = (long)(nanos % 1000000000ULL);

  errno = 0;
  if (nanosleep(&ts, NULL) != 0)
  {
#ifdef EINTR
    if (errno == EINTR)
    {
      return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
          bsts_ioerror_known(BSTS_IOERR_Interrupted, "sleep interrupted"));
    }
#endif
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, "sleep"));
  }

  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_unit_value());
}

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_path__sep()
{
  static const BSTS_String sep = BSTS_STATIC_STRING_INIT(1, "/");
  return BSTS_VALUE_FROM_PTR(&sep);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_stdin()
{
  return bsts_core_make_handle(BSTS_HANDLE_STDIN, stdin, 1, 0, 0);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_stdout()
{
  return bsts_core_make_handle(BSTS_HANDLE_STDOUT, stdout, 0, 1, 0);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_stderr()
{
  return bsts_core_make_handle(BSTS_HANDLE_STDERR, stderr, 0, 1, 0);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_read__utf8(BValue h, BValue max_chars)
{
  return bsts_prog_effect2(h, max_chars, bsts_core_read_utf8_effect);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_write__utf8(BValue h, BValue s)
{
  return bsts_prog_effect2(h, s, bsts_core_write_utf8_effect);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_read__bytes(BValue h, BValue max_bytes)
{
  return bsts_prog_effect2(h, max_bytes, bsts_core_read_bytes_effect);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_write__bytes(BValue h, BValue bytes)
{
  return bsts_prog_effect2(h, bytes, bsts_core_write_bytes_effect);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_read__all__bytes(BValue h, BValue chunk_size)
{
  return bsts_prog_effect2(h, chunk_size, bsts_core_read_all_bytes_effect);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_copy__bytes(BValue src, BValue dst, BValue chunk_size, BValue max_total)
{
  return bsts_prog_effect4(src, dst, chunk_size, max_total, bsts_core_copy_bytes_effect);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_flush(BValue h)
{
  return bsts_prog_effect1(h, bsts_core_flush_effect);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_close(BValue h)
{
  return bsts_prog_effect1(h, bsts_core_close_effect);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_open__file(BValue path, BValue mode)
{
  return bsts_prog_effect2(path, mode, bsts_core_open_file_effect);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_create__temp__file(BValue dir, BValue prefix, BValue suffix)
{
  return bsts_prog_effect3(dir, prefix, suffix, bsts_core_create_temp_file_effect);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_create__temp__dir(BValue dir, BValue prefix)
{
  return bsts_prog_effect2(dir, prefix, bsts_core_create_temp_dir_effect);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_list__dir(BValue path)
{
  return bsts_prog_effect1(path, bsts_core_list_dir_effect);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_stat(BValue path)
{
  return bsts_prog_effect1(path, bsts_core_stat_effect);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_mkdir(BValue path, BValue recursive)
{
  return bsts_prog_effect2(path, recursive, bsts_core_mkdir_effect);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_remove(BValue path, BValue recursive)
{
  return bsts_prog_effect2(path, recursive, bsts_core_remove_effect);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_rename(BValue from, BValue to)
{
  return bsts_prog_effect2(from, to, bsts_core_rename_effect);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_get__env(BValue name)
{
  return bsts_prog_effect1(name, bsts_core_get_env_effect);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_spawn(BValue cmd, BValue args, BValue stdio)
{
  return bsts_prog_effect3(cmd, args, stdio, bsts_core_spawn_effect);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_wait(BValue process)
{
  return bsts_prog_effect1(process, bsts_core_wait_effect);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_now__wall()
{
  return bsts_prog_effect1(bsts_unit_value(), bsts_core_now_wall_effect);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_now__mono()
{
  return bsts_prog_effect1(bsts_unit_value(), bsts_core_now_mono_effect);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_sleep(BValue duration)
{
  return bsts_prog_effect1(duration, bsts_core_sleep_effect);
}
