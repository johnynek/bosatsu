#include "bosatsu_ext_Bosatsu_l_IO_l_Core.h"
#include "bosatsu_ext_Bosatsu_l_Prog.h"

#include <dirent.h>
#include <errno.h>
#include <gc.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>

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

static int bsts_join_path(char **out_path, const char *base, const char *name)
{
  size_t base_len = strlen(base);
  size_t name_len = strlen(name);
  int need_sep = (base_len > 0 && base[base_len - 1] != '/');
  size_t total = base_len + (size_t)(need_sep ? 1 : 0) + name_len;
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

  char *path = bsts_path_to_cstr(path_value);
  if (!path)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, "opening file"));
  }

  ENUM_TAG mode_tag = get_variant(mode_value);
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
  default:
    free(path);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_invalid_argument("invalid OpenMode value"));
  }

  errno = 0;
  FILE *file = fopen(path, open_mode);
  if (!file)
  {
    BValue err = bsts_ioerror_from_errno_default(errno, "opening file");
    free(path);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(err);
  }

  free(path);
  BValue handle = bsts_core_make_handle(BSTS_HANDLE_FILE, file, readable, writable, 1);
  return ___bsts_g_Bosatsu_l_Prog_l_pure(handle);
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
