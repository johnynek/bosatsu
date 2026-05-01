#include "bosatsu_ext_Bosatsu_l_IO_l_Core.h"
#include "bosatsu_ext_Bosatsu_l_IO_l_Bytes.h"
#include "bosatsu_ext_Bosatsu_l_Prog.h"
#include "bosatsu_ext_Bosatsu_l_Prog_internal.h"

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
#if defined(__linux__)
#include <sys/syscall.h>
#endif
#include <time.h>
#include <unistd.h>
#include <uv.h>

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

#define BSTS_POSIX_MODE_MASK 07777
#define BSTS_OWNER_WRITE_EXECUTE_MASK 0300
#define BSTS_UV_IO_CHUNK_MAX ((unsigned int)INT_MAX)

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
  uv_file file;
  FILE *stdio_file;
  int readable;
  int writable;
  int close_on_close;
  int closed;
} BSTS_Core_Handle;

typedef struct
{
  uv_fs_t req;
  BSTS_Prog_Suspended *suspended;
  BSTS_Core_Handle *handle;
  BValue success;
  int mark_closed;
  char context[512];
} BSTS_Core_Fs_Request;

typedef struct
{
  uv_timer_t timer;
  BSTS_Prog_Suspended *suspended;
  uint64_t timeout_millis;
  BValue error;
  _Bool is_error;
} BSTS_Core_Sleep_Request;

#define BSTS_CORE_PROCESS_MAGIC 0x42505350u

typedef struct BSTS_Core_Process
{
  uint32_t magic;
  uv_process_t process;
  struct BSTS_Core_Process *active_prev;
  struct BSTS_Core_Process *active_next;
  int exited;
  int exit_code;
  int term_signal;
  int wait_consumed;
  int close_started;
  BSTS_Prog_Suspended *wait_suspended;
} BSTS_Core_Process;

static BSTS_Core_Process *bsts_core_active_processes = NULL;

typedef struct
{
  BValue args3;
  BSTS_Prog_Suspended *suspended;
  BSTS_Core_Process *process;
  char *cmd;
  char **argv;
  int argc;
  unsigned int stdio_flags[3];
  uv_file stdio_fds[3];
  uv_file pipe_parent_fds[3];
  uv_file pipe_child_fds[3];
  BSTS_Core_Handle *pipe_parent_handles[3];
  BValue stdio_results[3];
} BSTS_Core_Spawn_Request;

typedef struct
{
  BSTS_Core_Process *process;
  BSTS_Prog_Suspended *suspended;
} BSTS_Core_Wait_Request;

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

static BValue bsts_ioerror_from_uv(int err, const char *context)
{
  if (err < 0)
  {
    return bsts_ioerror_from_errno(-err, context);
  }
  return bsts_ioerror_from_errno_default(err, context);
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

#if !defined(__APPLE__) && !defined(__linux__)
static inline BValue bsts_ioerror_unsupported(const char *context)
{
  return bsts_ioerror_known(BSTS_IOERR_Unsupported, context);
}
#endif

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

static BSTS_Core_Handle *bsts_core_unbox_handle(BValue handle)
{
  return BSTS_PTR(BSTS_Core_Handle, handle);
}

static BValue bsts_core_make_handle(
    BSTS_Handle_Kind kind,
    uv_file file,
    FILE *stdio_file,
    int readable,
    int writable,
    int close_on_close)
{
  // This allocation does not cross an unboxed Bosatsu object: all inputs are
  // primitive or native handles, and the returned handle is rooted by callers.
  BSTS_Core_Handle *h = (BSTS_Core_Handle *)GC_malloc(sizeof(BSTS_Core_Handle));
  if (h == NULL)
  {
    perror("GC_malloc failure in bsts_core_make_handle");
    abort();
  }
  h->kind = kind;
  h->file = file;
  h->stdio_file = stdio_file;
  h->readable = readable;
  h->writable = writable;
  h->close_on_close = close_on_close;
  h->closed = 0;
  return BSTS_VALUE_FROM_PTR(h);
}

static void bsts_core_fs_request_resume(uv_fs_t *req)
{
  BSTS_Core_Fs_Request *request = (BSTS_Core_Fs_Request *)req->data;
  ssize_t result = req->result;
  uv_fs_req_cleanup(req);

  if (result < 0)
  {
    bsts_Bosatsu_Prog_suspended_error(
        request->suspended,
        bsts_ioerror_from_uv((int)result, request->context));
    return;
  }

  if (request->mark_closed && request->handle != NULL)
  {
    request->handle->closed = 1;
    request->handle->file = -1;
  }
  bsts_Bosatsu_Prog_suspended_success(request->suspended, request->success);
}

static ssize_t bsts_core_uv_fs_cleanup_result(uv_fs_t *req, const char *context)
{
  ssize_t result = req->result;
  uv_fs_req_cleanup(req);
  if (result < 0)
  {
    errno = -((int)result);
    (void)context;
    return -1;
  }
  return result;
}

static ssize_t bsts_core_uv_fs_cleanup_start_result(int start, uv_fs_t *req, const char *context)
{
  ssize_t result = (start < 0) ? (ssize_t)start : req->result;
  uv_fs_req_cleanup(req);
  if (result < 0)
  {
    errno = -((int)result);
    (void)context;
    return -1;
  }
  return result;
}

static unsigned int bsts_core_uv_io_chunk_size(size_t len)
{
  return (len > (size_t)BSTS_UV_IO_CHUNK_MAX)
      ? BSTS_UV_IO_CHUNK_MAX
      : (unsigned int)len;
}

unsigned int bsts_core_test_uv_io_chunk_size(size_t len)
{
  return bsts_core_uv_io_chunk_size(len);
}

static ssize_t bsts_core_uv_read(uv_file file, void *data, size_t len, const char *context)
{
  uv_buf_t buf = uv_buf_init((char *)data, bsts_core_uv_io_chunk_size(len));
  uv_fs_t req;
  int start = uv_fs_read(NULL, &req, file, &buf, 1, -1, NULL);
  return bsts_core_uv_fs_cleanup_start_result(start, &req, context);
}

static int bsts_core_uv_write_all(uv_file file, const void *data, size_t len, const char *context)
{
  const char *cursor = (const char *)data;
  size_t remaining = len;
  while (remaining > 0U)
  {
    uv_buf_t buf = uv_buf_init((char *)cursor, bsts_core_uv_io_chunk_size(remaining));
    uv_fs_t req;
    int start = uv_fs_write(NULL, &req, file, &buf, 1, -1, NULL);
    ssize_t wrote = bsts_core_uv_fs_cleanup_start_result(start, &req, context);
    if (wrote < 0)
    {
      return -1;
    }
    if (wrote == 0)
    {
#ifdef EIO
      errno = EIO;
#else
      errno = 0;
#endif
      return -1;
    }
    cursor += wrote;
    remaining -= (size_t)wrote;
  }
  return 0;
}

static int bsts_core_uv_fs_simple(uv_fs_t *req, const char *context)
{
  ssize_t result = bsts_core_uv_fs_cleanup_result(req, context);
  if (result < 0)
  {
    return -1;
  }
  return 0;
}

static int bsts_core_uv_stat_path(const char *path, uv_stat_t *out, int follow)
{
  uv_fs_t req;
  int start = follow
      ? uv_fs_stat(NULL, &req, path, NULL)
      : uv_fs_lstat(NULL, &req, path, NULL);
  if (start < 0)
  {
    return bsts_core_uv_fs_cleanup_start_result(start, &req, "stating path");
  }

  *out = *uv_fs_get_statbuf(&req);
  uv_fs_req_cleanup(&req);
  return 0;
}

static int bsts_core_uv_mkdir_path(const char *path, int mode_bits)
{
  uv_fs_t req;
  int start = uv_fs_mkdir(NULL, &req, path, mode_bits, NULL);
  if (start < 0)
  {
    return bsts_core_uv_fs_cleanup_start_result(start, &req, "creating directory");
  }
  return bsts_core_uv_fs_simple(&req, "creating directory");
}

static int bsts_core_uv_chmod_path(const char *path, int mode_bits)
{
  uv_fs_t req;
  int start = uv_fs_chmod(NULL, &req, path, mode_bits, NULL);
  if (start < 0)
  {
    return bsts_core_uv_fs_cleanup_start_result(start, &req, "setting path mode");
  }
  return bsts_core_uv_fs_simple(&req, "setting path mode");
}

static int bsts_core_uv_unlink_path(const char *path)
{
  uv_fs_t req;
  int start = uv_fs_unlink(NULL, &req, path, NULL);
  if (start < 0)
  {
    return bsts_core_uv_fs_cleanup_start_result(start, &req, "removing file path");
  }
  return bsts_core_uv_fs_simple(&req, "removing file path");
}

static int bsts_core_uv_rmdir_path(const char *path)
{
  uv_fs_t req;
  int start = uv_fs_rmdir(NULL, &req, path, NULL);
  if (start < 0)
  {
    return bsts_core_uv_fs_cleanup_start_result(start, &req, "removing directory path");
  }
  return bsts_core_uv_fs_simple(&req, "removing directory path");
}

static int bsts_core_uv_rename_path(const char *from, const char *to)
{
  uv_fs_t req;
  int start = uv_fs_rename(NULL, &req, from, to, NULL);
  if (start < 0)
  {
    return bsts_core_uv_fs_cleanup_start_result(start, &req, "renaming path");
  }
  return bsts_core_uv_fs_simple(&req, "renaming path");
}

static int bsts_core_close_start(BSTS_Prog_Suspended *suspended)
{
  BSTS_Core_Fs_Request *request =
      BSTS_PTR(BSTS_Core_Fs_Request, bsts_Bosatsu_Prog_suspended_request(suspended));
  request->suspended = suspended;
  return uv_fs_close(
      bsts_Bosatsu_Prog_suspended_loop(suspended),
      &request->req,
      request->handle->file,
      bsts_core_fs_request_resume);
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

static void bsts_core_free_spawn_request_strings(BSTS_Core_Spawn_Request *request)
{
  if (request->argv != NULL)
  {
    for (int i = 0; i < request->argc; i++)
    {
      free(request->argv[i]);
    }
    free(request->argv);
    request->argv = NULL;
  }
  free(request->cmd);
  request->cmd = NULL;
}

static int bsts_core_list_length(BValue list, int *out)
{
  int len = 0;
  BValue cursor = list;
  while (1)
  {
    ENUM_TAG tag = get_variant(cursor);
    if (tag == 0)
    {
      *out = len;
      return 1;
    }
    if (tag != 1 || len == INT_MAX)
    {
      return 0;
    }
    len++;
    cursor = get_enum_index(cursor, 1);
  }
}

static int bsts_core_process_args_to_argv(
    BValue cmd_value,
    BValue args_value,
    BSTS_Core_Spawn_Request *request)
{
  int arg_count = 0;
  if (!bsts_core_list_length(args_value, &arg_count))
  {
    return 0;
  }

  request->cmd = bsts_string_to_cstr(cmd_value);
  if (request->cmd == NULL)
  {
    return 0;
  }

  request->argc = arg_count + 1;
  request->argv = (char **)calloc((size_t)request->argc + 1U, sizeof(char *));
  if (request->argv == NULL)
  {
    return 0;
  }

  request->argv[0] = bsts_string_to_cstr(cmd_value);
  if (request->argv[0] == NULL)
  {
    return 0;
  }

  BValue cursor = args_value;
  for (int i = 1; i < request->argc; i++)
  {
    request->argv[i] = bsts_string_to_cstr(get_enum_index(cursor, 0));
    if (request->argv[i] == NULL)
    {
      return 0;
    }
    cursor = get_enum_index(cursor, 1);
  }
  request->argv[request->argc] = NULL;
  return 1;
}

static void bsts_core_close_fd_if_open(uv_file *fd)
{
  if (*fd >= 0)
  {
    uv_fs_t req;
    int start = uv_fs_close(NULL, &req, *fd, NULL);
    (void)bsts_core_uv_fs_cleanup_start_result(start, &req, "closing process stdio");
    *fd = -1;
  }
}

static void bsts_core_spawn_close_child_pipe_ends(BSTS_Core_Spawn_Request *request)
{
  for (int i = 0; i < 3; i++)
  {
    bsts_core_close_fd_if_open(&request->pipe_child_fds[i]);
  }
}

static void bsts_core_spawn_close_parent_pipe_ends(BSTS_Core_Spawn_Request *request)
{
  for (int i = 0; i < 3; i++)
  {
    bsts_core_close_fd_if_open(&request->pipe_parent_fds[i]);
    if (request->pipe_parent_handles[i] != NULL)
    {
      request->pipe_parent_handles[i]->closed = 1;
      request->pipe_parent_handles[i]->file = -1;
    }
  }
}

static int bsts_core_set_close_on_exec(uv_file fd)
{
  int flags = fcntl((int)fd, F_GETFD);
  if (flags < 0)
  {
    return -1;
  }
  return fcntl((int)fd, F_SETFD, flags | FD_CLOEXEC);
}

static int bsts_core_pipe_cloexec(int fds[2])
{
#if defined(__linux__) && defined(SYS_pipe2) && defined(O_CLOEXEC)
  if (syscall(SYS_pipe2, fds, O_CLOEXEC) == 0)
  {
    return 0;
  }
  if (errno != ENOSYS)
  {
    return -1;
  }
#endif

  if (pipe(fds) != 0)
  {
    return -1;
  }
  if (bsts_core_set_close_on_exec((uv_file)fds[0]) != 0 ||
      bsts_core_set_close_on_exec((uv_file)fds[1]) != 0)
  {
    int saved_errno = errno;
    (void)close(fds[0]);
    (void)close(fds[1]);
    errno = saved_errno;
    return -1;
  }
  return 0;
}

static int bsts_core_spawn_pipe_for_stdio(BSTS_Core_Spawn_Request *request, int index)
{
  int fds[2] = {-1, -1};
  if (bsts_core_pipe_cloexec(fds) != 0)
  {
    return 0;
  }

  int parent_index = (index == 0) ? 1 : 0;
  int child_index = (index == 0) ? 0 : 1;
  uv_file parent_fd = (uv_file)fds[parent_index];
  uv_file child_fd = (uv_file)fds[child_index];

  int parent_readable = (index != 0);
  int parent_writable = (index == 0);
  BValue handle_value = bsts_core_make_handle(
      BSTS_HANDLE_FILE,
      parent_fd,
      NULL,
      parent_readable,
      parent_writable,
      1);
  request->pipe_parent_fds[index] = parent_fd;
  request->pipe_child_fds[index] = child_fd;
  request->pipe_parent_handles[index] = bsts_core_unbox_handle(handle_value);
  request->stdio_results[index] = bsts_option_some(handle_value);
  request->stdio_flags[index] = UV_INHERIT_FD;
  request->stdio_fds[index] = child_fd;
  return 1;
}

static int bsts_core_spawn_use_handle_for_stdio(
    BSTS_Core_Spawn_Request *request,
    int index,
    BValue stdio_value,
    BValue *error)
{
  BValue handle_value = get_enum_index(stdio_value, 0);
  BSTS_Core_Handle *handle = bsts_core_unbox_handle(handle_value);
  if (handle->closed)
  {
    *error = bsts_ioerror_bad_fd("spawn stdio handle is closed");
    return 0;
  }
  if (index == 0 && !handle->readable)
  {
    *error = bsts_ioerror_bad_fd("spawn stdin handle must be readable");
    return 0;
  }
  if (index != 0 && !handle->writable)
  {
    *error = bsts_ioerror_bad_fd("spawn output handle must be writable");
    return 0;
  }

  request->stdio_flags[index] = UV_INHERIT_FD;
  request->stdio_fds[index] = handle->file;
  request->stdio_results[index] = bsts_option_none();
  return 1;
}

static int bsts_core_decode_stdio_config(
    BValue stdio_config,
    BSTS_Core_Spawn_Request *request,
    BValue *error)
{
  for (int i = 0; i < 3; i++)
  {
    BValue stdio_value = get_struct_index(stdio_config, i);
    request->stdio_results[i] = bsts_option_none();
    switch (get_variant(stdio_value))
    {
    case 0: // Inherit
      request->stdio_flags[i] = UV_INHERIT_FD;
      request->stdio_fds[i] = (uv_file)i;
      break;
    case 1: // Pipe
      if (!bsts_core_spawn_pipe_for_stdio(request, i))
      {
        *error = bsts_ioerror_from_errno_default(errno, "creating process stdio pipe");
        return 0;
      }
      break;
    case 2: // Null
      request->stdio_flags[i] = UV_IGNORE;
      break;
    case 3: // UseHandle
      if (!bsts_core_spawn_use_handle_for_stdio(request, i, stdio_value, error))
      {
        return 0;
      }
      break;
    default:
      *error = bsts_ioerror_invalid_argument("spawn stdio config has invalid Stdio tag");
      return 0;
    }
  }
  return 1;
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

static int bsts_posix_mode_arg(BValue value, int *out)
{
  BValue zero = bsts_integer_from_int(0);
  if (bsts_integer_cmp(value, zero) < 0)
  {
    return 0;
  }

  BValue max_mode = bsts_integer_from_int(BSTS_POSIX_MODE_MASK);
  if (bsts_integer_cmp(value, max_mode) > 0)
  {
    return 0;
  }

  *out = (int)bsts_integer_to_int32(value);
  return 1;
}

static int bsts_existing_directory(const char *path, int leaf)
{
  uv_stat_t st;
  /* mkdir -p should accept symlinked directory components while walking. */
  if (bsts_core_uv_stat_path(path, &st, 1) != 0)
  {
    return -1;
  }

  if (S_ISDIR(st.st_mode))
  {
    return 0;
  }

  errno = leaf ? EEXIST : ENOTDIR;
  return -1;
}

static int bsts_set_mode_bits(const char *path, int mode_bits)
{
  return bsts_core_uv_chmod_path(path, mode_bits & BSTS_POSIX_MODE_MASK);
}

static int bsts_repair_parent_mode(const char *path)
{
  uv_stat_t st;
  if (bsts_core_uv_stat_path(path, &st, 0) != 0)
  {
    return -1;
  }

  int current_bits = (int)(st.st_mode & BSTS_POSIX_MODE_MASK);
  int repaired_bits = current_bits | BSTS_OWNER_WRITE_EXECUTE_MASK;
  if (repaired_bits == current_bits)
  {
    return 0;
  }

  return bsts_set_mode_bits(path, repaired_bits);
}

static int bsts_mkdirs_with_mode(const char *path, int leaf_mode_bits, int apply_mode)
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
        if (bsts_core_uv_mkdir_path(copy, 0777) != 0)
        {
          if (errno != EEXIST || bsts_existing_directory(copy, 0) != 0)
          {
            free(copy);
            return -1;
          }
        }
        else if (apply_mode && bsts_repair_parent_mode(copy) != 0)
        {
          /* Keep newly created parents traversable after umask masking. */
          free(copy);
          return -1;
        }
      }
      *p = '/';
    }
  }

  if (bsts_core_uv_mkdir_path(copy, apply_mode ? leaf_mode_bits : 0777) != 0)
  {
    if (errno != EEXIST || bsts_existing_directory(copy, 1) != 0)
    {
      free(copy);
      return -1;
    }
  }
  else if (apply_mode && bsts_set_mode_bits(copy, leaf_mode_bits) != 0)
  {
    free(copy);
    return -1;
  }

  free(copy);
  return 0;
}

static int bsts_remove_recursive_impl(const char *path)
{
  uv_stat_t st;
  if (bsts_core_uv_stat_path(path, &st, 0) != 0)
  {
    return -1;
  }

  if (S_ISDIR(st.st_mode) && !S_ISLNK(st.st_mode))
  {
    uv_fs_t req;
    int scan_start = uv_fs_scandir(NULL, &req, path, 0, NULL);
    if (scan_start < 0)
    {
      ssize_t cleanup_result = bsts_core_uv_fs_cleanup_start_result(scan_start, &req, "scanning directory");
      return (cleanup_result < 0) ? -1 : 0;
    }
    if (req.result < 0)
    {
      ssize_t status = bsts_core_uv_fs_cleanup_start_result(scan_start, &req, "scanning directory");
      (void)status;
      return -1;
    }

    uv_dirent_t entry;
    int next_status = 0;
    while ((next_status = uv_fs_scandir_next(&req, &entry)) != UV_EOF)
    {
      if (next_status < 0)
      {
        errno = -next_status;
        uv_fs_req_cleanup(&req);
        return -1;
      }

      const char *name = entry.name;
      if (strcmp(name, ".") == 0 || strcmp(name, "..") == 0)
      {
        continue;
      }

      char *child = NULL;
      if (!bsts_join_path(&child, path, name))
      {
        uv_fs_req_cleanup(&req);
        return -1;
      }

      if (bsts_remove_recursive_impl(child) != 0)
      {
        free(child);
        uv_fs_req_cleanup(&req);
        return -1;
      }
      free(child);
    }

    uv_fs_req_cleanup(&req);

    return bsts_core_uv_rmdir_path(path);
  }

  return bsts_core_uv_unlink_path(path);
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
  ssize_t read_count = bsts_core_uv_read(handle->file, buf, (size_t)max_chars, "reading utf8");
  if (read_count == 0)
  {
    free(buf);
    return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_option_none());
  }
  if (read_count < 0)
  {
    free(buf);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, "reading utf8"));
  }

  int read_len = (int)read_count;
  if (!bsts_utf8_is_valid_prefix(buf, read_len))
  {
    free(buf);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_invalid_utf8("decoding bytes from handle"));
  }

  BValue text = bsts_string_from_utf8_bytes_copy((size_t)read_len, buf);
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
    if (bsts_core_uv_write_all(handle->file, view.bytes, view.len, "writing utf8") != 0)
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
  ssize_t read_count = bsts_core_uv_read(handle->file, buf, (size_t)max_bytes, "reading bytes");
  if (read_count == 0)
  {
    free(buf);
    return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_option_none());
  }
  if (read_count < 0)
  {
    free(buf);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, "reading bytes"));
  }

  int read_len = (int)read_count;
  // Keep pair reachable across the GC allocation below: it owns the handle
  // BValue we unboxed before reading, and the atomic data allocation cannot
  // contain references back to Bosatsu values.
  uint8_t *data = (uint8_t *)GC_malloc_atomic((size_t)read_len);
  if (data == NULL)
  {
    free(buf);
    perror("GC_malloc_atomic failure in bsts_core_read_bytes_effect");
    abort();
  }
  memcpy(data, buf, (size_t)read_len);
  free(buf);

  BValue bytes = bsts_bytes_wrap(data, 0, read_len);
  GC_reachable_here(pair);
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
    if (bsts_core_uv_write_all(
            handle->file,
            bytes->data + bytes->offset,
            (size_t)bytes->len,
            "writing bytes") != 0)
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
    ssize_t read_count = bsts_core_uv_read(handle->file, chunk_buf, (size_t)chunk_size, "reading bytes");
    if (read_count == 0)
    {
      break;
    }
    if (read_count < 0)
    {
      free(chunk_buf);
      free(acc);
      return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
          bsts_ioerror_from_errno_default(errno, "reading bytes"));
    }

    if (total > (size_t)INT_MAX - (size_t)read_count)
    {
      free(chunk_buf);
      free(acc);
      return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
          bsts_ioerror_invalid_argument("read_all_bytes result too large"));
    }

    size_t needed = total + (size_t)read_count;
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

    memcpy(acc + total, chunk_buf, (size_t)read_count);
    total += (size_t)read_count;
  }

  free(chunk_buf);

  if (total == 0)
  {
    free(acc);
    return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_bytes_empty());
  }

  // Keep pair reachable across the GC allocation below: it owns the handle
  // BValue used for the completed reads, while this atomic buffer stores only
  // raw byte payload.
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
  GC_reachable_here(pair);
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
    ssize_t read_count = bsts_core_uv_read(src->file, buf, (size_t)to_read, "reading bytes");
    if (read_count == 0)
    {
      break;
    }
    if (read_count < 0)
    {
      free(buf);
      return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
          bsts_ioerror_from_errno_default(errno, "reading bytes"));
    }

    errno = 0;
    if (bsts_core_uv_write_all(dst->file, buf, (size_t)read_count, "writing bytes") != 0)
    {
      free(buf);
      return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
          bsts_ioerror_from_errno_default(errno, "writing bytes"));
    }

    copied = bsts_integer_add(copied, bsts_integer_from_int((int)read_count));
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

  if (!handle->close_on_close)
  {
    // Process-owned standard streams keep stdio buffering; libuv owns only runtime file descriptors.
    errno = 0;
    if (handle->stdio_file != NULL && fflush(handle->stdio_file) != 0)
    {
      return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
          bsts_ioerror_from_errno_default(errno, "flushing handle"));
    }
    return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_unit_value());
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
    // Keep handle_value reachable across request allocation and setup. The
    // request is GC-managed and stores handle before suspension roots it.
    BSTS_Core_Fs_Request *request =
        (BSTS_Core_Fs_Request *)GC_malloc(sizeof(BSTS_Core_Fs_Request));
    if (request == NULL)
    {
      perror("GC_malloc failure in bsts_core_close_effect");
      abort();
    }
    request->handle = handle;
    request->success = bsts_unit_value();
    request->mark_closed = 1;
    bsts_contextf(request->context, sizeof(request->context), "closing handle");
    request->req.data = request;
    BValue suspended = bsts_Bosatsu_Prog_suspend(BSTS_VALUE_FROM_PTR(request), bsts_core_close_start);
    GC_reachable_here(handle_value);
    return suspended;
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
  int open_flags = 0;
  int readable = 0;
  int writable = 0;

  switch (mode_tag)
  {
  case 0: // Read
    open_flags = O_RDONLY;
    readable = 1;
    break;
  case 1: // WriteTruncate
    open_flags = O_WRONLY | O_CREAT | O_TRUNC;
    writable = 1;
    break;
  case 2: // Append
    open_flags = O_WRONLY | O_CREAT | O_APPEND;
    writable = 1;
    break;
  case 3: // CreateNew
    open_flags = O_WRONLY | O_CREAT | O_EXCL;
    writable = 1;
    break;
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
  bsts_contextf(
      context,
      sizeof(context),
      "open_file(path=%s, mode=%s): uv_fs_open failed",
      path,
      mode_name);
  uv_fs_t req;
  int start = uv_fs_open(NULL, &req, path, open_flags, 0666, NULL);
  if (start < 0)
  {
    ssize_t result = bsts_core_uv_fs_cleanup_start_result(start, &req, context);
    (void)result;
    BValue err = bsts_ioerror_from_uv(start, context);
    free(path);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(err);
  }
  ssize_t fd = bsts_core_uv_fs_cleanup_result(&req, context);
  if (fd < 0 || fd > (ssize_t)INT_MAX)
  {
    BValue err = (fd > (ssize_t)INT_MAX)
        ? bsts_ioerror_invalid_argument("open_file returned an unsupported file descriptor")
        : bsts_ioerror_from_errno_default(errno, context);
    free(path);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(err);
  }

  free(path);
  BValue handle = bsts_core_make_handle(BSTS_HANDLE_FILE, (uv_file)fd, NULL, readable, writable, 1);
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

  BValue path_out = bsts_path_from_cstr(template_path);
  BValue handle_out = bsts_core_make_handle(BSTS_HANDLE_FILE, (uv_file)fd, NULL, 0, 1, 1);
  BValue out = alloc_struct2(path_out, handle_out);

  // mkstemp/mkstemps provide the compatibility name contract; the returned handle is uv_file-backed.
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
  /* libuv has no mkdtemp equivalent that preserves the prefix contract. */
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

  uv_fs_t req;
  int scan_start = uv_fs_scandir(NULL, &req, path, 0, NULL);
  if (scan_start < 0)
  {
    ssize_t result = bsts_core_uv_fs_cleanup_start_result(scan_start, &req, "listing directory");
    (void)result;
    BValue err = bsts_ioerror_from_errno_default(errno, "listing directory");
    free(path);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(err);
  }
  if (req.result < 0)
  {
    ssize_t result = bsts_core_uv_fs_cleanup_start_result(scan_start, &req, "listing directory");
    (void)result;
    BValue err = bsts_ioerror_from_errno_default(errno, "listing directory");
    free(path);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(err);
  }

  size_t cap = 16;
  size_t count = 0;
  char **items = (char **)malloc(sizeof(char *) * cap);
  if (!items)
  {
    uv_fs_req_cleanup(&req);
    free(path);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, "allocating directory list"));
  }

  uv_dirent_t entry;
  int next_status = 0;
  while ((next_status = uv_fs_scandir_next(&req, &entry)) != UV_EOF)
  {
    if (next_status < 0)
    {
      int saved_errno = -next_status;
      for (size_t i = 0; i < count; i++)
      {
        free(items[i]);
      }
      free(items);
      uv_fs_req_cleanup(&req);
      free(path);
      errno = saved_errno;
      return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
          bsts_ioerror_from_errno_default(errno, "reading directory entry"));
    }

    const char *name = entry.name;
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
        uv_fs_req_cleanup(&req);
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
      uv_fs_req_cleanup(&req);
      free(path);
      return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
          bsts_ioerror_from_errno_default(errno, "joining child path"));
    }

    items[count++] = joined;
  }

  uv_fs_req_cleanup(&req);

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

  uv_stat_t st;
  errno = 0;
  if (bsts_core_uv_stat_path(path, &st, 0) != 0)
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
  BValue size_bytes = bsts_integer_from_int64((int64_t)st.st_size);

  int64_t sec = (int64_t)st.st_mtim.tv_sec;
  long nsec = st.st_mtim.tv_nsec;

  BValue sec_i = bsts_integer_from_int64(sec);
  BValue billion = bsts_integer_from_int(1000000000);
  BValue nsec_i = bsts_integer_from_int((int32_t)nsec);
  BValue mtime = bsts_integer_add(bsts_integer_times(sec_i, billion), nsec_i);
  BValue posix_mode = bsts_option_some(
      bsts_integer_from_int((int32_t)(st.st_mode & BSTS_POSIX_MODE_MASK)));

  BValue stat_value = alloc_struct4(kind, size_bytes, mtime, posix_mode);
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
  int status = recursive
      ? bsts_mkdirs_with_mode(path, 0, 0)
      : bsts_core_uv_mkdir_path(path, 0777);
  if (status != 0)
  {
    BValue err = bsts_ioerror_from_errno_default(errno, "creating directory");
    free(path);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(err);
  }

  free(path);
  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_unit_value());
}

static BValue bsts_core_mkdir_with_mode_effect(BValue args)
{
  BValue path_value = get_struct_index(args, 0);
  BValue recursive_value = get_struct_index(args, 1);
  BValue mode_value = get_struct_index(args, 2);

  char *path = bsts_path_to_cstr(path_value);
  if (!path)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, "creating directory with mode"));
  }

  int mode_bits = 0;
  if (!bsts_posix_mode_arg(mode_value, &mode_bits))
  {
    free(path);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_invalid_argument("invalid PosixMode for mkdir_with_mode"));
  }

  int recursive = (get_variant(recursive_value) == 1);
  errno = 0;
  int status = -1;
  if (recursive)
  {
    status = bsts_mkdirs_with_mode(path, mode_bits, 1);
  }
  else
  {
    status = bsts_core_uv_mkdir_path(path, mode_bits);
    if (status == 0)
    {
      /* chmod repairs umask-masked mode bits to match the existing contract. */
      status = bsts_set_mode_bits(path, mode_bits);
    }
  }

  if (status != 0)
  {
    BValue err =
        bsts_ioerror_from_errno_default(errno, "creating directory with mode");
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
    uv_stat_t st;
    if (bsts_core_uv_stat_path(path, &st, 0) != 0)
    {
      status = -1;
    }
    else if (S_ISDIR(st.st_mode) && !S_ISLNK(st.st_mode))
    {
      status = bsts_core_uv_rmdir_path(path);
    }
    else
    {
      status = bsts_core_uv_unlink_path(path);
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
  if (bsts_core_uv_rename_path(from, to) != 0)
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

  char stack_value[256];
  size_t size = sizeof(stack_value);
  int getenv_result = uv_os_getenv(name, stack_value, &size);
  if (getenv_result == UV_ENOENT)
  {
    free(name);
    return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_option_none());
  }

  if (getenv_result == UV_ENOBUFS)
  {
    size_t heap_capacity = size;
    char *heap_value = (char *)malloc(heap_capacity);
    if (!heap_value)
    {
      free(name);
      return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
          bsts_ioerror_from_errno_default(errno, "reading environment"));
    }

    size = heap_capacity;
    getenv_result = uv_os_getenv(name, heap_value, &size);
    free(name);
    if (getenv_result != 0)
    {
      free(heap_value);
      return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
          bsts_ioerror_from_uv(getenv_result, "reading environment"));
    }

    BValue v = bsts_string_from_utf8_bytes_copy(strlen(heap_value), heap_value);
    free(heap_value);
    return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_option_some(v));
  }

  free(name);
  if (getenv_result != 0)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_uv(getenv_result, "reading environment"));
  }

  BValue v = bsts_string_from_utf8_bytes_copy(strlen(stack_value), stack_value);
  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_option_some(v));
}

static BSTS_Core_Process *bsts_core_unbox_process(BValue process_value)
{
  if ((process_value & (BValue)0x1) != (BValue)0x0)
  {
    return NULL;
  }
  BSTS_Core_Process *process = BSTS_PTR(BSTS_Core_Process, process_value);
  if (process == NULL || process->magic != BSTS_CORE_PROCESS_MAGIC)
  {
    return NULL;
  }
  return process;
}

static BValue bsts_core_process_exit_value(BSTS_Core_Process *process)
{
  int code = process->exit_code;
  if (process->term_signal != 0)
  {
    code = 128 + process->term_signal;
  }
  return bsts_integer_from_int(code);
}

static void bsts_core_process_root_active(BSTS_Core_Process *process)
{
  process->active_prev = NULL;
  process->active_next = bsts_core_active_processes;
  if (bsts_core_active_processes != NULL)
  {
    bsts_core_active_processes->active_prev = process;
  }
  bsts_core_active_processes = process;
}

static void bsts_core_process_unroot_active(BSTS_Core_Process *process)
{
  if (process->active_prev != NULL)
  {
    process->active_prev->active_next = process->active_next;
  }
  else if (bsts_core_active_processes == process)
  {
    bsts_core_active_processes = process->active_next;
  }
  else
  {
    return;
  }

  if (process->active_next != NULL)
  {
    process->active_next->active_prev = process->active_prev;
  }
  process->active_prev = NULL;
  process->active_next = NULL;
}

static void bsts_core_process_close_cb(uv_handle_t *handle)
{
  BSTS_Core_Process *process = (BSTS_Core_Process *)handle->data;
  if (process != NULL)
  {
    bsts_core_process_unroot_active(process);
  }
}

static void bsts_core_process_maybe_close(BSTS_Core_Process *process)
{
  if (!process->close_started)
  {
    process->close_started = 1;
    uv_close((uv_handle_t *)&process->process, bsts_core_process_close_cb);
  }
}

static void bsts_core_process_exit_cb(
    uv_process_t *uv_process,
    int64_t exit_status,
    int term_signal)
{
  BSTS_Core_Process *process = (BSTS_Core_Process *)uv_process->data;
  process->exited = 1;
  process->exit_code = (int)exit_status;
  process->term_signal = term_signal;

  BSTS_Prog_Suspended *suspended = process->wait_suspended;
  if (suspended != NULL)
  {
    process->wait_suspended = NULL;
    process->wait_consumed = 1;
    bsts_Bosatsu_Prog_suspended_success(
        suspended,
        bsts_core_process_exit_value(process));
  }
  bsts_core_process_maybe_close(process);
}

static BValue bsts_core_process_spawn_result(BSTS_Core_Spawn_Request *request)
{
  return alloc_struct4(
      BSTS_VALUE_FROM_PTR(request->process),
      request->stdio_results[0],
      request->stdio_results[1],
      request->stdio_results[2]);
}

static int bsts_core_spawn_start(BSTS_Prog_Suspended *suspended)
{
  BSTS_Core_Spawn_Request *request =
      BSTS_PTR(BSTS_Core_Spawn_Request, bsts_Bosatsu_Prog_suspended_request(suspended));
  request->suspended = suspended;

  uv_process_options_t options;
  memset(&options, 0, sizeof(options));
  uv_stdio_container_t stdio[3];
  memset(stdio, 0, sizeof(stdio));
  for (int i = 0; i < 3; i++)
  {
    stdio[i].flags = request->stdio_flags[i];
    if (request->stdio_flags[i] == UV_INHERIT_FD)
    {
      stdio[i].data.fd = request->stdio_fds[i];
    }
  }

  request->process->process.data = request->process;
  options.exit_cb = bsts_core_process_exit_cb;
  options.file = request->cmd;
  options.args = request->argv;
  options.stdio_count = 3;
  options.stdio = stdio;

  int spawn_result = uv_spawn(
      bsts_Bosatsu_Prog_suspended_loop(suspended),
      &request->process->process,
      &options);
  bsts_core_spawn_close_child_pipe_ends(request);
  bsts_core_free_spawn_request_strings(request);

  if (spawn_result != 0)
  {
    bsts_core_spawn_close_parent_pipe_ends(request);
    bsts_core_process_root_active(request->process);
    bsts_core_process_maybe_close(request->process);
    bsts_Bosatsu_Prog_suspended_error(
        suspended,
        bsts_ioerror_from_uv(spawn_result, "spawning process"));
    return 0;
  }

  bsts_core_process_root_active(request->process);
  bsts_Bosatsu_Prog_suspended_success(
      suspended,
      bsts_core_process_spawn_result(request));
  return 0;
}

static BValue bsts_core_spawn_effect(BValue args3)
{
  BValue cmd_value = get_struct_index(args3, 0);
  BValue args_value = get_struct_index(args3, 1);
  BValue stdio_value = get_struct_index(args3, 2);

  BSTS_Core_Spawn_Request *request =
      (BSTS_Core_Spawn_Request *)GC_malloc(sizeof(BSTS_Core_Spawn_Request));
  if (request == NULL)
  {
    perror("GC_malloc failure in bsts_core_spawn_effect");
    abort();
  }
  memset(request, 0, sizeof(*request));
  request->args3 = args3;
  for (int i = 0; i < 3; i++)
  {
    request->stdio_fds[i] = -1;
    request->pipe_parent_fds[i] = -1;
    request->pipe_child_fds[i] = -1;
    request->stdio_results[i] = bsts_option_none();
  }

  BSTS_Core_Process *process =
      (BSTS_Core_Process *)GC_malloc(sizeof(BSTS_Core_Process));
  if (process == NULL)
  {
    perror("GC_malloc failure in bsts_core_spawn_effect process");
    abort();
  }
  memset(process, 0, sizeof(*process));
  process->magic = BSTS_CORE_PROCESS_MAGIC;
  request->process = process;

  errno = 0;
  if (!bsts_core_process_args_to_argv(cmd_value, args_value, request))
  {
    bsts_core_free_spawn_request_strings(request);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_errno_default(errno, "building process arguments"));
  }

  BValue stdio_error = bsts_unit_value();
  if (!bsts_core_decode_stdio_config(stdio_value, request, &stdio_error))
  {
    bsts_core_spawn_close_child_pipe_ends(request);
    bsts_core_spawn_close_parent_pipe_ends(request);
    bsts_core_free_spawn_request_strings(request);
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(stdio_error);
  }

  BValue suspended = bsts_Bosatsu_Prog_suspend(
      BSTS_VALUE_FROM_PTR(request),
      bsts_core_spawn_start);
  GC_reachable_here(args3);
  return suspended;
}

static int bsts_core_wait_start(BSTS_Prog_Suspended *suspended)
{
  BSTS_Core_Wait_Request *request =
      BSTS_PTR(BSTS_Core_Wait_Request, bsts_Bosatsu_Prog_suspended_request(suspended));
  request->suspended = suspended;
  request->process->wait_suspended = suspended;
  return 0;
}

static BValue bsts_core_wait_effect(BValue process)
{
  BSTS_Core_Process *process_state = bsts_core_unbox_process(process);
  if (process_state == NULL)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_bad_fd("waiting on invalid process"));
  }

  if (process_state->wait_consumed || process_state->wait_suspended != NULL)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_bad_fd("process wait already consumed"));
  }

  if (process_state->exited)
  {
    process_state->wait_consumed = 1;
    return ___bsts_g_Bosatsu_l_Prog_l_pure(
        bsts_core_process_exit_value(process_state));
  }

  BSTS_Core_Wait_Request *request =
      (BSTS_Core_Wait_Request *)GC_malloc(sizeof(BSTS_Core_Wait_Request));
  if (request == NULL)
  {
    perror("GC_malloc failure in bsts_core_wait_effect");
    abort();
  }
  request->process = process_state;
  request->suspended = NULL;
  BValue suspended = bsts_Bosatsu_Prog_suspend(
      BSTS_VALUE_FROM_PTR(request),
      bsts_core_wait_start);
  GC_reachable_here(process);
  return suspended;
}

static BValue bsts_core_now_wall_effect(BValue unit)
{
  (void)unit;
  uv_timeval64_t tv;
  int time_result = uv_gettimeofday(&tv);
  if (time_result != 0)
  {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
        bsts_ioerror_from_uv(time_result, "reading wall clock"));
  }

  BValue sec_i = bsts_integer_from_int64((int64_t)tv.tv_sec);
  BValue billion = bsts_integer_from_int(1000000000);
  BValue nsec_i = bsts_integer_from_int64((int64_t)tv.tv_usec * 1000);
  BValue nanos = bsts_integer_add(bsts_integer_times(sec_i, billion), nsec_i);
  return ___bsts_g_Bosatsu_l_Prog_l_pure(nanos);
}

static BValue bsts_core_now_mono_effect(BValue unit)
{
  (void)unit;
  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_integer_from_uint64(uv_hrtime()));
}

static uint64_t bsts_sleep_timeout_millis(uint64_t nanos)
{
  uint64_t millis = nanos / 1000000ULL;
  if ((nanos % 1000000ULL) != 0ULL)
  {
    millis += 1ULL;
  }
  return millis;
}

static void bsts_core_sleep_close_cb(uv_handle_t *handle)
{
  BSTS_Core_Sleep_Request *request = (BSTS_Core_Sleep_Request *)handle->data;
  if (request->is_error)
  {
    bsts_Bosatsu_Prog_suspended_error(request->suspended, request->error);
  }
  else
  {
    bsts_Bosatsu_Prog_suspended_success(request->suspended, bsts_unit_value());
  }
}

static void bsts_core_sleep_timer_cb(uv_timer_t *timer)
{
  uv_timer_stop(timer);
  uv_close((uv_handle_t *)timer, bsts_core_sleep_close_cb);
}

static int bsts_core_sleep_start(BSTS_Prog_Suspended *suspended)
{
  BSTS_Core_Sleep_Request *request =
      BSTS_PTR(BSTS_Core_Sleep_Request, bsts_Bosatsu_Prog_suspended_request(suspended));
  request->suspended = suspended;

  int timer_result = uv_timer_init(
      bsts_Bosatsu_Prog_suspended_loop(suspended),
      &request->timer);
  if (timer_result != 0)
  {
    bsts_Bosatsu_Prog_suspended_error(
        suspended,
        bsts_ioerror_from_uv(timer_result, "starting sleep timer"));
    return 0;
  }

  request->timer.data = request;
  timer_result = uv_timer_start(
      &request->timer,
      bsts_core_sleep_timer_cb,
      request->timeout_millis,
      0);
  if (timer_result != 0)
  {
    request->is_error = 1;
    request->error = bsts_ioerror_from_uv(timer_result, "starting sleep timer");
    uv_close((uv_handle_t *)&request->timer, bsts_core_sleep_close_cb);
  }
  return 0;
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

  // duration is still needed after this allocation to compute the timeout, so
  // keep it explicitly reachable through request initialization.
  BSTS_Core_Sleep_Request *request =
      (BSTS_Core_Sleep_Request *)GC_malloc(sizeof(BSTS_Core_Sleep_Request));
  if (request == NULL)
  {
    perror("GC_malloc failure in bsts_core_sleep_effect");
    abort();
  }

  request->suspended = NULL;
  request->timeout_millis =
      bsts_sleep_timeout_millis(bsts_integer_to_low_uint64(nanos_value));
  request->error = bsts_unit_value();
  request->is_error = 0;
  BValue suspended = bsts_Bosatsu_Prog_suspend(
      BSTS_VALUE_FROM_PTR(request),
      bsts_core_sleep_start);
  GC_reachable_here(duration);
  return suspended;
}

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_path__sep()
{
  static const BSTS_String sep = BSTS_STATIC_STRING_INIT(1, "/");
  return BSTS_VALUE_FROM_PTR(&sep);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_stdin()
{
  return bsts_core_make_handle(BSTS_HANDLE_STDIN, (uv_file)fileno(stdin), stdin, 1, 0, 0);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_stdout()
{
  return bsts_core_make_handle(BSTS_HANDLE_STDOUT, (uv_file)fileno(stdout), stdout, 0, 1, 0);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_stderr()
{
  return bsts_core_make_handle(BSTS_HANDLE_STDERR, (uv_file)fileno(stderr), stderr, 0, 1, 0);
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

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_mkdir__with__mode(BValue path, BValue recursive, BValue mode)
{
  return bsts_prog_effect3(path, recursive, mode, bsts_core_mkdir_with_mode_effect);
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
