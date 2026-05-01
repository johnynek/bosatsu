#include "bosatsu_runtime.h"
#include "bosatsu_array_internal.h"
#include <limits.h>
#include "bosatsu_ext_Bosatsu_l_Collection_l_Array.h"
#include "bosatsu_ext_Bosatsu_l_Predef.h"
#include "bosatsu_ext_Bosatsu_l_Num_l_Float64.h"
#include "bosatsu_ext_Bosatsu_l_Num_l_Int64.h"
#include "bosatsu_ext_Bosatsu_l_IO_l_Core.h"
#include "bosatsu_ext_Bosatsu_l_IO_l_Bytes.h"
#include "bosatsu_ext_Bosatsu_l_Prog.h"
#include "bosatsu_ext_Bosatsu_l_Prog_internal.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <uv.h>
#if !defined(_WIN32)
#include <signal.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#endif
#include "gc.h"

unsigned int bsts_core_test_uv_io_chunk_size(size_t len);

void assert(_Bool cond, const char* message) {
  if (!cond) {
    printf("%s\n", message);
    exit(1);
  }
}

void assert_string_equals(BValue got, const char* expected, const char* message) {
  BValue exp = bsts_string_from_utf8_bytes_static(strlen(expected), expected);
  if (bsts_string_cmp(got, exp) != 0) {
    printf("%s\nexpected: %s\ngot: ", message, expected);
    bsts_string_println(got);
    exit(1);
  }
}

void assert_int_string(BValue v, const char* expected, const char* message) {
  BValue s = bsts_integer_to_string(v);
  assert_string_equals(s, expected, message);
}

void assert_string_bytes(BValue got, const char* expected, size_t expected_len, const char* message) {
  BSTS_String_View view = bsts_string_view_ref(&got);
  size_t got_len = view.len;
  const char* got_bytes = view.bytes;
  if (got_len != expected_len || (expected_len > 0 && memcmp(got_bytes, expected, expected_len) != 0)) {
    printf("%s\nexpected len: %zu\ngot len: %zu\n", message, expected_len, got_len);
    exit(1);
  }
}

void assert_option_int(BValue opt, const char* expected, const char* message) {
  if (get_variant(opt) != 1) {
    printf("%s\nexpected: Some(%s)\n", message, expected);
    exit(1);
  }
  BValue v = get_enum_index(opt, 0);
  assert_int_string(v, expected, message);
}

void assert_option_none(BValue opt, const char* message) {
  if (get_variant(opt) != 0) {
    printf("%s\nexpected: None\n", message);
    exit(1);
  }
}

void assert_u64_equals(uint64_t got, uint64_t expected, const char* message) {
  if (got != expected) {
    printf("%s\nexpected: %llu\ngot: %llu\n", message, (unsigned long long)expected, (unsigned long long)got);
    exit(1);
  }
}

void assert_int64_bits(BValue value, uint64_t expected, const char* message) {
  assert_u64_equals(bsts_int64_to_bits(value), expected, message);
}

static void test_io_core_uv_chunk_sizes(void) {
  assert(
      bsts_core_test_uv_io_chunk_size(0) == 0U,
      "IO/Core libuv chunk size should preserve zero-length buffers");
  assert(
      bsts_core_test_uv_io_chunk_size((size_t)INT_MAX) == (unsigned int)INT_MAX,
      "IO/Core libuv chunk size should allow INT_MAX-byte buffers");
#if SIZE_MAX > INT_MAX
  assert(
      bsts_core_test_uv_io_chunk_size((size_t)INT_MAX + 1U) == (unsigned int)INT_MAX,
      "IO/Core libuv chunk size should cap buffers above INT_MAX");
#endif
#if SIZE_MAX > UINT_MAX
  assert(
      bsts_core_test_uv_io_chunk_size((size_t)UINT_MAX + 1U) == (unsigned int)INT_MAX,
      "IO/Core libuv chunk size should not wrap 4GB buffers to zero");
#endif
}

void assert_option_int64_bits(BValue opt, uint64_t expected, const char* message) {
  if (get_variant(opt) != 1) {
    printf("%s\nexpected: Some(Int64)\n", message);
    exit(1);
  }
  assert_int64_bits(get_enum_index(opt, 0), expected, message);
}

void assert_is_small_int(BValue value, const char* message) {
  if ((value & (BValue)0x1) != (BValue)0x1) {
    printf("%s\nexpected small-int immediate\n", message);
    exit(1);
  }
}

void assert_is_big_int(BValue value, const char* message) {
  if ((value & (BValue)0x1) != (BValue)0x0) {
    printf("%s\nexpected heap-backed integer\n", message);
    exit(1);
  }
}

static BValue prog_assoc_pure_fn(BValue arg) {
  return ___bsts_g_Bosatsu_l_Prog_l_pure(arg);
}

static BValue prog_assoc_raise_fn(BValue arg) {
  return ___bsts_g_Bosatsu_l_Prog_l_raise__error(arg);
}

static BValue prog_runner_pure_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_integer_from_int(7));
}

static BValue prog_runner_raise_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
      bsts_string_from_utf8_bytes_static(4, "boom"));
}

static BValue prog_runner_main_success_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_integer_from_int(3));
}

static BValue prog_runner_fm_success_fn(BValue arg) {
  return ___bsts_g_Bosatsu_l_Prog_l_pure(
      bsts_integer_add(arg, bsts_integer_from_int(1)));
}

static BValue prog_runner_unreachable_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
      bsts_string_from_utf8_bytes_static(11, "unreachable"));
}

static BValue prog_runner_recover_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_integer_from_int(11));
}

static BValue prog_runner_sync_effect_success_fn(BValue arg) {
  return ___bsts_g_Bosatsu_l_Prog_l_pure(
      bsts_integer_add(arg, bsts_integer_from_int(2)));
}

static BValue prog_runner_sync_effect_raise_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_raise__error(
      bsts_string_from_utf8_bytes_static(9, "sync fail"));
}

static BValue prog_runner_sync_effect_success_test_fn(BValue arg) {
  (void)arg;
  return alloc_enum2(
      5,
      bsts_integer_from_int(40),
      alloc_boxed_pure_fn1(prog_runner_sync_effect_success_fn));
}

static BValue prog_runner_sync_effect_recover_test_fn(BValue arg) {
  (void)arg;
  BValue effect = alloc_enum2(
      5,
      bsts_unit_value(),
      alloc_boxed_pure_fn1(prog_runner_sync_effect_raise_fn));
  return ___bsts_g_Bosatsu_l_Prog_l_recover(
      effect,
      alloc_boxed_pure_fn1(prog_runner_recover_fn));
}

static int prog_runner_async_effect_calls = 0;

typedef struct {
  uv_timer_t timer;
  BValue result;
  _Bool is_error;
  _Bool double_complete;
} ProgRunnerAsyncRequest;

static void prog_runner_async_close_cb(uv_handle_t *handle) {
  (void)handle;
}

static void prog_runner_async_timer_cb(uv_timer_t *timer) {
  BSTS_Prog_Suspended *suspended = (BSTS_Prog_Suspended *)timer->data;
  ProgRunnerAsyncRequest *request =
      BSTS_PTR(ProgRunnerAsyncRequest, bsts_Bosatsu_Prog_suspended_request(suspended));
  assert(
      bsts_Bosatsu_Prog_suspended_loop(suspended) == uv_handle_get_loop((uv_handle_t *)timer),
      "async completion should run on the suspended Prog owner loop");
  uv_timer_stop(timer);
  uv_close((uv_handle_t *)timer, prog_runner_async_close_cb);

  if (request->is_error) {
    bsts_Bosatsu_Prog_suspended_error(suspended, request->result);
  } else {
    bsts_Bosatsu_Prog_suspended_success(suspended, request->result);
  }

  if (request->double_complete) {
    bsts_Bosatsu_Prog_suspended_success(suspended, request->result);
  }
}

static int prog_runner_async_start(BSTS_Prog_Suspended *suspended) {
  ProgRunnerAsyncRequest *request =
      BSTS_PTR(ProgRunnerAsyncRequest, bsts_Bosatsu_Prog_suspended_request(suspended));
  int timer_result = uv_timer_init(
      bsts_Bosatsu_Prog_suspended_loop(suspended),
      &request->timer);
  if (timer_result != 0) {
    return timer_result;
  }

  request->timer.data = suspended;
  return uv_timer_start(&request->timer, prog_runner_async_timer_cb, 0, 0);
}

static BValue prog_runner_suspend_request(BValue result, _Bool is_error, _Bool double_complete) {
  ProgRunnerAsyncRequest *request =
      (ProgRunnerAsyncRequest *)GC_malloc(sizeof(ProgRunnerAsyncRequest));
  if (request == NULL) {
    perror("GC_malloc failure in prog_runner_suspend_request");
    exit(1);
  }

  request->result = result;
  request->is_error = is_error;
  request->double_complete = double_complete;
  return bsts_Bosatsu_Prog_suspend(
      BSTS_VALUE_FROM_PTR(request),
      prog_runner_async_start);
}

static BValue prog_runner_async_success_effect_fn(BValue arg) {
  prog_runner_async_effect_calls += 1;
  return prog_runner_suspend_request(
      bsts_integer_add(arg, bsts_integer_from_int(30)),
      0,
      0);
}

static BValue prog_runner_async_error_effect_fn(BValue arg) {
  (void)arg;
  prog_runner_async_effect_calls += 1;
  return prog_runner_suspend_request(
      bsts_string_from_utf8_bytes_static(10, "async fail"),
      1,
      0);
}

static BValue prog_runner_async_double_complete_effect_fn(BValue arg) {
  (void)arg;
  return prog_runner_suspend_request(bsts_integer_from_int(1), 0, 1);
}

static int prog_runner_async_start_fail(BSTS_Prog_Suspended *suspended) {
  (void)suspended;
  return UV_EINVAL;
}

static BValue prog_runner_async_start_fail_effect_fn(BValue arg) {
  (void)arg;
  return bsts_Bosatsu_Prog_suspend(
      bsts_unit_value(),
      prog_runner_async_start_fail);
}

static int prog_runner_async_never_complete_start(BSTS_Prog_Suspended *suspended) {
  (void)suspended;
  return 0;
}

static BValue prog_runner_async_never_complete_effect_fn(BValue arg) {
  (void)arg;
  return bsts_Bosatsu_Prog_suspend(
      bsts_unit_value(),
      prog_runner_async_never_complete_start);
}

static void prog_runner_async_unref_timer_cb(uv_timer_t *timer) {
  (void)timer;
  printf("unreferenced unfinished Prog timer unexpectedly fired\n");
  abort();
}

static int prog_runner_async_unref_start(BSTS_Prog_Suspended *suspended) {
  ProgRunnerAsyncRequest *request =
      BSTS_PTR(ProgRunnerAsyncRequest, bsts_Bosatsu_Prog_suspended_request(suspended));
  int timer_result = uv_timer_init(
      bsts_Bosatsu_Prog_suspended_loop(suspended),
      &request->timer);
  if (timer_result != 0) {
    return timer_result;
  }

  request->timer.data = suspended;
  timer_result = uv_timer_start(
      &request->timer,
      prog_runner_async_unref_timer_cb,
      60000,
      0);
  if (timer_result != 0) {
    return timer_result;
  }

  uv_unref((uv_handle_t *)&request->timer);
  return 0;
}

static BValue prog_runner_async_unref_effect_fn(BValue arg) {
  (void)arg;
  ProgRunnerAsyncRequest *request =
      (ProgRunnerAsyncRequest *)GC_malloc(sizeof(ProgRunnerAsyncRequest));
  if (request == NULL) {
    perror("GC_malloc failure in prog_runner_async_unref_effect_fn");
    exit(1);
  }

  request->result = bsts_unit_value();
  request->is_error = 0;
  request->double_complete = 0;
  return bsts_Bosatsu_Prog_suspend(
      BSTS_VALUE_FROM_PTR(request),
      prog_runner_async_unref_start);
}

static BValue prog_runner_async_after_success_fn(BValue arg) {
  assert(
      prog_runner_async_effect_calls > 0,
      "async continuation should run only after effect callback has been scheduled");
  return ___bsts_g_Bosatsu_l_Prog_l_pure(
      bsts_integer_add(arg, bsts_integer_from_int(1)));
}

static BValue prog_runner_async_success_test_fn(BValue arg) {
  (void)arg;
  BValue effect = alloc_enum2(
      5,
      bsts_integer_from_int(10),
      alloc_boxed_pure_fn1(prog_runner_async_success_effect_fn));
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      effect,
      alloc_boxed_pure_fn1(prog_runner_async_after_success_fn));
}

static BValue prog_runner_async_error_recover_test_fn(BValue arg) {
  (void)arg;
  BValue effect = alloc_enum2(
      5,
      bsts_unit_value(),
      alloc_boxed_pure_fn1(prog_runner_async_error_effect_fn));
  return ___bsts_g_Bosatsu_l_Prog_l_recover(
      effect,
      alloc_boxed_pure_fn1(prog_runner_recover_fn));
}

static BValue prog_runner_async_double_complete_test_fn(BValue arg) {
  (void)arg;
  return alloc_enum2(
      5,
      bsts_unit_value(),
      alloc_boxed_pure_fn1(prog_runner_async_double_complete_effect_fn));
}

static BValue prog_runner_async_start_fail_test_fn(BValue arg) {
  (void)arg;
  return alloc_enum2(
      5,
      bsts_unit_value(),
      alloc_boxed_pure_fn1(prog_runner_async_start_fail_effect_fn));
}

static BValue prog_runner_async_never_complete_test_fn(BValue arg) {
  (void)arg;
  return alloc_enum2(
      5,
      bsts_unit_value(),
      alloc_boxed_pure_fn1(prog_runner_async_never_complete_effect_fn));
}

static BValue prog_runner_async_unref_test_fn(BValue arg) {
  (void)arg;
  return alloc_enum2(
      5,
      bsts_unit_value(),
      alloc_boxed_pure_fn1(prog_runner_async_unref_effect_fn));
}

static BValue prog_runner_flatmap_after_pure_test_fn(BValue arg) {
  (void)arg;
  BValue step = alloc_boxed_pure_fn1(prog_runner_fm_success_fn);
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_integer_from_int(4)),
      step);
}

static BValue prog_runner_flatmap_after_raise_test_fn(BValue arg) {
  (void)arg;
  BValue step = alloc_boxed_pure_fn1(prog_runner_unreachable_fn);
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_Prog_l_raise__error(
          bsts_string_from_utf8_bytes_static(5, "error")),
      step);
}

static BValue prog_runner_recover_after_raise_test_fn(BValue arg) {
  (void)arg;
  BValue recover = alloc_boxed_pure_fn1(prog_runner_recover_fn);
  return ___bsts_g_Bosatsu_l_Prog_l_recover(
      ___bsts_g_Bosatsu_l_Prog_l_raise__error(
          bsts_string_from_utf8_bytes_static(5, "error")),
      recover);
}

static BValue prog_runner_recover_after_pure_test_fn(BValue arg) {
  (void)arg;
  BValue recover = alloc_boxed_pure_fn1(prog_runner_unreachable_fn);
  return ___bsts_g_Bosatsu_l_Prog_l_recover(
      ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_integer_from_int(13)),
      recover);
}

static BValue io_core_now_wall_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_now__wall();
}

static BValue io_core_now_mono_second_fn(BValue *slots, BValue second) {
  BValue first = slots[0];
  assert(
      bsts_integer_cmp(second, first) >= 0,
      "IO/Core now_mono should not move backward within a Prog");
  return ___bsts_g_Bosatsu_l_Prog_l_pure(second);
}

static BValue io_core_now_mono_first_fn(BValue first) {
  BValue slots[1] = { first };
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_now__mono(),
      alloc_closure1(1, slots, io_core_now_mono_second_fn));
}

static BValue io_core_now_mono_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_now__mono(),
      alloc_boxed_pure_fn1(io_core_now_mono_first_fn));
}

static BValue io_core_get_env_present_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_get__env(
      bsts_string_from_utf8_bytes_static(26, "BOSATSU_C_RUNTIME_TEST_ENV"));
}

static BValue io_core_get_env_empty_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_get__env(
      bsts_string_from_utf8_bytes_static(32, "BOSATSU_C_RUNTIME_TEST_ENV_EMPTY"));
}

static BValue io_core_get_env_long_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_get__env(
      bsts_string_from_utf8_bytes_static(31, "BOSATSU_C_RUNTIME_TEST_ENV_LONG"));
}

static BValue io_core_get_env_absent_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_get__env(
      bsts_string_from_utf8_bytes_static(33, "BOSATSU_C_RUNTIME_TEST_ENV_ABSENT"));
}

static int io_core_sleep_continuations = 0;

static BValue io_core_sleep_zero_after_fn(BValue arg) {
  (void)arg;
  io_core_sleep_continuations += 1;
  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_integer_from_int(17));
}

static BValue io_core_sleep_zero_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_sleep(bsts_integer_from_int(0)),
      alloc_boxed_pure_fn1(io_core_sleep_zero_after_fn));
}

static BValue io_core_sleep_positive_after_fn(BValue arg) {
  (void)arg;
  io_core_sleep_continuations += 1;
  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_integer_from_int(23));
}

static BValue io_core_sleep_positive_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_sleep(bsts_integer_from_int(2000000)),
      alloc_boxed_pure_fn1(io_core_sleep_positive_after_fn));
}

static BValue io_core_sleep_repeat_third_fn(BValue arg) {
  (void)arg;
  io_core_sleep_continuations += 1;
  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_integer_from_int(31));
}

static BValue io_core_sleep_repeat_second_fn(BValue arg) {
  (void)arg;
  io_core_sleep_continuations += 1;
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_sleep(bsts_integer_from_int(0)),
      alloc_boxed_pure_fn1(io_core_sleep_repeat_third_fn));
}

static BValue io_core_sleep_repeat_first_fn(BValue arg) {
  (void)arg;
  io_core_sleep_continuations += 1;
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_sleep(bsts_integer_from_int(1000000)),
      alloc_boxed_pure_fn1(io_core_sleep_repeat_second_fn));
}

static BValue io_core_sleep_repeat_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_sleep(bsts_integer_from_int(0)),
      alloc_boxed_pure_fn1(io_core_sleep_repeat_first_fn));
}

static char io_core_file_missing_path[PATH_MAX];
static char io_core_file_existing_path[PATH_MAX];
static char io_core_file_close_path[PATH_MAX];
static char io_core_file_closed_read_path[PATH_MAX];
static char io_core_file_utf8_path[PATH_MAX];
static char io_core_file_bytes_path[PATH_MAX];
static char io_core_file_empty_path[PATH_MAX];
static char io_core_file_copy_src_path[PATH_MAX];
static char io_core_file_copy_dst_path[PATH_MAX];
static char io_core_file_copy_limit_dst_path[PATH_MAX];
static char io_core_file_invalid_utf8_path[PATH_MAX];
static char io_core_file_write_only_path[PATH_MAX];
static char io_core_file_read_only_path[PATH_MAX];
static char io_core_file_temp_created_path[PATH_MAX];
static char io_core_dir_root_path[PATH_MAX];
static char io_core_dir_missing_path[PATH_MAX];
static char io_core_dir_mkdir_path[PATH_MAX];
static char io_core_dir_mkdir_nested_path[PATH_MAX];
static char io_core_dir_mode_path[PATH_MAX];
static char io_core_dir_list_path[PATH_MAX];
static char io_core_dir_list_a_path[PATH_MAX];
static char io_core_dir_list_b_path[PATH_MAX];
static char io_core_dir_stat_file_path[PATH_MAX];
static char io_core_dir_stat_symlink_path[PATH_MAX];
static char io_core_dir_remove_file_path[PATH_MAX];
static char io_core_dir_remove_nonempty_path[PATH_MAX];
static char io_core_dir_remove_nonempty_child_path[PATH_MAX];
static char io_core_dir_remove_tree_path[PATH_MAX];
static char io_core_dir_remove_tree_child_path[PATH_MAX];
static char io_core_dir_remove_tree_file_path[PATH_MAX];
static char io_core_dir_rename_from_path[PATH_MAX];
static char io_core_dir_rename_to_path[PATH_MAX];
static char io_core_dir_temp_created_path[PATH_MAX];

static BValue io_core_path_value(const char* path) {
  return bsts_string_from_utf8_bytes_copy(strlen(path), path);
}

static BValue io_core_bytes_value(const uint8_t* data, int len) {
  uint8_t* owned = NULL;
  if (len > 0) {
    owned = (uint8_t*)GC_malloc_atomic((size_t)len);
    if (owned == NULL) {
      perror("GC_malloc_atomic failure in io_core_bytes_value");
      exit(1);
    }
    memcpy(owned, data, (size_t)len);
  }
  return bsts_bytes_wrap(owned, 0, len);
}

static void io_core_test_unlink(const char* path) {
  uv_fs_t req;
  (void)uv_fs_unlink(NULL, &req, path, NULL);
  uv_fs_req_cleanup(&req);
}

static void io_core_test_rmdir(const char* path) {
  uv_fs_t req;
  (void)uv_fs_rmdir(NULL, &req, path, NULL);
  uv_fs_req_cleanup(&req);
}

static void io_core_test_mkdir(const char* path) {
  uv_fs_t req;
  int result = uv_fs_mkdir(NULL, &req, path, 0777, NULL);
  uv_fs_req_cleanup(&req);
  assert(result == 0, "creating IO/Core directory fixture should succeed");
}

static _Bool io_core_test_path_exists(const char* path) {
  uv_fs_t req;
  int result = uv_fs_lstat(NULL, &req, path, NULL);
  uv_fs_req_cleanup(&req);
  return result == 0;
}

static void io_core_write_fixture(const char* path, const uint8_t* data, size_t len) {
  FILE* file = fopen(path, "wb");
  assert(file != NULL, "creating IO/Core file fixture should succeed");
  if (len > 0) {
    assert(
        fwrite(data, 1, len, file) == len,
        "writing IO/Core file fixture should write all bytes");
  }
  assert(fclose(file) == 0, "closing IO/Core file fixture should succeed");
}

static void io_core_read_fixture(const char* path, uint8_t* data, size_t len, const char* message) {
  FILE* file = fopen(path, "rb");
  assert(file != NULL, "opening IO/Core file fixture should succeed");
  if (len > 0) {
    assert(fread(data, 1, len, file) == len, message);
  }
  assert(fgetc(file) == EOF, message);
  assert(fclose(file) == 0, "closing IO/Core file fixture should succeed");
}

static void assert_bytes_equal(BValue bytes, const uint8_t* expected, int expected_len, const char* message) {
  BSTS_Bytes* got = bsts_bytes_unbox(bytes);
  if (got->len != expected_len) {
    printf("%s\nexpected len: %d\ngot len: %d\n", message, expected_len, got->len);
    exit(1);
  }
  if (expected_len > 0 && memcmp(got->data + got->offset, expected, (size_t)expected_len) != 0) {
    printf("%s\nbyte payload mismatch\n", message);
    exit(1);
  }
}

static void assert_option_bytes_equal(BValue opt, const uint8_t* expected, int expected_len, const char* message) {
  if (get_variant(opt) != 1) {
    printf("%s\nexpected: Some(Bytes)\n", message);
    exit(1);
  }
  assert_bytes_equal(get_enum_index(opt, 0), expected, expected_len, message);
}

static void assert_path_list_equals(BValue list, const char** expected, size_t expected_len, const char* message) {
  BValue cursor = list;
  for (size_t idx = 0; idx < expected_len; idx++) {
    if (get_variant(cursor) != 1) {
      printf("%s\nexpected path list item at index: %zu\n", message, idx);
      exit(1);
    }
    assert_string_equals(get_enum_index(cursor, 0), expected[idx], message);
    cursor = get_enum_index(cursor, 1);
  }
  if (get_variant(cursor) != 0) {
    printf("%s\nexpected end of path list\n", message);
    exit(1);
  }
}

static void assert_stat_kind(BValue stat_opt, unsigned char expected_kind, const char* message) {
  if (get_variant(stat_opt) != 1) {
    printf("%s\nexpected Some(FileStat)\n", message);
    exit(1);
  }
  BValue stat_value = get_enum_index(stat_opt, 0);
  BValue kind = get_struct_index(stat_value, 0);
  if (get_variant(kind) != expected_kind) {
    printf("%s\nexpected kind: %u\ngot: %u\n", message, expected_kind, get_variant(kind));
    exit(1);
  }
}

static int stat_mode_bits_from_option(BValue stat_opt) {
  if (get_variant(stat_opt) != 1) {
    printf("expected Some(FileStat) while reading mode bits\n");
    exit(1);
  }
  BValue stat_value = get_enum_index(stat_opt, 0);
  BValue mode_opt = get_struct_index(stat_value, 3);
  if (get_variant(mode_opt) != 1) {
    printf("expected stat posix_mode on this C runtime target\n");
    exit(1);
  }
  return bsts_integer_to_int32(get_enum_index(mode_opt, 0));
}

static BValue io_core_open_missing_read_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_open__file(
      io_core_path_value(io_core_file_missing_path),
      alloc_enum0(0));
}

static BValue io_core_create_new_existing_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_open__file(
      io_core_path_value(io_core_file_existing_path),
      alloc_enum0(3));
}

static BValue io_core_close_again_fn(BValue* slots, BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_close(slots[0]);
}

static BValue io_core_close_first_fn(BValue handle) {
  BValue slots[1] = { handle };
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_close(handle),
      alloc_closure1(1, slots, io_core_close_again_fn));
}

static BValue io_core_close_twice_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_open__file(
          io_core_path_value(io_core_file_close_path),
          alloc_enum0(1)),
      alloc_boxed_pure_fn1(io_core_close_first_fn));
}

static BValue io_core_read_after_close_fn(BValue* slots, BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_read__bytes(
      slots[0],
      bsts_integer_from_int(1));
}

static BValue io_core_close_before_read_fn(BValue handle) {
  BValue slots[1] = { handle };
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_close(handle),
      alloc_closure1(1, slots, io_core_read_after_close_fn));
}

static BValue io_core_read_closed_handle_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_open__file(
          io_core_path_value(io_core_file_closed_read_path),
          alloc_enum0(1)),
      alloc_boxed_pure_fn1(io_core_close_before_read_fn));
}

static BValue io_core_close_slot0_fn(BValue* slots, BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_close(slots[0]);
}

static BValue io_core_write_utf8_flush_fn(BValue* slots, BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_flush(slots[0]),
      alloc_closure1(1, slots, io_core_close_slot0_fn));
}

static BValue io_core_write_utf8_close_fn(BValue* slots, BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_write__utf8(
          slots[0],
          bsts_string_from_utf8_bytes_static(8, "hello-\xc2\xb5")),
      alloc_closure1(1, slots, io_core_write_utf8_flush_fn));
}

static BValue io_core_write_utf8_opened_fn(BValue handle) {
  BValue slots[1] = { handle };
  return io_core_write_utf8_close_fn(slots, bsts_unit_value());
}

static BValue io_core_write_utf8_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_open__file(
          io_core_path_value(io_core_file_utf8_path),
          alloc_enum0(1)),
      alloc_boxed_pure_fn1(io_core_write_utf8_opened_fn));
}

static BValue io_core_read_utf8_eof_assert_fn(BValue* slots, BValue arg) {
  assert_option_none(arg, "IO/Core read_utf8 should return None at EOF");
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_close(slots[0]);
}

static BValue io_core_read_utf8_after_some_fn(BValue* slots, BValue arg) {
  if (get_variant(arg) != 1) {
    printf("IO/Core read_utf8 should return Some text before EOF\n");
    exit(1);
  }
  assert_string_equals(
      get_enum_index(arg, 0),
      "hello-\xc2\xb5",
      "IO/Core read_utf8 should preserve UTF-8 text");
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_read__utf8(slots[0], bsts_integer_from_int(32)),
      alloc_closure1(1, slots, io_core_read_utf8_eof_assert_fn));
}

static BValue io_core_read_utf8_test_fn(BValue handle) {
  BValue slots[1] = { handle };
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_read__utf8(handle, bsts_integer_from_int(32)),
      alloc_closure1(1, slots, io_core_read_utf8_after_some_fn));
}

static BValue io_core_read_utf8_file_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_open__file(
          io_core_path_value(io_core_file_utf8_path),
          alloc_enum0(0)),
      alloc_boxed_pure_fn1(io_core_read_utf8_test_fn));
}

static BValue io_core_write_bytes_flush_fn(BValue* slots, BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_flush(slots[0]),
      alloc_closure1(1, slots, io_core_close_slot0_fn));
}

static BValue io_core_write_bytes_close_fn(BValue* slots, BValue arg) {
  (void)arg;
  static const uint8_t payload[] = {0, 1, 2, 253, 254, 255};
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_write__bytes(
          slots[0],
          io_core_bytes_value(payload, (int)sizeof(payload))),
      alloc_closure1(1, slots, io_core_write_bytes_flush_fn));
}

static BValue io_core_write_bytes_opened_fn(BValue handle) {
  BValue slots[1] = { handle };
  return io_core_write_bytes_close_fn(slots, bsts_unit_value());
}

static BValue io_core_write_bytes_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_open__file(
          io_core_path_value(io_core_file_bytes_path),
          alloc_enum0(1)),
      alloc_boxed_pure_fn1(io_core_write_bytes_opened_fn));
}

static BValue io_core_read_bytes_eof_assert_fn(BValue* slots, BValue arg) {
  assert_option_none(arg, "IO/Core read_bytes should return None at EOF");
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_close(slots[0]);
}

static BValue io_core_read_bytes_second_assert_fn(BValue* slots, BValue arg) {
  static const uint8_t expected[] = {253, 254, 255};
  assert_option_bytes_equal(
      arg,
      expected,
      (int)sizeof(expected),
      "IO/Core read_bytes should return the remaining bytes");
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_read__bytes(slots[0], bsts_integer_from_int(4)),
      alloc_closure1(1, slots, io_core_read_bytes_eof_assert_fn));
}

static BValue io_core_read_bytes_first_assert_fn(BValue* slots, BValue arg) {
  static const uint8_t expected[] = {0, 1, 2};
  assert_option_bytes_equal(
      arg,
      expected,
      (int)sizeof(expected),
      "IO/Core read_bytes should return a bounded prefix");
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_read__bytes(slots[0], bsts_integer_from_int(4)),
      alloc_closure1(1, slots, io_core_read_bytes_second_assert_fn));
}

static BValue io_core_read_bytes_test_fn(BValue handle) {
  BValue slots[1] = { handle };
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_read__bytes(handle, bsts_integer_from_int(3)),
      alloc_closure1(1, slots, io_core_read_bytes_first_assert_fn));
}

static BValue io_core_read_bytes_file_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_open__file(
          io_core_path_value(io_core_file_bytes_path),
          alloc_enum0(0)),
      alloc_boxed_pure_fn1(io_core_read_bytes_test_fn));
}

static BValue io_core_read_all_bytes_assert_fn(BValue* slots, BValue arg) {
  static const uint8_t expected[] = {0, 1, 2, 253, 254, 255};
  assert_bytes_equal(
      arg,
      expected,
      (int)sizeof(expected),
      "IO/Core read_all_bytes should preserve byte ordering across chunks");
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_close(slots[0]);
}

static BValue io_core_read_all_bytes_test_fn(BValue handle) {
  BValue slots[1] = { handle };
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_read__all__bytes(handle, bsts_integer_from_int(2)),
      alloc_closure1(1, slots, io_core_read_all_bytes_assert_fn));
}

static BValue io_core_read_all_bytes_file_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_open__file(
          io_core_path_value(io_core_file_bytes_path),
          alloc_enum0(0)),
      alloc_boxed_pure_fn1(io_core_read_all_bytes_test_fn));
}

static BValue io_core_read_all_empty_assert_fn(BValue* slots, BValue arg) {
  assert_bytes_equal(arg, NULL, 0, "IO/Core read_all_bytes should return empty bytes for an empty file");
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_close(slots[0]);
}

static BValue io_core_read_all_empty_test_fn(BValue handle) {
  BValue slots[1] = { handle };
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_read__all__bytes(handle, bsts_integer_from_int(8)),
      alloc_closure1(1, slots, io_core_read_all_empty_assert_fn));
}

static BValue io_core_read_all_empty_file_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_open__file(
          io_core_path_value(io_core_file_empty_path),
          alloc_enum0(0)),
      alloc_boxed_pure_fn1(io_core_read_all_empty_test_fn));
}

static BValue io_core_copy_bytes_close_dst_fn(BValue* slots, BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_close(slots[1]);
}

static BValue io_core_copy_bytes_close_src_fn(BValue* slots, BValue arg) {
  assert_int_string(arg, "9", "IO/Core copy_bytes without a limit should return the copied byte count");
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_close(slots[0]),
      alloc_closure1(2, slots, io_core_copy_bytes_close_dst_fn));
}

static BValue io_core_copy_bytes_run_fn(BValue* slots, BValue dst) {
  BValue next_slots[2] = { slots[0], dst };
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_copy__bytes(
          slots[0],
          dst,
          bsts_integer_from_int(4),
          alloc_enum0(0)),
      alloc_closure1(2, next_slots, io_core_copy_bytes_close_src_fn));
}

static BValue io_core_copy_bytes_open_dst_fn(BValue src) {
  BValue slots[1] = { src };
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_open__file(
          io_core_path_value(io_core_file_copy_dst_path),
          alloc_enum0(1)),
      alloc_closure1(1, slots, io_core_copy_bytes_run_fn));
}

static BValue io_core_copy_bytes_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_open__file(
          io_core_path_value(io_core_file_copy_src_path),
          alloc_enum0(0)),
      alloc_boxed_pure_fn1(io_core_copy_bytes_open_dst_fn));
}

static BValue io_core_copy_bytes_limit_close_dst_fn(BValue* slots, BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_close(slots[1]);
}

static BValue io_core_copy_bytes_limit_close_src_fn(BValue* slots, BValue arg) {
  assert_int_string(arg, "5", "IO/Core copy_bytes with a finite limit should return the limited count");
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_close(slots[0]),
      alloc_closure1(2, slots, io_core_copy_bytes_limit_close_dst_fn));
}

static BValue io_core_copy_bytes_limit_run_fn(BValue* slots, BValue dst) {
  BValue next_slots[2] = { slots[0], dst };
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_copy__bytes(
          slots[0],
          dst,
          bsts_integer_from_int(3),
          alloc_enum1(1, bsts_integer_from_int(5))),
      alloc_closure1(2, next_slots, io_core_copy_bytes_limit_close_src_fn));
}

static BValue io_core_copy_bytes_limit_open_dst_fn(BValue src) {
  BValue slots[1] = { src };
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_open__file(
          io_core_path_value(io_core_file_copy_limit_dst_path),
          alloc_enum0(1)),
      alloc_closure1(1, slots, io_core_copy_bytes_limit_run_fn));
}

static BValue io_core_copy_bytes_limit_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_open__file(
          io_core_path_value(io_core_file_copy_src_path),
          alloc_enum0(0)),
      alloc_boxed_pure_fn1(io_core_copy_bytes_limit_open_dst_fn));
}

static BValue io_core_invalid_utf8_read_test_fn(BValue handle) {
  BValue slots[1] = { handle };
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_read__utf8(handle, bsts_integer_from_int(4)),
      alloc_closure1(1, slots, io_core_close_slot0_fn));
}

static BValue io_core_invalid_utf8_file_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_open__file(
          io_core_path_value(io_core_file_invalid_utf8_path),
          alloc_enum0(0)),
      alloc_boxed_pure_fn1(io_core_invalid_utf8_read_test_fn));
}

static BValue io_core_write_only_read_test_fn(BValue handle) {
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_read__bytes(handle, bsts_integer_from_int(1));
}

static BValue io_core_read_from_write_only_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_open__file(
          io_core_path_value(io_core_file_write_only_path),
          alloc_enum0(1)),
      alloc_boxed_pure_fn1(io_core_write_only_read_test_fn));
}

static BValue io_core_read_only_write_test_fn(BValue handle) {
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_write__bytes(
      handle,
      bsts_bytes_empty());
}

static BValue io_core_write_to_read_only_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_open__file(
          io_core_path_value(io_core_file_read_only_path),
          alloc_enum0(0)),
      alloc_boxed_pure_fn1(io_core_read_only_write_test_fn));
}

static BValue io_core_temp_file_close_fn(BValue* slots, BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_close(slots[0]);
}

static BValue io_core_temp_file_assert_fn(BValue temp_file) {
  BValue path = get_struct_index(temp_file, 0);
  BValue handle = get_struct_index(temp_file, 1);
  BSTS_String_View view = bsts_string_view_ref(&path);
  size_t name_start = 0;
  for (size_t idx = 0; idx < view.len; idx++) {
    if (view.bytes[idx] == '/') {
      name_start = idx + 1;
    }
  }
  static const char prefix[] = "bosatsu_test_";
  assert(
      view.len >= name_start + (sizeof(prefix) - 1) + 4 &&
          memcmp(view.bytes + name_start, prefix, sizeof(prefix) - 1) == 0 &&
          memcmp(view.bytes + view.len - 4, ".tmp", 4) == 0,
      "IO/Core create_temp_file should preserve requested prefix and suffix");
  char* path_copy = (char*)malloc(view.len + 1);
  assert(path_copy != NULL, "allocating temp path copy should succeed");
  memcpy(path_copy, view.bytes, view.len);
  path_copy[view.len] = '\0';
  strncpy(io_core_file_temp_created_path, path_copy, sizeof(io_core_file_temp_created_path) - 1);
  io_core_file_temp_created_path[sizeof(io_core_file_temp_created_path) - 1] = '\0';
  free(path_copy);

  BValue slots[1] = { handle };
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_write__utf8(
          handle,
          bsts_string_from_utf8_bytes_static(4, "temp")),
      alloc_closure1(1, slots, io_core_temp_file_close_fn));
}

static BValue io_core_create_temp_file_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_create__temp__file(
          alloc_enum0(0),
          bsts_string_from_utf8_bytes_static(13, "bosatsu_test_"),
          bsts_string_from_utf8_bytes_static(4, ".tmp")),
      alloc_boxed_pure_fn1(io_core_temp_file_assert_fn));
}

static BValue io_core_create_temp_dir_assert_fn(BValue path) {
  BSTS_String_View view = bsts_string_view_ref(&path);
  size_t name_start = 0;
  for (size_t idx = 0; idx < view.len; idx++) {
    if (view.bytes[idx] == '/') {
      name_start = idx + 1;
    }
  }
  static const char prefix[] = "bosatsu_test_dir_";
  assert(
      view.len >= name_start + (sizeof(prefix) - 1) &&
          memcmp(view.bytes + name_start, prefix, sizeof(prefix) - 1) == 0,
      "IO/Core create_temp_dir should preserve requested prefix");
  char* path_copy = (char*)malloc(view.len + 1);
  assert(path_copy != NULL, "allocating temp dir path copy should succeed");
  memcpy(path_copy, view.bytes, view.len);
  path_copy[view.len] = '\0';
  strncpy(io_core_dir_temp_created_path, path_copy, sizeof(io_core_dir_temp_created_path) - 1);
  io_core_dir_temp_created_path[sizeof(io_core_dir_temp_created_path) - 1] = '\0';
  free(path_copy);
  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_unit_value());
}

static BValue io_core_create_temp_dir_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_create__temp__dir(
          alloc_enum0(0),
          bsts_string_from_utf8_bytes_static(17, "bosatsu_test_dir_")),
      alloc_boxed_pure_fn1(io_core_create_temp_dir_assert_fn));
}

static BValue io_core_create_temp_dir_invalid_prefix_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_create__temp__dir(
      alloc_enum0(0),
      bsts_string_from_utf8_bytes_static(4, "bad/"));
}

static BValue io_core_list_missing_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_list__dir(
      io_core_path_value(io_core_dir_missing_path));
}

static BValue io_core_mkdir_existing_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_mkdir(
      io_core_path_value(io_core_dir_mkdir_path),
      alloc_enum0(0));
}

static BValue io_core_remove_missing_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_remove(
      io_core_path_value(io_core_dir_missing_path),
      alloc_enum0(0));
}

static BValue io_core_rename_missing_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_rename(
      io_core_path_value(io_core_dir_missing_path),
      io_core_path_value(io_core_dir_rename_to_path));
}

static BValue io_core_stat_missing_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_stat(
      io_core_path_value(io_core_dir_missing_path));
}

static BValue io_core_mkdir_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_mkdir(
      io_core_path_value(io_core_dir_mkdir_path),
      alloc_enum0(0));
}

static BValue io_core_mkdir_recursive_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_mkdir(
      io_core_path_value(io_core_dir_mkdir_nested_path),
      alloc_enum0(1));
}

static BValue io_core_mkdir_with_mode_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_mkdir__with__mode(
      io_core_path_value(io_core_dir_mode_path),
      alloc_enum0(0),
      bsts_integer_from_int(0700));
}

static BValue io_core_list_dir_assert_fn(BValue list) {
  const char* expected[] = {
      io_core_dir_list_a_path,
      io_core_dir_list_b_path
  };
  assert_path_list_equals(
      list,
      expected,
      2,
      "IO/Core list_dir should return sorted joined child paths");
  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_unit_value());
}

static BValue io_core_list_dir_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_list__dir(
          io_core_path_value(io_core_dir_list_path)),
      alloc_boxed_pure_fn1(io_core_list_dir_assert_fn));
}

static BValue io_core_stat_file_assert_fn(BValue stat_opt) {
  assert_stat_kind(stat_opt, 0, "IO/Core stat should classify regular files");
  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_unit_value());
}

static BValue io_core_stat_file_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_stat(
          io_core_path_value(io_core_dir_stat_file_path)),
      alloc_boxed_pure_fn1(io_core_stat_file_assert_fn));
}

static BValue io_core_stat_dir_assert_fn(BValue stat_opt) {
  assert_stat_kind(stat_opt, 1, "IO/Core stat should classify directories");
  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_unit_value());
}

static BValue io_core_stat_dir_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_stat(
          io_core_path_value(io_core_dir_list_path)),
      alloc_boxed_pure_fn1(io_core_stat_dir_assert_fn));
}

static BValue io_core_stat_symlink_assert_fn(BValue stat_opt) {
  assert_stat_kind(stat_opt, 2, "IO/Core stat should classify symlinks without following them");
  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_unit_value());
}

static BValue io_core_stat_symlink_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_stat(
          io_core_path_value(io_core_dir_stat_symlink_path)),
      alloc_boxed_pure_fn1(io_core_stat_symlink_assert_fn));
}

static BValue io_core_stat_mode_assert_fn(BValue stat_opt) {
  int mode_bits = stat_mode_bits_from_option(stat_opt) & 0777;
  if (mode_bits != 0700) {
    printf("IO/Core mkdir_with_mode should set requested mode bits\nexpected: 0700\ngot: 0%o\n", mode_bits);
    exit(1);
  }
  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_unit_value());
}

static BValue io_core_stat_mode_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_IO_l_Core_l_stat(
          io_core_path_value(io_core_dir_mode_path)),
      alloc_boxed_pure_fn1(io_core_stat_mode_assert_fn));
}

static BValue io_core_remove_file_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_remove(
      io_core_path_value(io_core_dir_remove_file_path),
      alloc_enum0(0));
}

static BValue io_core_remove_nonempty_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_remove(
      io_core_path_value(io_core_dir_remove_nonempty_path),
      alloc_enum0(0));
}

static BValue io_core_remove_recursive_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_remove(
      io_core_path_value(io_core_dir_remove_tree_path),
      alloc_enum0(1));
}

static BValue io_core_rename_test_fn(BValue arg) {
  (void)arg;
  return ___bsts_g_Bosatsu_l_IO_l_Core_l_rename(
      io_core_path_value(io_core_dir_rename_from_path),
      io_core_path_value(io_core_dir_rename_to_path));
}

#if !defined(_WIN32)
typedef void (*VoidFn)(void);

static void assert_child_aborts(VoidFn fn, const char* message) {
  pid_t pid = fork();
  if (pid < 0) {
    perror("fork failed");
    exit(1);
  }
  if (pid == 0) {
    fn();
    _exit(0);
  }

  int status = 0;
  if (waitpid(pid, &status, 0) < 0) {
    perror("waitpid failed");
    exit(1);
  }

  if (!(WIFSIGNALED(status) && WTERMSIG(status) == SIGABRT)) {
    printf("%s\n", message);
    exit(1);
  }
}

static void call_string_copy_too_large() {
  (void)bsts_string_from_utf8_bytes_copy(BSTS_STRING_INLINE16_FLAG, "x");
}

static void call_string_static_too_large() {
  (void)bsts_string_from_utf8_bytes_static(BSTS_STRING_INLINE16_FLAG, "x");
}

static void call_string_mut_too_large() {
  (void)bsts_string_mut(BSTS_STRING_INLINE16_FLAG);
}

#if defined(BSTS_RUNTIME_DEBUG_CHECKS)
static BValue closure_zero_abort_fn(BValue *slots, BValue arg) {
  (void)slots;
  return arg;
}

static void call_alloc_closure_zero() {
  (void)alloc_closure1(0, NULL, closure_zero_abort_fn);
}
#endif

static void call_prog_async_double_complete() {
  BValue double_complete = alloc_boxed_pure_fn1(prog_runner_async_double_complete_test_fn);
  (void)bsts_Bosatsu_Prog_run_test(double_complete);
}

static void call_prog_async_never_complete() {
  BValue never_complete = alloc_boxed_pure_fn1(prog_runner_async_never_complete_test_fn);
  (void)bsts_Bosatsu_Prog_run_test(never_complete);
}

static void call_prog_async_unref() {
  BValue unref = alloc_boxed_pure_fn1(prog_runner_async_unref_test_fn);
  (void)bsts_Bosatsu_Prog_run_test(unref);
}
#endif

void assert_option_float_bits(BValue opt, uint64_t expected, const char* message) {
  if (get_variant(opt) != 1) {
    printf("%s\nexpected: Some(float64)\ngot: None\n", message);
    exit(1);
  }
  BValue v = get_enum_index(opt, 0);
  assert_u64_equals(bsts_float64_to_bits(v), expected, message);
}

static BSTS_Array* test_array_unbox(BValue array) {
  return BSTS_PTR(BSTS_Array, array);
}

static BValue test_array_from_values(size_t len, const BValue* values) {
  BValue* data = NULL;
  if (len > 0) {
    data = (BValue*)GC_malloc(sizeof(BValue) * len);
    if (data == NULL) {
      perror("GC_malloc failure in test_array_from_values data");
      exit(1);
    }
    memcpy(data, values, sizeof(BValue) * len);
  }

  BSTS_Array* arr = (BSTS_Array*)GC_malloc(sizeof(BSTS_Array));
  if (arr == NULL) {
    perror("GC_malloc failure in test_array_from_values array");
    exit(1);
  }

  arr->data = data;
  arr->offset = 0;
  arr->len = (int)len;
  return BSTS_VALUE_FROM_PTR(arr);
}

static void assert_int_array_equals(BValue array, const int* expected, size_t expected_len, const char* message) {
  BSTS_Array* arr = test_array_unbox(array);
  if ((size_t)arr->len != expected_len) {
    printf("%s\nexpected len: %zu\ngot len: %d\n", message, expected_len, arr->len);
    exit(1);
  }

  for (size_t idx = 0; idx < expected_len; idx++) {
    BValue got = arr->data[arr->offset + (int)idx];
    if (bsts_integer_cmp(got, bsts_integer_from_int(expected[idx])) != 0) {
      printf("%s\nmismatch at index %zu\n", message, idx);
      exit(1);
    }
  }
}

static void assert_int64_array_bits(BValue array, const uint64_t* expected, size_t expected_len, const char* message) {
  BSTS_Array* arr = test_array_unbox(array);
  if ((size_t)arr->len != expected_len) {
    printf("%s\nexpected len: %zu\ngot len: %d\n", message, expected_len, arr->len);
    exit(1);
  }

  for (size_t idx = 0; idx < expected_len; idx++) {
    assert_u64_equals(
        bsts_int64_to_bits(arr->data[arr->offset + (int)idx]),
        expected[idx],
        message);
  }
}

static BValue array_identity_i64_fn(BValue arg) {
  return arg;
}

static BValue array_echo_i64_fn(BValue arg) {
  return arg;
}

static BValue array_fold_index_sum_fn(BValue acc, BValue item, BValue idx) {
  int64_t idx_i64 = (int64_t)bsts_int64_to_bits(idx);
  return bsts_integer_add(
      acc,
      bsts_integer_add(item, bsts_integer_from_int64(idx_i64)));
}

static BValue array_map_index_sum_fn(BValue item, BValue idx) {
  int64_t idx_i64 = (int64_t)bsts_int64_to_bits(idx);
  return bsts_integer_add(item, bsts_integer_from_int64(idx_i64));
}

static BValue array_zip_add_fn(BValue left, BValue right) {
  return bsts_integer_add(left, right);
}

static BValue array_zip_accum_add_fn(BValue acc, BValue left, BValue right) {
  return bsts_integer_add(acc, bsts_integer_add(left, right));
}

static BValue array_float_mul_fn(BValue left, BValue right) {
  return bsts_float64_from_double(
      bsts_float64_to_double(left) * bsts_float64_to_double(right));
}

void test_runtime_enum_struct() {
  BValue s1 = alloc_struct2(alloc_enum0(0), alloc_enum0(1));
  assert(get_variant(get_struct_index(s1, 0)) == 0, "index0 == alloc_enum0");
  assert(get_variant(get_struct_index(s1, 1)) == 1, "index0 == alloc_enum0(1)");
}

void test_integer() {
  assert_int_string(bsts_integer_from_int(0), "0", "to_string 0");
  assert_int_string(bsts_integer_from_int(-1), "-1", "to_string -1");
  assert_int_string(bsts_integer_from_int(123456), "123456", "to_string 123456");

  BValue i32_pos = bsts_integer_from_int(305419896); // 0x12345678
  BValue i32_neg = bsts_integer_from_int(-305419896);
  BValue i32_max = bsts_integer_from_int(INT32_MAX);
  BValue i32_min = bsts_integer_from_int(INT32_MIN);

  uint32_t i32_words[1] = { 0x12345678 };
  BValue i32_pos_big = bsts_integer_from_words_copy(1, 1, i32_words);
  BValue i32_neg_big = bsts_integer_from_words_copy(0, 1, i32_words);

  uint32_t i64_words[2] = { 0x9abcdef0, 0x12345678 };
  BValue i64_pos = bsts_integer_from_words_copy(1, 2, i64_words);
  BValue i64_neg = bsts_integer_from_words_copy(0, 2, i64_words);
  BValue i61_max = bsts_integer_from_int64((INT64_C(1) << 61) - 1);
  BValue i61_min = bsts_integer_from_int64(-(INT64_C(1) << 61));
  BValue i62_max = bsts_integer_from_int64((INT64_C(1) << 62) - 1);
  BValue i62_min = bsts_integer_from_int64(-(INT64_C(1) << 62));

  uint32_t i61_max_mag_words[2] = { 0xffffffff, 0x1fffffff };
  uint32_t i61_min_mag_words[2] = { 0x00000000, 0x20000000 };
  uint32_t i61_under_mag_words[2] = { 0x00000001, 0x20000000 };
  BValue i61_max_from_words = bsts_integer_from_words_copy(1, 2, i61_max_mag_words);
  BValue i61_min_from_words = bsts_integer_from_words_copy(0, 2, i61_min_mag_words);
  BValue i61_over = bsts_integer_from_words_copy(1, 2, i61_min_mag_words);
  BValue i61_under = bsts_integer_from_words_copy(0, 2, i61_under_mag_words);
  uint32_t i62_max_mag_words[2] = { 0xffffffff, 0x3fffffff };
  uint32_t i62_min_mag_words[2] = { 0x00000000, 0x40000000 };
  uint32_t i62_under_mag_words[2] = { 0x00000001, 0x40000000 };
  BValue i62_max_from_words = bsts_integer_from_words_copy(1, 2, i62_max_mag_words);
  BValue i62_min_from_words = bsts_integer_from_words_copy(0, 2, i62_min_mag_words);
  BValue i62_over = bsts_integer_from_words_copy(1, 2, i62_min_mag_words);
  BValue i62_under = bsts_integer_from_words_copy(0, 2, i62_under_mag_words);
  BValue s62_pos = bsts_integer_from_int64((INT64_C(1) << 40) + 1234);
  BValue s62_neg = bsts_integer_from_int64(-((INT64_C(1) << 40) + 1234));
  BValue pow40 = bsts_integer_from_int64(INT64_C(1) << 40);
  BValue pow2_32 = bsts_integer_from_int(32);
  BValue pow2_neg_32 = bsts_integer_from_int(-32);

  uint32_t i128_words[4] = { 0x9abcdef0, 0x12345678, 0x9abcdef0, 0x12345678 };
  BValue i128_pos = bsts_integer_from_words_copy(1, 4, i128_words);
  BValue i128_neg = bsts_integer_from_words_copy(0, 4, i128_words);
  uint32_t and_small_left_words[4] = { 0x12345678, 0x00000000, 0xaaaaaaaa, 0xaaaaaaaa };
  uint32_t and_small_right_words[4] = { 0xffffffff, 0x00000000, 0x55555555, 0x55555555 };
  uint32_t xor_small_left_words[4] = { 0x12345678, 0x00000000, 0xaaaaaaaa, 0xbbbbbbbb };
  uint32_t xor_small_right_words[4] = { 0x9abcdef0, 0x00000000, 0xaaaaaaaa, 0xbbbbbbbb };
  BValue and_small_left = bsts_integer_from_words_copy(1, 4, and_small_left_words);
  BValue and_small_right = bsts_integer_from_words_copy(1, 4, and_small_right_words);
  BValue xor_small_left = bsts_integer_from_words_copy(1, 4, xor_small_left_words);
  BValue xor_small_right = bsts_integer_from_words_copy(1, 4, xor_small_right_words);
  uint32_t pos_mismatch_left_words[16] = {
      123456789U, 0U, 0U, 2147483648U, 0U, 0U, 0U, 0U,
      0U, 2147483648U, 0U, 0U, 0U, 0U, 0U, 1U
  };
  uint32_t pos_mismatch_right_words[15] = {
      987654321U, 0U, 2147483648U, 0U, 0U, 0U, 0U, 2147483648U,
      0U, 0U, 0U, 0U, 0U, 0U, 2147483648U
  };
  uint32_t pos_mismatch_or_words[16] = {
      1071639989U, 0U, 2147483648U, 2147483648U, 0U, 0U, 0U, 2147483648U,
      0U, 2147483648U, 0U, 0U, 0U, 0U, 2147483648U, 1U
  };
  uint32_t pos_mismatch_xor_words[16] = {
      1032168868U, 0U, 2147483648U, 2147483648U, 0U, 0U, 0U, 2147483648U,
      0U, 2147483648U, 0U, 0U, 0U, 0U, 2147483648U, 1U
  };
  BValue pos_mismatch_left = bsts_integer_from_words_copy(1, 16, pos_mismatch_left_words);
  BValue pos_mismatch_right = bsts_integer_from_words_copy(1, 15, pos_mismatch_right_words);
  BValue pos_mismatch_or_expected = bsts_integer_from_words_copy(1, 16, pos_mismatch_or_words);
  BValue pos_mismatch_xor_expected = bsts_integer_from_words_copy(1, 16, pos_mismatch_xor_words);

  uint32_t neg_small_words[1] = { 123 };
  BValue neg_small_big = bsts_integer_from_words_copy(0, 1, neg_small_words);

  assert_int_string(i32_pos, "305419896", "i32_pos to_string");
  assert_int_string(i32_neg, "-305419896", "i32_neg to_string");
  assert_int_string(i32_max, "2147483647", "i32_max to_string");
  assert_int_string(i32_min, "-2147483648", "i32_min to_string");
  assert_int_string(i64_pos, "1311768467463790320", "i64_pos to_string");
  assert_int_string(i64_neg, "-1311768467463790320", "i64_neg to_string");
  assert_int_string(i61_max, "2305843009213693951", "i61_max to_string");
  assert_int_string(i61_min, "-2305843009213693952", "i61_min to_string");
  assert_int_string(i62_max, "4611686018427387903", "i62_max to_string");
  assert_int_string(i62_min, "-4611686018427387904", "i62_min to_string");
  assert_int_string(i61_over, "2305843009213693952", "i61_over to_string");
  assert_int_string(i61_under, "-2305843009213693953", "i61_under to_string");
  assert_int_string(i62_over, "4611686018427387904", "i62_over to_string");
  assert_int_string(i62_under, "-4611686018427387905", "i62_under to_string");
  assert_int_string(s62_pos, "1099511629010", "s62_pos to_string");
  assert_int_string(s62_neg, "-1099511629010", "s62_neg to_string");
  assert_int_string(i128_pos, "24197857203266734864793317670504947440", "i128_pos to_string");
  assert_int_string(i128_neg, "-24197857203266734864793317670504947440", "i128_neg to_string");

  assert_is_small_int(i64_pos, "i64_pos should be immediate");
  assert_is_small_int(i64_neg, "i64_neg should be immediate");
  assert_is_small_int(i61_max, "i61_max should be immediate");
  assert_is_small_int(i61_min, "i61_min should be immediate");
  assert_is_small_int(i62_max, "i62_max should be immediate");
  assert_is_small_int(i62_min, "i62_min should be immediate");
  assert_is_small_int(i61_max_from_words, "i61_max_from_words should canonicalize to immediate");
  assert_is_small_int(i61_min_from_words, "i61_min_from_words should canonicalize to immediate");
  assert_is_small_int(i62_max_from_words, "i62_max_from_words should canonicalize to immediate");
  assert_is_small_int(i62_min_from_words, "i62_min_from_words should canonicalize to immediate");
  assert_is_small_int(i61_over, "i61_over should now be immediate");
  assert_is_small_int(i61_under, "i61_under should now be immediate");
  assert_is_small_int(s62_pos, "s62_pos should be immediate");
  assert_is_small_int(s62_neg, "s62_neg should be immediate");
  assert_is_big_int(i62_over, "i62_over should spill to big-int");
  assert_is_big_int(i62_under, "i62_under should spill to big-int");

  assert(bsts_integer_equals(i32_pos, i32_pos), "i32_pos equals self");
  assert(bsts_integer_equals(i32_pos, i32_pos_big), "i32_pos equals big");
  assert(bsts_integer_equals(i32_neg, i32_neg_big), "i32_neg equals big");
  assert(!bsts_integer_equals(i32_pos, i32_neg), "i32_pos not equal i32_neg");
  assert(bsts_integer_equals(i128_pos, i128_pos), "i128_pos equals self");
  assert(!bsts_integer_equals(i64_pos, i64_neg), "i64_pos not equal i64_neg");

  assert_int_string(bsts_integer_negate(i32_pos), "-305419896", "negate i32_pos");
  assert_int_string(bsts_integer_negate(i32_neg), "305419896", "negate i32_neg");
  assert_int_string(bsts_integer_negate(i64_pos), "-1311768467463790320", "negate i64_pos");
  assert_int_string(bsts_integer_negate(i64_neg), "1311768467463790320", "negate i64_neg");
  assert_int_string(bsts_integer_negate(i128_pos), "-24197857203266734864793317670504947440", "negate i128_pos");
  assert_int_string(bsts_integer_negate(i128_neg), "24197857203266734864793317670504947440", "negate i128_neg");
  assert_int_string(bsts_integer_negate(i32_min), "2147483648", "negate i32_min");

  BValue add_i61_over = bsts_integer_add(i61_max, bsts_integer_from_int(1));
  BValue sub_i61_under = bsts_integer_add(i61_min, bsts_integer_from_int(-1));
  BValue neg_i61_min = bsts_integer_negate(i61_min);
  BValue add_i62_over = bsts_integer_add(i62_max, bsts_integer_from_int(1));
  BValue sub_i62_under = bsts_integer_add(i62_min, bsts_integer_from_int(-1));
  BValue neg_i62_min = bsts_integer_negate(i62_min);
  assert_int_string(add_i61_over, "2305843009213693952", "add i61_max 1");
  assert_int_string(sub_i61_under, "-2305843009213693953", "sub i61_min 1");
  assert_int_string(neg_i61_min, "2305843009213693952", "negate i61_min");
  assert_int_string(add_i62_over, "4611686018427387904", "add i62_max 1");
  assert_int_string(sub_i62_under, "-4611686018427387905", "sub i62_min 1");
  assert_int_string(neg_i62_min, "4611686018427387904", "negate i62_min");
  assert_is_small_int(add_i61_over, "add i61_max 1 should stay immediate");
  assert_is_small_int(sub_i61_under, "sub i61_min 1 should stay immediate");
  assert_is_small_int(neg_i61_min, "negate i61_min should stay immediate");
  assert_is_big_int(add_i62_over, "add i62_max 1 should spill to big-int");
  assert_is_big_int(sub_i62_under, "sub i62_min 1 should spill to big-int");
  assert_is_big_int(neg_i62_min, "negate i62_min should spill to big-int");

  assert(bsts_integer_to_int32(i32_pos) == 305419896, "to_int32 i32_pos");
  assert(bsts_integer_to_int32(i32_neg) == -305419896, "to_int32 i32_neg");
  assert(bsts_integer_to_int32(i32_pos_big) == 305419896, "to_int32 i32_pos_big");
  assert(bsts_integer_to_int32(neg_small_big) == -123, "to_int32 neg_small_big");
  assert(bsts_integer_to_int32(i64_pos) == -1698898192, "to_int32 i64_pos trunc");
  assert(bsts_integer_to_int32(i64_neg) == INT32_MIN, "to_int32 i64_neg sentinel");
  assert(bsts_integer_to_int32(i61_max) == -1, "to_int32 i61_max trunc");
  assert(bsts_integer_to_int32(i61_min) == INT32_MIN, "to_int32 i61_min sentinel");

  struct IntBinCase { const char* name; BValue a; BValue b; const char* expected; };
  struct IntBinCase add_cases[] = {
    { "add i32_pos i32_neg", i32_pos, i32_neg, "0" },
    { "add i64_pos i32_pos", i64_pos, i32_pos, "1311768467769210216" },
    { "add i128_pos i64_neg", i128_pos, i64_neg, "24197857203266734863481549203041157120" },
    { "add i128_neg i32_pos", i128_neg, i32_pos, "-24197857203266734864793317670199527544" },
  };
  for (size_t i = 0; i < sizeof(add_cases) / sizeof(add_cases[0]); i++) {
    assert_int_string(bsts_integer_add(add_cases[i].a, add_cases[i].b), add_cases[i].expected, add_cases[i].name);
  }

  struct IntBinCase mul_cases[] = {
    { "mul i32_pos i32_neg", i32_pos, i32_neg, "-93281312872650816" },
    { "mul i64_pos i32_pos", i64_pos, i32_pos, "400640188908870223300206720" },
    { "mul i128_pos i32_neg", i128_pos, i32_neg, "-7390507030444577022664749144420583314610266240" },
    { "mul i128_pos 32", i128_pos, pow2_32, "774331430504535515673386165456158318080" },
    { "mul i128_pos -32", i128_pos, pow2_neg_32, "-774331430504535515673386165456158318080" },
    { "mul i64_neg i64_neg", i64_neg, i64_neg, "1720736512232301123366780340925702400" },
  };
  for (size_t i = 0; i < sizeof(mul_cases) / sizeof(mul_cases[0]); i++) {
    assert_int_string(bsts_integer_times(mul_cases[i].a, mul_cases[i].b), mul_cases[i].expected, mul_cases[i].name);
  }

  BValue mul_small_small = bsts_integer_times(
      bsts_integer_from_int64(INT64_C(1) << 30),
      bsts_integer_from_int64(INT64_C(1) << 20));
  BValue mul_small_over = bsts_integer_times(
      bsts_integer_from_int64(INT64_C(1) << 31),
      bsts_integer_from_int64(INT64_C(1) << 31));
  assert_int_string(mul_small_small, "1125899906842624", "mul small range stays immediate");
  assert_int_string(mul_small_over, "4611686018427387904", "mul small overflow spills");
  assert_is_small_int(mul_small_small, "mul small range should stay immediate");
  assert_is_big_int(mul_small_over, "mul small overflow should spill to big-int");

  struct IntBinCase and_cases[] = {
    { "and i32_pos i32_neg", i32_pos, i32_neg, "8" },
    { "and i64_pos i64_neg", i64_pos, i64_neg, "16" },
    { "and i128_pos i64_pos", i128_pos, i64_pos, "1311768467463790320" },
    { "and s62_pos s62_neg", s62_pos, s62_neg, "2" },
  };
  for (size_t i = 0; i < sizeof(and_cases) / sizeof(and_cases[0]); i++) {
    assert_int_string(bsts_integer_and(and_cases[i].a, and_cases[i].b), and_cases[i].expected, and_cases[i].name);
  }

  struct IntBinCase or_cases[] = {
    { "or i32_pos i32_neg", i32_pos, i32_neg, "-8" },
    { "or i64_pos i64_neg", i64_pos, i64_neg, "-16" },
    { "or i128_pos i64_pos", i128_pos, i64_pos, "24197857203266734864793317670504947440" },
    { "or s62_pos s62_neg", s62_pos, s62_neg, "-2" },
  };
  for (size_t i = 0; i < sizeof(or_cases) / sizeof(or_cases[0]); i++) {
    assert_int_string(bsts_integer_or(or_cases[i].a, or_cases[i].b), or_cases[i].expected, or_cases[i].name);
  }

  struct IntBinCase xor_cases[] = {
    { "xor i32_pos i32_neg", i32_pos, i32_neg, "-16" },
    { "xor i64_pos i64_neg", i64_pos, i64_neg, "-32" },
    { "xor i128_pos i64_pos", i128_pos, i64_pos, "24197857203266734863481549203041157120" },
    { "xor s62_pos s62_neg", s62_pos, s62_neg, "-4" },
  };
  for (size_t i = 0; i < sizeof(xor_cases) / sizeof(xor_cases[0]); i++) {
    assert_int_string(bsts_integer_xor(xor_cases[i].a, xor_cases[i].b), xor_cases[i].expected, xor_cases[i].name);
  }

  BValue and_small_result = bsts_integer_and(and_small_left, and_small_right);
  BValue xor_small_result = bsts_integer_xor(xor_small_left, xor_small_right);
  BValue and_zero_result = bsts_integer_and(i128_pos, bsts_integer_from_int(0));
  BValue and_neg1_result = bsts_integer_and(i128_pos, bsts_integer_from_int(-1));
  BValue or_zero_result = bsts_integer_or(i128_pos, bsts_integer_from_int(0));
  BValue or_neg1_result = bsts_integer_or(i128_pos, bsts_integer_from_int(-1));
  BValue xor_zero_result = bsts_integer_xor(i128_pos, bsts_integer_from_int(0));
  BValue xor_neg1_result = bsts_integer_xor(i128_pos, bsts_integer_from_int(-1));
  assert_int_string(and_small_result, "305419896", "and big big small result");
  assert_int_string(xor_small_result, "2290649224", "xor big big small result");
  assert_int_string(and_zero_result, "0", "and big zero");
  assert_int_string(and_neg1_result, "24197857203266734864793317670504947440", "and big neg1");
  assert_int_string(or_zero_result, "24197857203266734864793317670504947440", "or big zero");
  assert_int_string(or_neg1_result, "-1", "or big neg1");
  assert_int_string(xor_zero_result, "24197857203266734864793317670504947440", "xor big zero");
  assert(bsts_integer_equals(xor_neg1_result, bsts_integer_not(i128_pos)), "xor big neg1 equals not");
  assert(bsts_integer_equals(
      bsts_integer_or(pos_mismatch_left, pos_mismatch_right),
      pos_mismatch_or_expected),
      "or positive mismatched lengths");
  assert(bsts_integer_equals(
      bsts_integer_xor(pos_mismatch_left, pos_mismatch_right),
      pos_mismatch_xor_expected),
      "xor positive mismatched lengths");
  assert_is_small_int(and_small_result, "and big big small should canonicalize to immediate");
  assert_is_small_int(xor_small_result, "xor big big small should canonicalize to immediate");
  assert_is_small_int(and_zero_result, "and big zero should be immediate");
  assert_is_small_int(or_neg1_result, "or big neg1 should be immediate");

  struct IntShiftCase { const char* name; BValue value; int shift; const char* expected; };
  struct IntShiftCase shift_cases[] = {
    { "shift i32_pos << 5", i32_pos, 5, "9773436672" },
    { "shift i32_neg << 1", i32_neg, 1, "-610839792" },
    { "shift i32_pos >> 1", i32_pos, -1, "152709948" },
    { "shift i32_neg >> 5", i32_neg, -5, "-9544372" },
    { "shift i64_pos << 17", i64_pos, 17, "171936116567413924823040" },
    { "shift pow40 >> 32", pow40, -32, "256" },
    { "shift i64_neg >> 17", i64_neg, -17, "-10007999171935" },
    { "shift pow40 << 5", pow40, 5, "35184372088832" },
    { "shift pow40 << 30", pow40, 30, "1180591620717411303424" },
    { "shift i128_pos << 33", i128_pos, 33, "207858010642617301217980562388315306121997844480" },
    { "shift i128_pos >> 33", i128_pos, -33, "2817001333840509744453397308" },
    { "shift i128_neg >> 33", i128_neg, -33, "-2817001333840509744453397309" },
  };
  for (size_t i = 0; i < sizeof(shift_cases) / sizeof(shift_cases[0]); i++) {
    BValue shift = bsts_integer_from_int(shift_cases[i].shift);
    assert_int_string(bsts_integer_shift_left(shift_cases[i].value, shift), shift_cases[i].expected, shift_cases[i].name);
  }

  BValue shift_twos_small = bsts_integer_shift_left(bsts_integer_from_int(1), bsts_integer_from_int(40));
  BValue shift_small_left1 = bsts_integer_shift_left(i32_pos, bsts_integer_from_int(1));
  BValue shift_small_right1 = bsts_integer_shift_left(i32_pos, bsts_integer_from_int(-1));
  BValue shift_big_right64_small = bsts_integer_shift_left(i128_pos, bsts_integer_from_int(-64));
  BValue shift_big_right96_small = bsts_integer_shift_left(i128_pos, bsts_integer_from_int(-96));
  BValue shift_twos_big = bsts_integer_shift_left(pow40, bsts_integer_from_int(30));
  assert_int_string(shift_twos_small, "1099511627776", "shift twos small canonicalization");
  assert_int_string(shift_small_left1, "610839792", "shift small left1 canonicalization");
  assert_int_string(shift_small_right1, "152709948", "shift small right1 canonicalization");
  assert_int_string(shift_big_right64_small, "1311768467463790320", "shift big right64 canonicalization");
  assert_int_string(shift_big_right96_small, "305419896", "shift big right96 canonicalization");
  assert_is_small_int(shift_twos_small, "shift twos small should stay immediate");
  assert_is_small_int(shift_small_left1, "shift small left1 should stay immediate");
  assert_is_small_int(shift_small_right1, "shift small right1 should stay immediate");
  assert_is_small_int(shift_big_right64_small, "shift big right64 should canonicalize to immediate");
  assert_is_small_int(shift_big_right96_small, "shift big right96 should canonicalize to immediate");
  assert_is_big_int(shift_twos_big, "shift pow40 << 30 should spill to big-int");

  struct IntCmpCase { const char* name; BValue a; BValue b; int expected; };
  struct IntCmpCase cmp_cases[] = {
    { "cmp i32_neg i32_pos", i32_neg, i32_pos, -1 },
    { "cmp i64_pos i32_pos", i64_pos, i32_pos, 1 },
    { "cmp i64_neg i32_neg", i64_neg, i32_neg, -1 },
    { "cmp i128_pos i128_pos", i128_pos, i128_pos, 0 },
    { "cmp i128_neg i64_neg", i128_neg, i64_neg, -1 },
  };
  for (size_t i = 0; i < sizeof(cmp_cases) / sizeof(cmp_cases[0]); i++) {
    int cmp = bsts_integer_cmp(cmp_cases[i].a, cmp_cases[i].b);
    if ((cmp_cases[i].expected < 0 && cmp >= 0) ||
        (cmp_cases[i].expected == 0 && cmp != 0) ||
        (cmp_cases[i].expected > 0 && cmp <= 0)) {
      printf("cmp failed: %s got %d\n", cmp_cases[i].name, cmp);
      exit(1);
    }
  }

  struct IntDivModCase { const char* name; BValue a; BValue b; const char* div; const char* mod; };
  struct IntDivModCase div_cases[] = {
    { "divmod i32_pos 7", i32_pos, bsts_integer_from_int(7), "43631413", "5" },
    { "divmod i32_neg 7", i32_neg, bsts_integer_from_int(7), "-43631414", "2" },
    { "divmod i64_pos -12345", i64_pos, bsts_integer_from_int(-12345), "-106259090114524", "-8460" },
    { "divmod i61_max 7", i61_max, bsts_integer_from_int(7), "329406144173384850", "1" },
    { "divmod i61_min 7", i61_min, bsts_integer_from_int(7), "-329406144173384851", "5" },
    { "divmod i128_neg i64_pos", i128_neg, i64_pos, "-18446744073709551617", "0" },
    { "divmod i128_pos 32", i128_pos, bsts_integer_from_int(32), "756183037602085464524791177203279607", "16" },
    { "divmod i128_neg 32", i128_neg, bsts_integer_from_int(32), "-756183037602085464524791177203279608", "16" },
    { "divmod i128_pos -32", i128_pos, bsts_integer_from_int(-32), "-756183037602085464524791177203279608", "-16" },
    { "divmod i128_neg -32", i128_neg, bsts_integer_from_int(-32), "756183037602085464524791177203279607", "-16" },
    { "divmod i128_pos 2^65", i128_pos, bsts_integer_shift_left(bsts_integer_from_int(1), bsts_integer_from_int(65)), "655884233731895160", "1311768467463790320" },
    { "divmod i128_neg 2^65", i128_neg, bsts_integer_shift_left(bsts_integer_from_int(1), bsts_integer_from_int(65)), "-655884233731895161", "35581719679955312912" },
    { "divmod i128_pos -2^65", i128_pos, bsts_integer_negate(bsts_integer_shift_left(bsts_integer_from_int(1), bsts_integer_from_int(65))), "-655884233731895161", "-35581719679955312912" },
    { "divmod i64_pos 0", i64_pos, bsts_integer_from_int(0), "0", "1311768467463790320" },
  };
  for (size_t i = 0; i < sizeof(div_cases) / sizeof(div_cases[0]); i++) {
    BValue dm = bsts_integer_div_mod(div_cases[i].a, div_cases[i].b);
    BValue div = get_struct_index(dm, 0);
    BValue mod = get_struct_index(dm, 1);
    assert_int_string(div, div_cases[i].div, div_cases[i].name);
    assert_int_string(mod, div_cases[i].mod, div_cases[i].name);
  }

  assert_int_string(
      ___bsts_g_Bosatsu_l_Predef_l_gcd__Int(
          i128_pos,
          bsts_integer_shift_left(bsts_integer_from_int(1), bsts_integer_from_int(65))),
      "16",
      "gcd i128_pos 2^65");

  struct StrToIntCase { const char* name; const char* text; const char* expected; };
  struct StrToIntCase str_cases[] = {
    { "stoi i32_pos", "305419896", "305419896" },
    { "stoi i32_neg", "-305419896", "-305419896" },
    { "stoi i32_max", "2147483647", "2147483647" },
    { "stoi i32_min", "-2147483648", "-2147483648" },
    { "stoi i64_pos", "1311768467463790320", "1311768467463790320" },
    { "stoi i64_neg", "-1311768467463790320", "-1311768467463790320" },
    { "stoi i61_max", "2305843009213693951", "2305843009213693951" },
    { "stoi i61_min", "-2305843009213693952", "-2305843009213693952" },
    { "stoi i61_over", "2305843009213693952", "2305843009213693952" },
    { "stoi i61_under", "-2305843009213693953", "-2305843009213693953" },
    { "stoi i128_pos", "24197857203266734864793317670504947440", "24197857203266734864793317670504947440" },
    { "stoi i128_neg", "-24197857203266734864793317670504947440", "-24197857203266734864793317670504947440" },
  };
  for (size_t i = 0; i < sizeof(str_cases) / sizeof(str_cases[0]); i++) {
    BValue s = bsts_string_from_utf8_bytes_static(strlen(str_cases[i].text), str_cases[i].text);
    BValue opt = bsts_string_to_integer(s);
    assert_option_int(opt, str_cases[i].expected, str_cases[i].name);
  }

  struct StrToIntCase none_cases[] = {
    { "stoi empty", "", NULL },
    { "stoi dash", "-", NULL },
    { "stoi junk", "12x3", NULL },
  };
  for (size_t i = 0; i < sizeof(none_cases) / sizeof(none_cases[0]); i++) {
    BValue s = bsts_string_from_utf8_bytes_static(strlen(none_cases[i].text), none_cases[i].text);
    BValue opt = bsts_string_to_integer(s);
    assert_option_none(opt, none_cases[i].name);
  }
}

void test_runtime_strings() {

  char* hello = "hello1";

#if !defined(_WIN32)
  assert_child_aborts(call_string_copy_too_large, "oversized copy string should abort");
  assert_child_aborts(call_string_static_too_large, "oversized static string should abort");
  assert_child_aborts(call_string_mut_too_large, "oversized mutable string should abort");
#endif

  {
    BValue ch_ascii = bsts_char_from_code_point('A');
    BValue ch_null = bsts_char_from_code_point(0);
    BValue ch_smile = bsts_char_from_code_point(0x1F60A);
    assert(bsts_char_code_point_from_value(ch_ascii) == 'A', "char ascii roundtrip");
    assert(bsts_char_code_point_from_value(ch_null) == 0, "char NUL roundtrip");
    assert(bsts_char_code_point_from_value(ch_smile) == 0x1F60A, "char 4-byte roundtrip");
  }

  BValue v1 = bsts_string_from_utf8_bytes_copy(5, "hello");
  // we can ignore trailing byte string on hello, by taking the front
  BValue v2 = bsts_string_from_utf8_bytes_static(5, hello);
  assert(bsts_string_equals(v1, v2), "v1 == v2");
  assert(bsts_string_equals(v1, v1), "v1 == v1");
  assert(bsts_string_equals(v2, v2), "v2 == v2");
  //codepoint tests
  assert(bsts_string_code_point_bytes(v1, 0) == 1, "code_point_bytes(v[0]) == 1");
  assert(bsts_string_char_at(v1, 0) == bsts_char_from_code_point(104), "char_at(v, 0) == 104");
  assert(bsts_string_char_at(v1, 1) == bsts_char_from_code_point(101), "char_at(v, 1) == 101");
  BValue v3 = bsts_string_from_utf8_bytes_static(4, "\x00F0\x009F\x0098\x008A");
  assert(bsts_string_char_at(v3, 0) == bsts_char_from_code_point(0x1F60A), "smiley check char");
  assert(bsts_string_code_point_bytes(v3, 0) == 4, "smiley length");

  BValue v1tail = bsts_string_substring_tail(v1, 1);
  BValue v2tail = bsts_string_substring_tail(v2, 1);
  BValue tail_expected = bsts_string_from_utf8_bytes_static(4, "ello");
  assert(bsts_string_equals(v1tail, v2tail), "v1tail == v2tail");
  assert(bsts_string_equals(v1tail, tail_expected), "v1tail == expected");

  {
    BValue hello_world1 = bsts_string_from_utf8_bytes_static(11, "hello world");
    BValue hello1 = bsts_string_from_utf8_bytes_static(5, "world");
    int find1 = bsts_string_find(hello_world1, hello1, 0);
    assert(find1 == 6, "find1");
    int find2 = bsts_string_find(hello_world1, hello1, 1);
    assert(find2 == 6, "find2");
    int find3 = bsts_string_find(hello_world1, hello1, 7);
    assert(find3 == -1, "find3");
  }

  {
    BValue empty_a = bsts_string_from_utf8_bytes_static(0, NULL);
    BValue empty_b = bsts_string_from_utf8_bytes_static(0, "");
    assert(bsts_string_equals(empty_a, empty_b), "empty small strings compare equal");
    assert(bsts_string_utf8_len_ref(&empty_a) == 0, "empty small string has len 0");
    free_on_close(empty_a);
  }

  {
    BValue s7_static = bsts_string_from_utf8_bytes_static(7, "abcdefg");
    BValue s7_copy = bsts_string_from_utf8_bytes_copy(7, "abcdefg");
    BValue s8_static = bsts_string_from_utf8_bytes_static(8, "abcdefgh");
    assert(bsts_string_equals(s7_static, s7_copy), "small static/copy equals");
    assert(bsts_string_cmp(s7_static, s7_copy) == 0, "small static/copy cmp");
    assert(bsts_string_cmp(s7_static, s8_static) < 0, "small cmp against heap string");
    assert_string_bytes(s7_static, "abcdefg", 7, "small bytes roundtrip");
    assert_string_bytes(s8_static, "abcdefgh", 8, "heap bytes roundtrip");
  }

  {
    BValue barbara = bsts_string_from_utf8_bytes_static(7, "BARBARA");
    BValue linda = bsts_string_from_utf8_bytes_static(5, "LINDA");
    BValue a = bsts_string_from_utf8_bytes_static(1, "A");
    BValue c = bsts_string_from_utf8_bytes_static(1, "C");
    assert(bsts_string_cmp(barbara, linda) == -1, "cmp BARBARA < LINDA normalized");
    assert(bsts_string_cmp(linda, barbara) == 1, "cmp LINDA > BARBARA normalized");
    assert(bsts_string_cmp(a, c) == -1, "cmp A < C normalized");
    assert(bsts_string_cmp(c, a) == 1, "cmp C > A normalized");
  }

  {
    BValue u_e000 = bsts_char_from_code_point(0xE000);
    BValue u_ffff = bsts_char_from_code_point(0xFFFF);
    BValue u_10000 = bsts_char_from_code_point(0x10000);
    BValue a_u_e000 = bsts_string_from_utf8_bytes_static(4, "a\xEE\x80\x80");
    BValue a_u_10000 = bsts_string_from_utf8_bytes_static(5, "a\xF0\x90\x80\x80");

    assert(bsts_string_cmp(u_e000, u_10000) == -1, "cmp U+E000 < U+10000");
    assert(bsts_string_cmp(u_ffff, u_10000) == -1, "cmp U+FFFF < U+10000");
    assert(bsts_string_cmp(a_u_e000, a_u_10000) == -1, "cmp a+U+E000 < a+U+10000");
    assert(___bsts_g_Bosatsu_l_Predef_l_cmp__Char(u_e000, u_10000) == alloc_enum0(0), "cmp_Char U+E000 < U+10000");
    assert(___bsts_g_Bosatsu_l_Predef_l_eq__Char(u_e000, u_e000) == alloc_enum0(1), "eq_Char same");
    assert(___bsts_g_Bosatsu_l_Predef_l_eq__Char(u_e000, u_10000) == alloc_enum0(0), "eq_Char different");
  }

  {
    BValue long_s = bsts_string_from_utf8_bytes_static(10, "0123456789");
    BValue mid = bsts_string_substring(long_s, 3, 7);
    BValue tail = bsts_string_substring_tail(long_s, 8);
    assert_string_bytes(mid, "3456", 4, "substring long->small");
    assert_string_bytes(tail, "89", 2, "substring_tail long->small");
    assert(bsts_string_find(long_s, mid, 0) == 3, "find long with small needle");
    assert(bsts_string_rfind(long_s, bsts_string_from_utf8_bytes_static(2, "89"), 9) == 8, "rfind long with small needle");
  }

  {
    BValue small_hay = bsts_string_from_utf8_bytes_static(7, "abcbcba");
    BValue small_need = bsts_string_from_utf8_bytes_static(2, "bc");
    assert(bsts_string_find(small_hay, small_need, 0) == 1, "find in small haystack");
    assert(bsts_string_rfind(small_hay, small_need, 6) == 3, "rfind in small haystack");
    assert(bsts_string_char_at(small_hay, 6) == bsts_char_from_code_point('a'), "char_at small haystack");
  }

  {
    BValue a = bsts_string_from_utf8_bytes_static(3, "foo");
    BValue b = bsts_string_from_utf8_bytes_static(3, "bar");
    const char* a_bytes = bsts_string_utf8_bytes_ref(&a);
    char a_copy[3];
    memcpy(a_copy, a_bytes, 3);
    const char* b_bytes = bsts_string_utf8_bytes_ref(&b);
    assert(memcmp(a_copy, "foo", 3) == 0, "first small bytes survive second lookup");
    assert(memcmp(b_bytes, "bar", 3) == 0, "second small bytes are correct");
  }

  {
    BValue tiny = bsts_string_from_utf8_bytes_static(5, "abcde");
    BSTS_String_View view = bsts_string_view_ref(&tiny);
    const char* expected = ((const char*)(const void*)&tiny) + 1;
    assert(view.len == 5, "tiny view length");
    assert(view.bytes == expected, "tiny view points inside BValue storage");
    assert(view.bytes[5] == '\0', "tiny view is NUL terminated");
  }

  {
    BValue inline16 = bsts_string_from_utf8_bytes_copy(16, "abcdefghijklmnop");
    BSTS_String* inline16_str = BSTS_PTR(BSTS_String, inline16);
    assert((inline16_str->len_meta & BSTS_STRING_INLINE16_FLAG) != 0, "len16 string is inline");
    assert_string_bytes(inline16, "abcdefghijklmnop", 16, "len16 bytes roundtrip");

    BValue large = bsts_string_from_utf8_bytes_copy(17, "abcdefghijklmnopq");
    BSTS_String* large_str = BSTS_PTR(BSTS_String, large);
    assert((large_str->len_meta & BSTS_STRING_INLINE16_FLAG) == 0, "len17 string is external");
    assert_string_bytes(large, "abcdefghijklmnopq", 17, "len17 bytes roundtrip");
  }

  {
    static const BSTS_String static_lit = BSTS_STATIC_STRING_INIT(18, "abcdefghijklmnopqr");
    BValue lit = BSTS_VALUE_FROM_PTR(&static_lit);
    const char* lit_bytes = bsts_string_utf8_bytes_ref(&lit);
    assert_string_bytes(lit, "abcdefghijklmnopqr", 18, "boxed static literal bytes");
    assert(lit_bytes == static_lit.payload.ext.bytes, "boxed static literal is zero-copy");
  }

}

void test_float64() {
  uint64_t bits_cases[] = {
    UINT64_C(0x0000000000000000),
    UINT64_C(0x8000000000000000),
    UINT64_C(0x3ff0000000000000),
    UINT64_C(0xbff0000000000000),
    UINT64_C(0x7ff0000000000000),
    UINT64_C(0xfff0000000000000),
    UINT64_C(0x7ff8000000000001),
    UINT64_C(0x7ff8000000000002),
    UINT64_C(0x0000000000000001),
    UINT64_C(0x7fefffffffffffff)
  };
  for (size_t i = 0; i < sizeof(bits_cases) / sizeof(bits_cases[0]); i++) {
    BValue v = bsts_float64_from_bits(bits_cases[i]);
    assert_u64_equals(bsts_float64_to_bits(v), bits_cases[i], "float bits roundtrip");
  }

  BValue neg_zero = bsts_float64_from_bits(UINT64_C(0x8000000000000000));
  BValue pos_zero = bsts_float64_from_bits(UINT64_C(0x0000000000000000));
  BValue nan1 = bsts_float64_from_bits(UINT64_C(0x7ff8000000000001));
  BValue nan2 = bsts_float64_from_bits(UINT64_C(0x7ff8000000000002));
  BValue one = bsts_float64_from_double(1.0);
  BValue two = bsts_float64_from_double(2.0);

  assert(bsts_float64_cmp_total(nan1, one) < 0, "nan sorts before non-nan");
  assert(bsts_float64_cmp_total(one, nan1) > 0, "non-nan sorts after nan");
  assert(bsts_float64_cmp_total(nan1, nan2) == 0, "all nan compare equal");
  assert(bsts_float64_cmp_total(neg_zero, pos_zero) == 0, "-0.0 compares equal to 0.0");
  assert(bsts_float64_cmp_total(one, two) < 0, "1.0 < 2.0");
  assert(bsts_float64_equals(neg_zero, pos_zero) == 1, "float equality treats signed zeros as equal");
  assert(bsts_float64_equals(nan1, nan2) == 1, "float equality matches all nan values");
  assert(get_variant_value(___bsts_g_Bosatsu_l_Predef_l_eq__Float64(neg_zero, pos_zero)) == 1,
      "predef eq_Float64 treats signed zeros as equal");
  assert(get_variant_value(___bsts_g_Bosatsu_l_Predef_l_eq__Float64(nan1, nan2)) == 1,
      "predef eq_Float64 treats all nan values as equal");
  assert(get_variant_value(___bsts_g_Bosatsu_l_Predef_l_eq__Float64(one, two)) == 0,
      "predef eq_Float64 is false for different finite values");

  for (size_t i = 0; i < sizeof(bits_cases) / sizeof(bits_cases[0]); i++) {
    BValue f = bsts_float64_from_bits(bits_cases[i]);
    BValue as_int = ___bsts_g_Bosatsu_l_Num_l_Float64_l_float64__bits__to__Int(f);
    BValue round = ___bsts_g_Bosatsu_l_Num_l_Float64_l_int__bits__to__Float64(as_int);
    assert_u64_equals(bsts_float64_to_bits(round), bits_cases[i], "float64 <-> int bits roundtrip");
  }

  BValue minus_one = bsts_integer_from_int(-1);
  BValue minus_one_float = ___bsts_g_Bosatsu_l_Num_l_Float64_l_int__bits__to__Float64(minus_one);
  assert_u64_equals(bsts_float64_to_bits(minus_one_float), UINT64_C(0xffffffffffffffff), "int_bits uses low 64 two's complement");

  {
    BValue inf_str = bsts_string_from_utf8_bytes_static(3, "\xE2\x88\x9E");
    BValue parsed = ___bsts_g_Bosatsu_l_Num_l_Float64_l_string__to__Float64(inf_str);
    assert_option_float_bits(parsed, UINT64_C(0x7ff0000000000000), "parse +infinity");
  }
  {
    BValue ninf_str = bsts_string_from_utf8_bytes_static(4, "-\xE2\x88\x9E");
    BValue parsed = ___bsts_g_Bosatsu_l_Num_l_Float64_l_string__to__Float64(ninf_str);
    assert_option_float_bits(parsed, UINT64_C(0xfff0000000000000), "parse -infinity");
  }
  {
    uint64_t nan_bits = UINT64_C(0x7ff80000000000ab);
    BValue nanv = bsts_float64_from_bits(nan_bits);
    BValue nan_str = ___bsts_g_Bosatsu_l_Num_l_Float64_l_float64__to__String(nanv);
    BValue parsed = ___bsts_g_Bosatsu_l_Num_l_Float64_l_string__to__Float64(nan_str);
    assert_option_float_bits(parsed, nan_bits, "parse NaN payload");
  }
  {
    BValue nan_lit = bsts_string_from_utf8_bytes_static(4, ".NaN");
    BValue parsed = ___bsts_g_Bosatsu_l_Num_l_Float64_l_string__to__Float64(nan_lit);
    assert(get_variant(parsed) == 1, "parse .NaN returns Some");
    assert(isnan(bsts_float64_to_double(get_enum_index(parsed, 0))), "parse .NaN produces NaN");
  }
  {
    BValue bad = bsts_string_from_utf8_bytes_static(6, "nope42");
    BValue parsed = ___bsts_g_Bosatsu_l_Num_l_Float64_l_string__to__Float64(bad);
    assert(get_variant(parsed) == 0, "invalid float string returns None");
  }
}

void test_int64() {
  uint64_t bits_cases[] = {
    UINT64_C(0x0000000000000000),
    UINT64_C(0x0000000000000001),
    UINT64_C(0xffffffffffffffff),
    UINT64_C(0x7fffffffffffffff),
    UINT64_C(0x8000000000000000),
    UINT64_C(0x1234567890abcdef)
  };

  for (size_t i = 0; i < sizeof(bits_cases) / sizeof(bits_cases[0]); i++) {
    BValue v = bsts_int64_from_bits(bits_cases[i]);
    assert_u64_equals(bsts_int64_to_bits(v), bits_cases[i], "int64 bits roundtrip");
  }

  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_min__i64(),
      UINT64_C(0x8000000000000000),
      "min_i64 bits");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_max__i64(),
      UINT64_C(0x7fffffffffffffff),
      "max_i64 bits");

  assert_option_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_int__to__Int64(bsts_integer_from_int64(INT64_MIN)),
      UINT64_C(0x8000000000000000),
      "safe int64 min conversion");
  assert_option_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_int__to__Int64(bsts_integer_from_int64(INT64_MAX)),
      UINT64_C(0x7fffffffffffffff),
      "safe int64 max conversion");
  {
    BValue too_big = bsts_integer_add(
        bsts_integer_from_int64(INT64_MAX),
        bsts_integer_from_int(1));
    BValue too_small = bsts_integer_add(
        bsts_integer_from_int64(INT64_MIN),
        bsts_integer_from_int(-1));
    assert_option_none(
        ___bsts_g_Bosatsu_l_Num_l_Int64_l_int__to__Int64(too_big),
        "safe int64 rejects max + 1");
    assert_option_none(
        ___bsts_g_Bosatsu_l_Num_l_Int64_l_int__to__Int64(too_small),
        "safe int64 rejects min - 1");
  }

  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_int__low__bits__to__Int64(bsts_integer_from_int(-1)),
      UINT64_C(0xffffffffffffffff),
      "low bits conversion keeps two's complement");
  assert_int_string(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_int64__to__Int(bsts_int64_from_bits(UINT64_C(0xffffffffffffffff))),
      "-1",
      "int64_to_Int decodes signed payload");

  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_add__Int64(
          bsts_int64_from_int64(INT64_MAX),
          bsts_int64_from_int64(1)),
      UINT64_C(0x8000000000000000),
      "add wraps to min_i64");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_sub__Int64(
          bsts_int64_from_int64(INT64_MIN),
          bsts_int64_from_int64(1)),
      UINT64_C(0x7fffffffffffffff),
      "sub wraps to max_i64");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_mul__Int64(
          bsts_int64_from_int64(INT64_MIN),
          bsts_int64_from_int64(-1)),
      UINT64_C(0x8000000000000000),
      "mul wraps min_i64 * -1");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_div__Int64(
          bsts_int64_from_int64(-5),
          bsts_int64_from_int64(3)),
      UINT64_C(0xfffffffffffffffe),
      "division uses floor semantics");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_div__Int64(
          bsts_int64_from_int64(INT64_MIN),
          bsts_int64_from_int64(-1)),
      UINT64_C(0x8000000000000000),
      "division overflow wraps to min_i64");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_div__Int64(
          bsts_int64_from_int64(1234),
          bsts_int64_from_int64(0)),
      UINT64_C(0x0000000000000000),
      "division by zero returns zero");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_mod__Int64(
          bsts_int64_from_int64(5),
          bsts_int64_from_int64(-3)),
      UINT64_C(0xffffffffffffffff),
      "mod_Int64 keeps the divisor sign");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_mod__Int64(
          bsts_int64_from_int64(1234),
          bsts_int64_from_int64(0)),
      UINT64_C(0x00000000000004d2),
      "mod_Int64 by zero returns the left value");

  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_and__Int64(
          bsts_int64_from_bits(UINT64_C(0xffff0000ffff0000)),
          bsts_int64_from_bits(UINT64_C(0x0f0f0f0f0f0f0f0f))),
      UINT64_C(0x0f0f00000f0f0000),
      "bitwise and keeps low bits");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_or__Int64(
          bsts_int64_from_bits(UINT64_C(0xf0f00000f0f00000)),
          bsts_int64_from_bits(UINT64_C(0x0f0f0f0f0f0f0f0f))),
      UINT64_C(0xffff0f0fffff0f0f),
      "bitwise or keeps low bits");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_xor__Int64(
          bsts_int64_from_bits(UINT64_C(0xffff0000ffff0000)),
          bsts_int64_from_bits(UINT64_C(0x0f0f0f0f0f0f0f0f))),
      UINT64_C(0xf0f00f0ff0f00f0f),
      "bitwise xor keeps low bits");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_not__Int64(bsts_int64_from_int64(0)),
      UINT64_C(0xffffffffffffffff),
      "bitwise not flips all bits");

  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_shift__left__Int64(
          bsts_int64_from_int64(1),
          bsts_integer_from_int(63)),
      UINT64_C(0x8000000000000000),
      "left shift keeps low 64 bits");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_shift__left__Int64(
          bsts_int64_from_int64(1),
          bsts_integer_from_int(64)),
      UINT64_C(0x0000000000000000),
      "left shift by >= 64 clears the value");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_shift__left__Int64(
          bsts_int64_from_int64(1),
          bsts_integer_from_int(-1)),
      UINT64_C(0x0000000000000000),
      "negative left shift becomes arithmetic right shift");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_shift__right__Int64(
          bsts_int64_from_int64(-1),
          bsts_integer_from_int(100)),
      UINT64_C(0xffffffffffffffff),
      "right shift keeps sign for large positive counts");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_shift__right__Int64(
          bsts_int64_from_int64(1),
          bsts_integer_from_int(-63)),
      UINT64_C(0x8000000000000000),
      "negative right shift becomes wrapped left shift");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_shift__right__unsigned__Int64(
          bsts_int64_from_bits(UINT64_C(0xffffffffffffffff)),
          bsts_integer_from_int(1)),
      UINT64_C(0x7fffffffffffffff),
      "unsigned right shift clears the sign bit");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_shift__right__unsigned__Int64(
          bsts_int64_from_int64(1),
          bsts_integer_from_int(-63)),
      UINT64_C(0x8000000000000000),
      "negative unsigned right shift becomes wrapped left shift");
  {
    BValue huge_shift =
        bsts_integer_shift_left(bsts_integer_from_int(1), bsts_integer_from_int(70));
    BValue huge_neg_shift = bsts_integer_negate(huge_shift);
    assert_int64_bits(
        ___bsts_g_Bosatsu_l_Num_l_Int64_l_shift__left__Int64(
            bsts_int64_from_int64(1),
            huge_shift),
        UINT64_C(0x0000000000000000),
        "boxed huge left shift clears the value");
    assert_int64_bits(
        ___bsts_g_Bosatsu_l_Num_l_Int64_l_shift__left__Int64(
            bsts_int64_from_int64(-1),
            huge_neg_shift),
        UINT64_C(0xffffffffffffffff),
        "boxed huge negative left shift sign-fills");
    assert_int64_bits(
        ___bsts_g_Bosatsu_l_Num_l_Int64_l_shift__right__Int64(
            bsts_int64_from_int64(-1),
            huge_shift),
        UINT64_C(0xffffffffffffffff),
        "boxed huge right shift keeps the sign bit");
    assert_int64_bits(
        ___bsts_g_Bosatsu_l_Num_l_Int64_l_shift__right__unsigned__Int64(
            bsts_int64_from_bits(UINT64_C(0xffffffffffffffff)),
            huge_shift),
        UINT64_C(0x0000000000000000),
        "boxed huge unsigned right shift clears the value");
  }
  assert_int_string(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_popcount__Int64(
          bsts_int64_from_bits(UINT64_C(0xffffffffffffffff))),
      "64",
      "popcount_Int64 counts raw bits");
  assert(
      get_variant(
          ___bsts_g_Bosatsu_l_Num_l_Int64_l_eq__Int64(
              bsts_int64_from_int64(7),
              bsts_int64_from_int64(7))) == 1,
      "eq_Int64 true");
  assert(
      get_variant(
          ___bsts_g_Bosatsu_l_Num_l_Int64_l_eq__Int64(
              bsts_int64_from_int64(7),
              bsts_int64_from_int64(8))) == 0,
      "eq_Int64 false");

  assert(
      get_variant(
          ___bsts_g_Bosatsu_l_Num_l_Int64_l_cmp__Int64(
              bsts_int64_from_int64(-1),
              bsts_int64_from_int64(1))) == 0,
      "cmp_Int64 sorts signed values");
  assert(
      get_variant(
          ___bsts_g_Bosatsu_l_Num_l_Int64_l_cmp__Int64(
              bsts_int64_from_int64(7),
              bsts_int64_from_int64(7))) == 1,
      "cmp_Int64 eq");

  assert_option_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_float64__to__Int64(
          bsts_float64_from_double(1.5)),
      UINT64_C(0x0000000000000002),
      "float64_to_Int64 rounds ties to even");
  assert_u64_equals(
      bsts_float64_to_bits(bsts_float64_from_double(bsts_round_ties_even(0.5))),
      UINT64_C(0x0000000000000000),
      "round_ties_even rounds +0.5 to +0.0");
  assert_u64_equals(
      bsts_float64_to_bits(bsts_float64_from_double(bsts_round_ties_even(-0.5))),
      UINT64_C(0x8000000000000000),
      "round_ties_even rounds -0.5 to -0.0");
  assert_u64_equals(
      bsts_float64_to_bits(bsts_float64_from_double(bsts_round_ties_even(2.5))),
      UINT64_C(0x4000000000000000),
      "round_ties_even keeps even integer ties");
  assert_u64_equals(
      bsts_float64_to_bits(bsts_float64_from_double(bsts_round_ties_even(-1.5))),
      UINT64_C(0xc000000000000000),
      "round_ties_even handles negative half steps");
  assert_u64_equals(
      bsts_float64_to_bits(bsts_float64_from_double(bsts_round_ties_even(4503599627370496.0))),
      UINT64_C(0x4330000000000000),
      "round_ties_even leaves large integral values unchanged");
  assert_option_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_float64__to__Int64(
          bsts_float64_from_double(2.5)),
      UINT64_C(0x0000000000000002),
      "float64_to_Int64 keeps even ties");
  assert_option_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_float64__to__Int64(
          bsts_float64_from_double(-0.5)),
      UINT64_C(0x0000000000000000),
      "float64_to_Int64 handles negative ties");
  assert_option_none(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_float64__to__Int64(
          bsts_float64_from_double(NAN)),
      "float64_to_Int64 rejects NaN");
  assert_option_none(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_float64__to__Int64(
          bsts_float64_from_double(INFINITY)),
      "float64_to_Int64 rejects infinity");
  assert_option_none(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_float64__to__Int64(
          bsts_float64_from_double(ldexp(1.0, 63))),
      "float64_to_Int64 rejects 2^63");
  assert_option_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_float64__to__Int64(
          bsts_float64_from_double(-ldexp(1.0, 63))),
      UINT64_C(0x8000000000000000),
      "float64_to_Int64 accepts -2^63");
  assert(
      bsts_float64_to_double(
          ___bsts_g_Bosatsu_l_Num_l_Int64_l_int64__to__Float64(
              bsts_int64_from_int64(INT64_MIN))) == -ldexp(1.0, 63),
      "int64_to_Float64 matches Int conversion semantics");
}

void test_array_int64() {
  BValue tabulate_fn = alloc_boxed_pure_fn1(array_identity_i64_fn);
  BValue default_fn = alloc_boxed_pure_fn1(array_echo_i64_fn);
  BValue fold_index_fn = alloc_boxed_pure_fn3(array_fold_index_sum_fn);
  BValue map_index_fn = alloc_boxed_pure_fn2(array_map_index_sum_fn);
  BValue zip_add_fn = alloc_boxed_pure_fn2(array_zip_add_fn);
  BValue zip_accum_add_fn = alloc_boxed_pure_fn3(array_zip_accum_add_fn);
  BValue float_mul_fn = alloc_boxed_pure_fn2(array_float_mul_fn);

  BValue tabulated =
      ___bsts_g_Bosatsu_l_Collection_l_Array_l_tabulate__Array(
          bsts_int64_from_int64(3),
          tabulate_fn);
  uint64_t tabulated_bits[] = { 0, 1, 2 };
  assert_int64_array_bits(
      tabulated,
      tabulated_bits,
      3,
      "tabulate_Array uses visible Int64 indices");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Collection_l_Array_l_size__Array(tabulated),
      3,
      "size_Array returns Int64");

  BValue tabulated_empty =
      ___bsts_g_Bosatsu_l_Collection_l_Array_l_tabulate__Array(
          bsts_int64_from_int64(-1),
          tabulate_fn);
  assert(test_array_unbox(tabulated_empty)->len == 0, "tabulate_Array rejects negative sizes");

  BValue tabulated_oversized =
      ___bsts_g_Bosatsu_l_Collection_l_Array_l_tabulate__Array(
          bsts_int64_from_int64((int64_t)INT_MAX + 1),
          tabulate_fn);
  assert(test_array_unbox(tabulated_oversized)->len == 0, "tabulate_Array rejects oversized sizes");

  const BValue ints[] = {
    bsts_integer_from_int(0),
    bsts_integer_from_int(1),
    bsts_integer_from_int(2),
    bsts_integer_from_int(3),
    bsts_integer_from_int(4),
  };
  BValue base_ints = test_array_from_values(5, ints);
  BValue sliced =
      ___bsts_g_Bosatsu_l_Collection_l_Array_l_slice__Array(
          base_ints,
          bsts_int64_from_int64(2),
          bsts_int64_from_int64(5));

  assert_int_string(
      ___bsts_g_Bosatsu_l_Collection_l_Array_l_get__or__Array(
          sliced,
          bsts_int64_from_int64(0),
          default_fn),
      "2",
      "get_or_Array uses slice-relative visible indices");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Collection_l_Array_l_get__or__Array(
          sliced,
          bsts_int64_from_int64(9),
          default_fn),
      9,
      "get_or_Array forwards the original miss index");

  assert_int_string(
      ___bsts_g_Bosatsu_l_Collection_l_Array_l_foldl__with__index__Array(
          sliced,
          bsts_integer_from_int(0),
          fold_index_fn),
      "12",
      "foldl_with_index_Array uses visible slice indices");

  BValue mapped =
      ___bsts_g_Bosatsu_l_Collection_l_Array_l_map__with__index__Array(
          sliced,
          map_index_fn);
  const int mapped_expected[] = { 2, 4, 6 };
  assert_int_array_equals(mapped, mapped_expected, 3, "map_with_index_Array uses visible indices");

  const BValue right_ints[] = {
    bsts_integer_from_int(10),
    bsts_integer_from_int(11),
    bsts_integer_from_int(12),
  };
  BValue right_prefix = test_array_from_values(3, right_ints);
  BValue zipped =
      ___bsts_g_Bosatsu_l_Collection_l_Array_l_zip__map__Array(
          base_ints,
          right_prefix,
          zip_add_fn);
  const int zipped_expected[] = { 10, 12, 14 };
  assert_int_array_equals(zipped, zipped_expected, 3, "zip_map_Array truncates to the shorter input");

  assert_int_string(
      ___bsts_g_Bosatsu_l_Collection_l_Array_l_zip__foldl__Array(
          base_ints,
          right_prefix,
          bsts_integer_from_int(0),
          zip_accum_add_fn),
      "36",
      "zip_foldl_Array truncates to the shorter input");
  assert_int_string(
      ___bsts_g_Bosatsu_l_Collection_l_Array_l_zip__foldl__Array(
          test_array_from_values(0, NULL),
          base_ints,
          bsts_integer_from_int(99),
          zip_accum_add_fn),
      "99",
      "zip_foldl_Array keeps the initial accumulator on empty prefixes");

  const BValue neg_zero_items[] = { bsts_float64_from_double(-0.0) };
  BValue neg_zero_array = test_array_from_values(1, neg_zero_items);
  assert_u64_equals(
      bsts_float64_to_bits(
          ___bsts_g_Bosatsu_l_Collection_l_Array_l_sumf__Array(neg_zero_array)),
      UINT64_C(0x8000000000000000),
      "sumf_Array preserves negative zero");
  assert(
      bsts_float64_to_double(
          ___bsts_g_Bosatsu_l_Collection_l_Array_l_sumf__Array(
              test_array_from_values(0, NULL))) == 0.0,
      "sumf_Array returns 0.0 on empty arrays");
  assert(
      bsts_float64_to_double(
          ___bsts_g_Bosatsu_l_Collection_l_Array_l_sumsqf__Array(
              test_array_from_values(0, NULL))) == 0.0,
      "sumsqf_Array returns 0.0 on empty arrays");

  const BValue left_float_items[] = { bsts_float64_from_double(INFINITY) };
  const BValue right_float_items[] = {
    bsts_float64_from_double(1.0),
    bsts_float64_from_double(NAN),
  };
  BValue left_float_array = test_array_from_values(1, left_float_items);
  BValue right_float_array = test_array_from_values(2, right_float_items);

  assert(
      isinf(bsts_float64_to_double(
          ___bsts_g_Bosatsu_l_Collection_l_Array_l_dotf__Array(
              left_float_array,
              right_float_array))),
      "dotf_Array truncates before reading past the shorter input");
  assert(
      isinf(bsts_float64_to_double(
          ___bsts_g_Bosatsu_l_Collection_l_Array_l_zip__sumf__Array(
              left_float_array,
              right_float_array,
              float_mul_fn))),
      "zip_sumf_Array truncates before reading past the shorter input");
}

void test_prog_assoc() {
#if !defined(_WIN32) && defined(BSTS_RUNTIME_DEBUG_CHECKS)
  assert_child_aborts(call_alloc_closure_zero, "zero-capture closures must use alloc_boxed_pure_fn");
#endif

  BValue pure_fn = alloc_boxed_pure_fn1(prog_assoc_pure_fn);
  BValue raise_fn = alloc_boxed_pure_fn1(prog_assoc_raise_fn);

  BValue flat_base = ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_integer_from_int(1));
  BValue flat_assoc = ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_Prog_l_flat__map(flat_base, pure_fn),
      pure_fn);
  assert(get_variant(flat_assoc) == 2, "flat_map assoc keeps flat_map tag");
  assert(get_enum_index(flat_assoc, 0) == flat_base, "flat_map assoc keeps left-most program");

  BValue flat_arg = bsts_integer_from_int(9);
  BValue flat_composed = call_fn1(get_enum_index(flat_assoc, 1), flat_arg);
  assert(get_variant(flat_composed) == 2, "flat_map assoc composes continuation into flat_map");
  assert(get_enum_index(flat_composed, 1) == pure_fn, "flat_map assoc keeps outer continuation");
  BValue flat_left = get_enum_index(flat_composed, 0);
  assert(get_variant(flat_left) == 0, "flat_map composed left branch is pure");
  assert(get_enum_index(flat_left, 0) == flat_arg, "flat_map composed pure keeps argument");

  BValue recover_base = ___bsts_g_Bosatsu_l_Prog_l_raise__error(
      bsts_string_from_utf8_bytes_static(4, "boom"));
  BValue recover_assoc = ___bsts_g_Bosatsu_l_Prog_l_recover(
      ___bsts_g_Bosatsu_l_Prog_l_recover(recover_base, raise_fn),
      raise_fn);
  assert(get_variant(recover_assoc) == 3, "recover assoc keeps recover tag");
  assert(get_enum_index(recover_assoc, 0) == recover_base, "recover assoc keeps left-most program");

  BValue recover_arg = bsts_string_from_utf8_bytes_static(1, "e");
  BValue recover_composed = call_fn1(get_enum_index(recover_assoc, 1), recover_arg);
  assert(get_variant(recover_composed) == 3, "recover assoc composes handler into recover");
  assert(get_enum_index(recover_composed, 1) == raise_fn, "recover assoc keeps outer handler");
  BValue recover_left = get_enum_index(recover_composed, 0);
  assert(get_variant(recover_left) == 1, "recover composed left branch is raise");
  assert(get_enum_index(recover_left, 0) == recover_arg, "recover composed raise keeps error");
}

static void assert_prog_success_int(BSTS_Prog_Test_Result result, const char* expected, const char* message) {
  if (result.is_error) {
    printf("%s\nexpected successful Prog result\n", message);
    exit(1);
  }
  assert_int_string(result.value, expected, message);
}

static void assert_prog_error_string(BSTS_Prog_Test_Result result, const char* expected, const char* message) {
  if (!result.is_error) {
    printf("%s\nexpected Prog error result\n", message);
    exit(1);
  }
  assert_string_equals(result.value, expected, message);
}

static void assert_prog_error_variant(BSTS_Prog_Test_Result result, unsigned char expected, const char* message) {
  if (!result.is_error) {
    printf("%s\nexpected Prog error result\n", message);
    exit(1);
  }
  if (get_variant(result.value) != expected) {
    printf("%s\nexpected Prog error variant: %u\ngot: %u\n", message, expected, get_variant(result.value));
    exit(1);
  }
}

static BValue assert_prog_success(BSTS_Prog_Test_Result result, const char* message) {
  if (result.is_error) {
    printf("%s\nexpected successful Prog result\n", message);
    exit(1);
  }
  return result.value;
}

static void assert_prog_success_option_string(BSTS_Prog_Test_Result result, const char* expected, const char* message) {
  BValue opt = assert_prog_success(result, message);
  if (get_variant(opt) != 1) {
    printf("%s\nexpected: Some(%s)\n", message, expected);
    exit(1);
  }
  assert_string_equals(get_enum_index(opt, 0), expected, message);
}

static void assert_prog_success_option_none(BSTS_Prog_Test_Result result, const char* message) {
  BValue opt = assert_prog_success(result, message);
  if (get_variant(opt) != 0) {
    printf("%s\nexpected: None\n", message);
    exit(1);
  }
}

static void assert_uv_ok(int result, const char* message) {
  if (result != 0) {
    printf("%s: %s\n", message, uv_strerror(result));
    exit(1);
  }
}

void test_io_core_libuv_effects() {
  BValue wall_test = alloc_boxed_pure_fn1(io_core_now_wall_test_fn);
  BValue wall = assert_prog_success(
      bsts_Bosatsu_Prog_run_test(wall_test),
      "IO/Core now_wall runs through C Prog runner");
  assert(
      bsts_integer_cmp(wall, bsts_integer_from_int(0)) > 0,
      "IO/Core now_wall should return positive epoch nanoseconds");

  BValue mono_test = alloc_boxed_pure_fn1(io_core_now_mono_test_fn);
  BValue mono = assert_prog_success(
      bsts_Bosatsu_Prog_run_test(mono_test),
      "IO/Core now_mono runs through C Prog runner");
  assert(
      bsts_integer_cmp(mono, bsts_integer_from_int(0)) >= 0,
      "IO/Core now_mono should return non-negative nanoseconds");

  assert_uv_ok(
      uv_os_setenv("BOSATSU_C_RUNTIME_TEST_ENV", "bosatsu-libuv-env"),
      "uv_os_setenv present env failed");
  assert_prog_success_option_string(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_get_env_present_test_fn)),
      "bosatsu-libuv-env",
      "IO/Core get_env should read a present environment value");

  assert_uv_ok(
      uv_os_setenv("BOSATSU_C_RUNTIME_TEST_ENV_EMPTY", ""),
      "uv_os_setenv empty env failed");
  assert_prog_success_option_string(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_get_env_empty_test_fn)),
      "",
      "IO/Core get_env should preserve present empty environment values");

  char long_env_value[301];
  for (size_t idx = 0; idx < sizeof(long_env_value) - 1U; idx++) {
    long_env_value[idx] = (char)('a' + (idx % 26U));
  }
  long_env_value[sizeof(long_env_value) - 1U] = '\0';

  assert_uv_ok(
      uv_os_setenv("BOSATSU_C_RUNTIME_TEST_ENV_LONG", long_env_value),
      "uv_os_setenv long env failed");
  assert_prog_success_option_string(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_get_env_long_test_fn)),
      long_env_value,
      "IO/Core get_env should read environment values that do not fit the stack buffer");

  (void)uv_os_unsetenv("BOSATSU_C_RUNTIME_TEST_ENV_ABSENT");
  assert_prog_success_option_none(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_get_env_absent_test_fn)),
      "IO/Core get_env should return None for an absent environment value");

  (void)uv_os_unsetenv("BOSATSU_C_RUNTIME_TEST_ENV");
  (void)uv_os_unsetenv("BOSATSU_C_RUNTIME_TEST_ENV_EMPTY");
  (void)uv_os_unsetenv("BOSATSU_C_RUNTIME_TEST_ENV_LONG");

  int sleep_calls_before_zero = io_core_sleep_continuations;
  assert_prog_success_int(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_sleep_zero_test_fn)),
      "17",
      "IO/Core zero sleep should resume and run flat_map continuation");
  assert(
      io_core_sleep_continuations == sleep_calls_before_zero + 1,
      "IO/Core zero sleep should run the post-sleep continuation exactly once");

  int sleep_calls_before_positive = io_core_sleep_continuations;
  assert_prog_success_int(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_sleep_positive_test_fn)),
      "23",
      "IO/Core positive sleep should resume and run flat_map continuation");
  assert(
      io_core_sleep_continuations == sleep_calls_before_positive + 1,
      "IO/Core positive sleep should run the post-sleep continuation exactly once");

  int sleep_calls_before_repeat = io_core_sleep_continuations;
  assert_prog_success_int(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_sleep_repeat_test_fn)),
      "31",
      "IO/Core repeated sleeps should all resume in program order");
  assert(
      io_core_sleep_continuations == sleep_calls_before_repeat + 3,
      "IO/Core repeated sleeps should run each continuation exactly once");

  long pid = (long)uv_os_getpid();
  snprintf(
      io_core_file_missing_path,
      sizeof(io_core_file_missing_path),
      "/tmp/bosatsu-c-runtime-missing-%ld.txt",
      pid);
  snprintf(
      io_core_file_existing_path,
      sizeof(io_core_file_existing_path),
      "/tmp/bosatsu-c-runtime-existing-%ld.txt",
      pid);
  snprintf(
      io_core_file_close_path,
      sizeof(io_core_file_close_path),
      "/tmp/bosatsu-c-runtime-close-%ld.txt",
      pid);
  snprintf(
      io_core_file_closed_read_path,
      sizeof(io_core_file_closed_read_path),
      "/tmp/bosatsu-c-runtime-closed-read-%ld.txt",
      pid);
  snprintf(
      io_core_file_utf8_path,
      sizeof(io_core_file_utf8_path),
      "/tmp/bosatsu-c-runtime-utf8-%ld.txt",
      pid);
  snprintf(
      io_core_file_bytes_path,
      sizeof(io_core_file_bytes_path),
      "/tmp/bosatsu-c-runtime-bytes-%ld.bin",
      pid);
  snprintf(
      io_core_file_empty_path,
      sizeof(io_core_file_empty_path),
      "/tmp/bosatsu-c-runtime-empty-%ld.bin",
      pid);
  snprintf(
      io_core_file_copy_src_path,
      sizeof(io_core_file_copy_src_path),
      "/tmp/bosatsu-c-runtime-copy-src-%ld.bin",
      pid);
  snprintf(
      io_core_file_copy_dst_path,
      sizeof(io_core_file_copy_dst_path),
      "/tmp/bosatsu-c-runtime-copy-dst-%ld.bin",
      pid);
  snprintf(
      io_core_file_copy_limit_dst_path,
      sizeof(io_core_file_copy_limit_dst_path),
      "/tmp/bosatsu-c-runtime-copy-limit-dst-%ld.bin",
      pid);
  snprintf(
      io_core_file_invalid_utf8_path,
      sizeof(io_core_file_invalid_utf8_path),
      "/tmp/bosatsu-c-runtime-invalid-utf8-%ld.bin",
      pid);
  snprintf(
      io_core_file_write_only_path,
      sizeof(io_core_file_write_only_path),
      "/tmp/bosatsu-c-runtime-write-only-%ld.bin",
      pid);
  snprintf(
      io_core_file_read_only_path,
      sizeof(io_core_file_read_only_path),
      "/tmp/bosatsu-c-runtime-read-only-%ld.bin",
      pid);
  snprintf(
      io_core_dir_root_path,
      sizeof(io_core_dir_root_path),
      "/tmp/bosatsu-c-runtime-dir-root-%ld",
      pid);
  snprintf(
      io_core_dir_missing_path,
      sizeof(io_core_dir_missing_path),
      "%s/missing",
      io_core_dir_root_path);
  snprintf(
      io_core_dir_mkdir_path,
      sizeof(io_core_dir_mkdir_path),
      "%s/mkdir-leaf",
      io_core_dir_root_path);
  snprintf(
      io_core_dir_mkdir_nested_path,
      sizeof(io_core_dir_mkdir_nested_path),
      "%s/mkdir-parent/mkdir-child",
      io_core_dir_root_path);
  snprintf(
      io_core_dir_mode_path,
      sizeof(io_core_dir_mode_path),
      "%s/mode-leaf",
      io_core_dir_root_path);
  snprintf(
      io_core_dir_list_path,
      sizeof(io_core_dir_list_path),
      "%s/list-dir",
      io_core_dir_root_path);
  snprintf(
      io_core_dir_list_a_path,
      sizeof(io_core_dir_list_a_path),
      "%s/a.txt",
      io_core_dir_list_path);
  snprintf(
      io_core_dir_list_b_path,
      sizeof(io_core_dir_list_b_path),
      "%s/b.txt",
      io_core_dir_list_path);
  snprintf(
      io_core_dir_stat_file_path,
      sizeof(io_core_dir_stat_file_path),
      "%s/stat-file.txt",
      io_core_dir_root_path);
  snprintf(
      io_core_dir_stat_symlink_path,
      sizeof(io_core_dir_stat_symlink_path),
      "%s/stat-link",
      io_core_dir_root_path);
  snprintf(
      io_core_dir_remove_file_path,
      sizeof(io_core_dir_remove_file_path),
      "%s/remove-file.txt",
      io_core_dir_root_path);
  snprintf(
      io_core_dir_remove_nonempty_path,
      sizeof(io_core_dir_remove_nonempty_path),
      "%s/remove-nonempty",
      io_core_dir_root_path);
  snprintf(
      io_core_dir_remove_nonempty_child_path,
      sizeof(io_core_dir_remove_nonempty_child_path),
      "%s/child.txt",
      io_core_dir_remove_nonempty_path);
  snprintf(
      io_core_dir_remove_tree_path,
      sizeof(io_core_dir_remove_tree_path),
      "%s/remove-tree",
      io_core_dir_root_path);
  snprintf(
      io_core_dir_remove_tree_child_path,
      sizeof(io_core_dir_remove_tree_child_path),
      "%s/child",
      io_core_dir_remove_tree_path);
  snprintf(
      io_core_dir_remove_tree_file_path,
      sizeof(io_core_dir_remove_tree_file_path),
      "%s/payload.txt",
      io_core_dir_remove_tree_child_path);
  snprintf(
      io_core_dir_rename_from_path,
      sizeof(io_core_dir_rename_from_path),
      "%s/rename-from.txt",
      io_core_dir_root_path);
  snprintf(
      io_core_dir_rename_to_path,
      sizeof(io_core_dir_rename_to_path),
      "%s/rename-to.txt",
      io_core_dir_root_path);
  io_core_file_temp_created_path[0] = '\0';
  io_core_dir_temp_created_path[0] = '\0';

  io_core_test_unlink(io_core_file_missing_path);
  io_core_test_unlink(io_core_file_existing_path);
  io_core_test_unlink(io_core_file_close_path);
  io_core_test_unlink(io_core_file_closed_read_path);
  io_core_test_unlink(io_core_file_utf8_path);
  io_core_test_unlink(io_core_file_bytes_path);
  io_core_test_unlink(io_core_file_empty_path);
  io_core_test_unlink(io_core_file_copy_src_path);
  io_core_test_unlink(io_core_file_copy_dst_path);
  io_core_test_unlink(io_core_file_copy_limit_dst_path);
  io_core_test_unlink(io_core_file_invalid_utf8_path);
  io_core_test_unlink(io_core_file_write_only_path);
  io_core_test_unlink(io_core_file_read_only_path);
  io_core_test_unlink(io_core_dir_stat_symlink_path);
  io_core_test_unlink(io_core_dir_rename_to_path);
  io_core_test_unlink(io_core_dir_rename_from_path);
  io_core_test_unlink(io_core_dir_remove_tree_file_path);
  io_core_test_rmdir(io_core_dir_remove_tree_child_path);
  io_core_test_rmdir(io_core_dir_remove_tree_path);
  io_core_test_unlink(io_core_dir_remove_nonempty_child_path);
  io_core_test_rmdir(io_core_dir_remove_nonempty_path);
  io_core_test_unlink(io_core_dir_remove_file_path);
  io_core_test_unlink(io_core_dir_stat_file_path);
  io_core_test_unlink(io_core_dir_list_a_path);
  io_core_test_unlink(io_core_dir_list_b_path);
  io_core_test_rmdir(io_core_dir_list_path);
  io_core_test_rmdir(io_core_dir_mode_path);
  io_core_test_rmdir(io_core_dir_mkdir_nested_path);
  char io_core_dir_mkdir_parent_path[PATH_MAX];
  snprintf(
      io_core_dir_mkdir_parent_path,
      sizeof(io_core_dir_mkdir_parent_path),
      "%s/mkdir-parent",
      io_core_dir_root_path);
  io_core_test_rmdir(io_core_dir_mkdir_parent_path);
  io_core_test_rmdir(io_core_dir_mkdir_path);
  io_core_test_rmdir(io_core_dir_root_path);

  FILE* existing = fopen(io_core_file_existing_path, "wb");
  assert(existing != NULL, "creating existing file fixture should succeed");
  assert(fclose(existing) == 0, "closing existing file fixture should succeed");
  static const uint8_t bytes_payload[] = {0, 1, 2, 253, 254, 255};
  static const uint8_t copy_payload[] = {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'};
  static const uint8_t invalid_utf8_payload[] = {0xff, 0x61};
  io_core_write_fixture(io_core_file_empty_path, NULL, 0);
  io_core_write_fixture(io_core_file_copy_src_path, copy_payload, sizeof(copy_payload));
  io_core_write_fixture(io_core_file_invalid_utf8_path, invalid_utf8_payload, sizeof(invalid_utf8_payload));
  io_core_write_fixture(io_core_file_read_only_path, bytes_payload, sizeof(bytes_payload));
  io_core_test_mkdir(io_core_dir_root_path);
  io_core_test_mkdir(io_core_dir_list_path);
  io_core_write_fixture(io_core_dir_list_b_path, bytes_payload, sizeof(bytes_payload));
  io_core_write_fixture(io_core_dir_list_a_path, bytes_payload, sizeof(bytes_payload));
  io_core_write_fixture(io_core_dir_stat_file_path, bytes_payload, sizeof(bytes_payload));
  io_core_write_fixture(io_core_dir_remove_file_path, bytes_payload, sizeof(bytes_payload));
  io_core_test_mkdir(io_core_dir_remove_nonempty_path);
  io_core_write_fixture(io_core_dir_remove_nonempty_child_path, bytes_payload, sizeof(bytes_payload));
  io_core_test_mkdir(io_core_dir_remove_tree_path);
  io_core_test_mkdir(io_core_dir_remove_tree_child_path);
  io_core_write_fixture(io_core_dir_remove_tree_file_path, bytes_payload, sizeof(bytes_payload));
  io_core_write_fixture(io_core_dir_rename_from_path, bytes_payload, sizeof(bytes_payload));
  assert(symlink(io_core_dir_stat_file_path, io_core_dir_stat_symlink_path) == 0, "creating IO/Core symlink fixture should succeed");

  assert_prog_error_variant(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_open_missing_read_test_fn)),
      0,
      "IO/Core open_file Read should map a missing path to NotFound");
  assert_prog_error_variant(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_create_new_existing_test_fn)),
      2,
      "IO/Core open_file CreateNew should map an existing path to AlreadyExists");
  (void)assert_prog_success(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_close_twice_test_fn)),
      "IO/Core close should be idempotent for runtime-owned handles");
  assert_prog_error_variant(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_read_closed_handle_test_fn)),
      14,
      "IO/Core read on a closed runtime-owned handle should raise BadFileDescriptor");
  (void)assert_prog_success(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_write_utf8_test_fn)),
      "IO/Core write_utf8 and flush should succeed on a writable runtime-owned handle");
  uint8_t utf8_readback[8];
  io_core_read_fixture(
      io_core_file_utf8_path,
      utf8_readback,
      sizeof(utf8_readback),
      "IO/Core write_utf8 should persist exact UTF-8 bytes");
  assert(memcmp(utf8_readback, "hello-\xc2\xb5", sizeof(utf8_readback)) == 0, "IO/Core write_utf8 persisted bytes should match");
  (void)assert_prog_success(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_read_utf8_file_test_fn)),
      "IO/Core read_utf8 should read text then return EOF");
  (void)assert_prog_success(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_write_bytes_test_fn)),
      "IO/Core write_bytes and flush should succeed on a writable runtime-owned handle");
  uint8_t bytes_readback[sizeof(bytes_payload)];
  io_core_read_fixture(
      io_core_file_bytes_path,
      bytes_readback,
      sizeof(bytes_readback),
      "IO/Core write_bytes should persist exact bytes");
  assert(memcmp(bytes_readback, bytes_payload, sizeof(bytes_payload)) == 0, "IO/Core write_bytes persisted bytes should match");
  (void)assert_prog_success(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_read_bytes_file_test_fn)),
      "IO/Core read_bytes should read bounded chunks then return EOF");
  (void)assert_prog_success(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_read_all_bytes_file_test_fn)),
      "IO/Core read_all_bytes should read all bytes across chunks");
  (void)assert_prog_success(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_read_all_empty_file_test_fn)),
      "IO/Core read_all_bytes should return empty bytes for an empty file");
  (void)assert_prog_success(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_copy_bytes_test_fn)),
      "IO/Core copy_bytes should copy all bytes without a limit");
  uint8_t copy_readback[sizeof(copy_payload)];
  io_core_read_fixture(
      io_core_file_copy_dst_path,
      copy_readback,
      sizeof(copy_readback),
      "IO/Core copy_bytes should write all source bytes");
  assert(memcmp(copy_readback, copy_payload, sizeof(copy_payload)) == 0, "IO/Core copy_bytes destination should match source");
  (void)assert_prog_success(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_copy_bytes_limit_test_fn)),
      "IO/Core copy_bytes should respect a finite max_total");
  uint8_t copy_limit_readback[5];
  io_core_read_fixture(
      io_core_file_copy_limit_dst_path,
      copy_limit_readback,
      sizeof(copy_limit_readback),
      "IO/Core copy_bytes with max_total should write only the prefix");
  assert(memcmp(copy_limit_readback, copy_payload, sizeof(copy_limit_readback)) == 0, "IO/Core limited copy destination should match source prefix");
  assert_prog_error_variant(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_invalid_utf8_file_test_fn)),
      13,
      "IO/Core read_utf8 should map invalid bytes to InvalidUtf8");
  assert_prog_error_variant(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_read_from_write_only_test_fn)),
      14,
      "IO/Core read_bytes on a write-only handle should raise BadFileDescriptor");
  assert_prog_error_variant(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_write_to_read_only_test_fn)),
      14,
      "IO/Core write_bytes on a read-only handle should raise BadFileDescriptor");
  (void)assert_prog_success(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_create_temp_file_test_fn)),
      "IO/Core create_temp_file should return a writable uv_file-backed handle");
  if (io_core_file_temp_created_path[0] != '\0') {
    uint8_t temp_readback[4];
    io_core_read_fixture(
        io_core_file_temp_created_path,
        temp_readback,
        sizeof(temp_readback),
        "IO/Core temp file handle should write through to the created file");
    assert(memcmp(temp_readback, "temp", sizeof(temp_readback)) == 0, "IO/Core temp file contents should match written data");
  }
  assert_prog_success_option_none(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_stat_missing_test_fn)),
      "IO/Core stat should return None for a missing path");
  assert_prog_error_variant(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_list_missing_test_fn)),
      0,
      "IO/Core list_dir should map a missing path to NotFound");
  (void)assert_prog_success(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_create_temp_dir_test_fn)),
      "IO/Core create_temp_dir should return a created directory path");
  assert_prog_error_variant(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_create_temp_dir_invalid_prefix_test_fn)),
      12,
      "IO/Core create_temp_dir should reject invalid prefixes");
  (void)assert_prog_success(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_mkdir_test_fn)),
      "IO/Core mkdir should create a leaf directory");
  assert_prog_error_variant(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_mkdir_existing_test_fn)),
      2,
      "IO/Core mkdir should map an existing path to AlreadyExists");
  (void)assert_prog_success(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_mkdir_recursive_test_fn)),
      "IO/Core recursive mkdir should create parent directories");
  (void)assert_prog_success(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_mkdir_with_mode_test_fn)),
      "IO/Core mkdir_with_mode should create a directory with requested mode bits");
  (void)assert_prog_success(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_stat_mode_test_fn)),
      "IO/Core stat should expose mkdir_with_mode mode bits");
  (void)assert_prog_success(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_list_dir_test_fn)),
      "IO/Core list_dir should return sorted joined child paths");
  (void)assert_prog_success(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_stat_file_test_fn)),
      "IO/Core stat should classify files");
  (void)assert_prog_success(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_stat_dir_test_fn)),
      "IO/Core stat should classify directories");
  (void)assert_prog_success(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_stat_symlink_test_fn)),
      "IO/Core stat should classify symlinks");
  (void)assert_prog_success(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_remove_file_test_fn)),
      "IO/Core remove should unlink files");
  assert_prog_error_variant(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_remove_nonempty_test_fn)),
      5,
      "IO/Core non-recursive remove should reject non-empty directories");
  (void)assert_prog_success(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_remove_recursive_test_fn)),
      "IO/Core recursive remove should delete nested trees");
  assert(
      !io_core_test_path_exists(io_core_dir_remove_tree_path),
      "IO/Core recursive remove should remove the tree root");
  (void)assert_prog_success(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_rename_test_fn)),
      "IO/Core rename should move a path");
  assert(
      !io_core_test_path_exists(io_core_dir_rename_from_path) &&
          io_core_test_path_exists(io_core_dir_rename_to_path),
      "IO/Core rename should move the source to the destination");
  assert_prog_error_variant(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_rename_missing_test_fn)),
      0,
      "IO/Core rename should map a missing source to NotFound");
  assert_prog_error_variant(
      bsts_Bosatsu_Prog_run_test(alloc_boxed_pure_fn1(io_core_remove_missing_test_fn)),
      0,
      "IO/Core remove should map a missing path to NotFound");

  io_core_test_unlink(io_core_file_existing_path);
  io_core_test_unlink(io_core_file_close_path);
  io_core_test_unlink(io_core_file_closed_read_path);
  io_core_test_unlink(io_core_file_utf8_path);
  io_core_test_unlink(io_core_file_bytes_path);
  io_core_test_unlink(io_core_file_empty_path);
  io_core_test_unlink(io_core_file_copy_src_path);
  io_core_test_unlink(io_core_file_copy_dst_path);
  io_core_test_unlink(io_core_file_copy_limit_dst_path);
  io_core_test_unlink(io_core_file_invalid_utf8_path);
  io_core_test_unlink(io_core_file_write_only_path);
  io_core_test_unlink(io_core_file_read_only_path);
  if (io_core_file_temp_created_path[0] != '\0') {
    io_core_test_unlink(io_core_file_temp_created_path);
  }
  if (io_core_dir_temp_created_path[0] != '\0') {
    io_core_test_rmdir(io_core_dir_temp_created_path);
  }
  io_core_test_unlink(io_core_dir_stat_symlink_path);
  io_core_test_unlink(io_core_dir_rename_to_path);
  io_core_test_unlink(io_core_dir_remove_nonempty_child_path);
  io_core_test_rmdir(io_core_dir_remove_nonempty_path);
  io_core_test_unlink(io_core_dir_stat_file_path);
  io_core_test_unlink(io_core_dir_list_a_path);
  io_core_test_unlink(io_core_dir_list_b_path);
  io_core_test_rmdir(io_core_dir_list_path);
  io_core_test_rmdir(io_core_dir_mode_path);
  io_core_test_rmdir(io_core_dir_mkdir_nested_path);
  io_core_test_rmdir(io_core_dir_mkdir_parent_path);
  io_core_test_rmdir(io_core_dir_mkdir_path);
  io_core_test_rmdir(io_core_dir_root_path);
}

void test_prog_runner_loop() {
  BValue pure_test = alloc_boxed_pure_fn1(prog_runner_pure_test_fn);
  assert_prog_success_int(
      bsts_Bosatsu_Prog_run_test(pure_test),
      "7",
      "Prog test pure result runs through libuv loop");
  assert_prog_success_int(
      bsts_Bosatsu_Prog_run_test(pure_test),
      "7",
      "Repeated Prog test runs use independent libuv loops");

  BValue raise_test = alloc_boxed_pure_fn1(prog_runner_raise_test_fn);
  assert_prog_error_string(
      bsts_Bosatsu_Prog_run_test(raise_test),
      "boom",
      "Prog test uncaught raise runs through libuv loop");

  BValue flatmap_pure = alloc_boxed_pure_fn1(prog_runner_flatmap_after_pure_test_fn);
  assert_prog_success_int(
      bsts_Bosatsu_Prog_run_test(flatmap_pure),
      "5",
      "FlatMap after Pure preserves synchronous behavior through libuv loop");

  BValue flatmap_raise = alloc_boxed_pure_fn1(prog_runner_flatmap_after_raise_test_fn);
  assert_prog_error_string(
      bsts_Bosatsu_Prog_run_test(flatmap_raise),
      "error",
      "FlatMap after Raise skips continuations through libuv loop");

  BValue recover_raise = alloc_boxed_pure_fn1(prog_runner_recover_after_raise_test_fn);
  assert_prog_success_int(
      bsts_Bosatsu_Prog_run_test(recover_raise),
      "11",
      "Recover after Raise handles errors through libuv loop");

  BValue recover_pure = alloc_boxed_pure_fn1(prog_runner_recover_after_pure_test_fn);
  assert_prog_success_int(
      bsts_Bosatsu_Prog_run_test(recover_pure),
      "13",
      "Recover after Pure skips handlers through libuv loop");

  BValue sync_effect_success = alloc_boxed_pure_fn1(prog_runner_sync_effect_success_test_fn);
  assert_prog_success_int(
      bsts_Bosatsu_Prog_run_test(sync_effect_success),
      "42",
      "Synchronous Effect success still returns through the stepper");

  BValue sync_effect_recover = alloc_boxed_pure_fn1(prog_runner_sync_effect_recover_test_fn);
  assert_prog_success_int(
      bsts_Bosatsu_Prog_run_test(sync_effect_recover),
      "11",
      "Synchronous Effect raise still routes through recover");

  BValue async_success = alloc_boxed_pure_fn1(prog_runner_async_success_test_fn);
  int calls_before_async_success = prog_runner_async_effect_calls;
  assert_prog_success_int(
      bsts_Bosatsu_Prog_run_test(async_success),
      "41",
      "Suspended Effect success resumes captured flat_map continuation");
  assert(
      prog_runner_async_effect_calls == calls_before_async_success + 1,
      "Suspended Effect success should run one request per Prog invocation");

  BValue async_error_recover = alloc_boxed_pure_fn1(prog_runner_async_error_recover_test_fn);
  int calls_before_async_error = prog_runner_async_effect_calls;
  assert_prog_success_int(
      bsts_Bosatsu_Prog_run_test(async_error_recover),
      "11",
      "Suspended Effect error resumes through captured recover handler");
  assert(
      prog_runner_async_effect_calls == calls_before_async_error + 1,
      "Suspended Effect error should run one request per Prog invocation");

  int calls_before_async_repeat = prog_runner_async_effect_calls;
  assert_prog_success_int(
      bsts_Bosatsu_Prog_run_test(async_success),
      "41",
      "Repeated suspended Prog tests use independent pending state");
  assert(
      prog_runner_async_effect_calls == calls_before_async_repeat + 1,
      "Repeated suspended Prog test should not reuse pending state");

  BValue async_start_fail = alloc_boxed_pure_fn1(prog_runner_async_start_fail_test_fn);
  assert_prog_error_variant(
      bsts_Bosatsu_Prog_run_test(async_start_fail),
      5,
      "Suspended Effect start failure reports the original Prog, not the private Suspend tag");

#if !defined(_WIN32)
  assert_child_aborts(
      call_prog_async_double_complete,
      "Suspended Prog completion helper should reject a second completion");
  assert_child_aborts(
      call_prog_async_never_complete,
      "Prog runner should reject a suspended effect that never completes");
  assert_child_aborts(
      call_prog_async_unref,
      "Prog runner should reject unreferenced unfinished suspended work");
#endif

  BValue main_success = alloc_boxed_pure_fn1(prog_runner_main_success_fn);
  assert(
      bsts_Bosatsu_Prog_run_main(main_success, 0, NULL) == 3,
      "Prog main success returns integer exit code through libuv loop");

  assert(
      bsts_Bosatsu_Prog_run_main(raise_test, 0, NULL) == 1,
      "Prog main uncaught raise returns exit code 1 through libuv loop");
}

int main(int argc, char** argv) {

  GC_init();
  test_runtime_enum_struct();
  test_runtime_strings();
  test_integer();
  test_float64();
  test_int64();
  test_array_int64();
  test_prog_assoc();
  test_prog_runner_loop();
  test_io_core_uv_chunk_sizes();
  test_io_core_libuv_effects();
  printf("success\n");
  return 0;
}
