#include "bosatsu_ext_Bosatsu_l_Prog.h"
#include "bosatsu_ext_Bosatsu_l_Prog_internal.h"

#include <gc.h>
#include <stdio.h>
#include <stdatomic.h>
#include <stdlib.h>
#include <uv.h>

/*
# Prog is an ADT with the following values:
# Pure(a) => (0, a)
# Raise(e) => (1, e)
# FlatMap(p, f) => (2, p, f)
# Recover(p, f) => (3, p, f)
# ApplyFix(a, f) => (4, a, f)
# Effect(arg: BValue, f: BValue => BValue) => (5, arg, f)
#
# The C runtime also reserves a private effect-result tag:
# Suspend(suspend_request, request) => (6, suspend_request, request)
# where suspend_request holds a private C callback that publishes
# exactly one completion for request. Generated Bosatsu code should not
# construct this tag directly.
*/

typedef struct {
  _Atomic BValue value;
} BSTS_Prog_Var;

typedef enum {
  BSTS_PROG_RUNTIME_RUNNING,
  BSTS_PROG_RUNTIME_SUSPENDED,
  BSTS_PROG_RUNTIME_RESUMED_SUCCESS,
  BSTS_PROG_RUNTIME_RESUMED_ERROR,
  BSTS_PROG_RUNTIME_FINISHED
} BSTS_Prog_Runtime_State;

typedef enum {
  BSTS_PROG_SUSPEND_PENDING,
  BSTS_PROG_SUSPEND_RESUMED,
  BSTS_PROG_SUSPEND_CONSUMED
} BSTS_Prog_Suspend_State;

typedef struct BSTS_Prog_Runtime BSTS_Prog_Runtime;

struct BSTS_Prog_Suspended {
  BSTS_Prog_Runtime *runtime;
  struct BSTS_Prog_Suspended *next;
  struct BSTS_Prog_Suspended *prev;
  BValue effect_arg;
  BValue stack;
  BValue request;
  BValue result;
  _Bool is_error;
  BSTS_Prog_Suspend_State state;
};

typedef struct {
  BValue request;
  BSTS_Prog_Suspend_Start start;
} BSTS_Prog_Suspend_Request;

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

static BValue bsts_prog_flat_map_assoc_closure(BValue *slots, BValue a)
{
  return ___bsts_g_Bosatsu_l_Prog_l_flat__map(call_fn1(slots[0], a), slots[1]);
}

BValue ___bsts_g_Bosatsu_l_Prog_l_flat__map(BValue p, BValue f)
{
  if (get_variant(p) == 2)
  {
    BValue captures[2] = {get_enum_index(p, 1), f};
    BValue combined = alloc_closure1(2, captures, bsts_prog_flat_map_assoc_closure);
    return alloc_enum2(2, get_enum_index(p, 0), combined);
  }
  return alloc_enum2(2, p, f);
}

static BValue bsts_prog_recover_assoc_closure(BValue *slots, BValue a)
{
  return ___bsts_g_Bosatsu_l_Prog_l_recover(call_fn1(slots[0], a), slots[1]);
}

BValue ___bsts_g_Bosatsu_l_Prog_l_recover(BValue p, BValue f)
{
  if (get_variant(p) == 3)
  {
    BValue captures[2] = {get_enum_index(p, 1), f};
    BValue combined = alloc_closure1(2, captures, bsts_prog_recover_assoc_closure);
    return alloc_enum2(3, get_enum_index(p, 0), combined);
  }
  return alloc_enum2(3, p, f);
}

static BValue bsts_prog_effect1(BValue a, BValue (*fn)(BValue))
{
  return alloc_enum2(5, a, alloc_boxed_pure_fn1(fn));
}

static BValue bsts_prog_effect2(BValue a, BValue b, BValue (*fn)(BValue))
{
  return alloc_enum2(5, alloc_struct2(a, b), alloc_boxed_pure_fn1(fn));
}

BValue bsts_Bosatsu_Prog_suspend(BValue request, BSTS_Prog_Suspend_Start start)
{
  BSTS_Prog_Suspend_Request *suspend_request =
      (BSTS_Prog_Suspend_Request *)GC_malloc(sizeof(BSTS_Prog_Suspend_Request));
  if (suspend_request == NULL)
  {
    perror("GC_malloc failure in bsts_Bosatsu_Prog_suspend");
    abort();
  }

  suspend_request->request = request;
  suspend_request->start = start;
  return alloc_enum2(6, BSTS_VALUE_FROM_PTR(suspend_request), request);
}

static BSTS_Prog_Var *bsts_prog_unbox_var(BValue var_value)
{
  return BSTS_PTR(BSTS_Prog_Var, var_value);
}

static BValue bsts_prog_new_var_effect(BValue initial)
{
  BSTS_Prog_Var *cell = (BSTS_Prog_Var *)GC_malloc(sizeof(BSTS_Prog_Var));
  if (cell == NULL)
  {
    perror("GC_malloc failure in bsts_prog_new_var_effect");
    abort();
  }

  atomic_init(&cell->value, initial);
  return ___bsts_g_Bosatsu_l_Prog_l_pure(BSTS_VALUE_FROM_PTR(cell));
}

static BValue bsts_prog_var_get_effect(BValue var_value)
{
  BSTS_Prog_Var *cell = bsts_prog_unbox_var(var_value);
  BValue current = atomic_load_explicit(&cell->value, memory_order_acquire);
  return ___bsts_g_Bosatsu_l_Prog_l_pure(current);
}

static BValue bsts_prog_var_set_effect(BValue pair)
{
  BValue var_value = get_struct_index(pair, 0);
  BValue next_value = get_struct_index(pair, 1);
  BSTS_Prog_Var *cell = bsts_prog_unbox_var(var_value);
  atomic_store_explicit(&cell->value, next_value, memory_order_release);
  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_unit_value());
}

static BValue bsts_prog_var_swap_effect(BValue pair)
{
  BValue var_value = get_struct_index(pair, 0);
  BValue next_value = get_struct_index(pair, 1);
  BSTS_Prog_Var *cell = bsts_prog_unbox_var(var_value);
  BValue current = atomic_exchange_explicit(&cell->value, next_value, memory_order_acq_rel);
  return ___bsts_g_Bosatsu_l_Prog_l_pure(current);
}

static BValue bsts_prog_var_update_effect(BValue pair)
{
  BValue var_value = get_struct_index(pair, 0);
  BValue fn = get_struct_index(pair, 1);
  BSTS_Prog_Var *cell = bsts_prog_unbox_var(var_value);

  while (1)
  {
    BValue current = atomic_load_explicit(&cell->value, memory_order_acquire);
    BValue update_result = call_fn1(fn, current);
    BValue next_value = get_struct_index(update_result, 0);
    BValue result = get_struct_index(update_result, 1);
    BValue expected = current;
    // Weak CAS may retry even without contention, so fn may be re-run.
    if (atomic_compare_exchange_weak_explicit(
            &cell->value,
            &expected,
            next_value,
            memory_order_acq_rel,
            memory_order_acquire))
    {
      return ___bsts_g_Bosatsu_l_Prog_l_pure(result);
    }
  }
}

BValue ___bsts_g_Bosatsu_l_Prog_l_new__var(BValue a)
{
  return bsts_prog_effect1(a, bsts_prog_new_var_effect);
}

BValue ___bsts_g_Bosatsu_l_Prog_l_get(BValue a)
{
  return bsts_prog_effect1(a, bsts_prog_var_get_effect);
}

BValue ___bsts_g_Bosatsu_l_Prog_l_set(BValue a, BValue b)
{
  return bsts_prog_effect2(a, b, bsts_prog_var_set_effect);
}

BValue ___bsts_g_Bosatsu_l_Prog_l_swap(BValue a, BValue b)
{
  return bsts_prog_effect2(a, b, bsts_prog_var_swap_effect);
}

BValue ___bsts_g_Bosatsu_l_Prog_l_update(BValue a, BValue b)
{
  return bsts_prog_effect2(a, b, bsts_prog_var_update_effect);
}

static volatile BValue bsts_prog_observe_sink = (BValue)0;

static BValue bsts_prog_observe_effect(BValue arg)
{
  bsts_prog_observe_sink = arg;
  bsts_prog_observe_sink = bsts_unit_value();
  return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_unit_value());
}

BValue ___bsts_g_Bosatsu_l_Prog_l_observe(BValue a)
{
  return bsts_prog_effect1(a, bsts_prog_observe_effect);
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

static BSTS_Prog_Test_Result bsts_prog_result(_Bool is_error, BValue value)
{
  BSTS_Prog_Test_Result result = { is_error, value };
  return result;
}

struct BSTS_Prog_Runtime {
  uv_loop_t loop;
  uv_idle_t start_handle;
  BValue root_prog;
  BValue arg;
  BValue stack;
  BSTS_Prog_Test_Result result;
  BSTS_Prog_Suspended *pending_head;
  BSTS_Prog_Suspended *resumed;
  BSTS_Prog_Runtime_State state;
  int runtime_status;
};

static void bsts_prog_close_handle(uv_handle_t *handle, void *arg)
{
  (void)arg;
  if (!uv_is_closing(handle))
  {
    uv_close(handle, NULL);
  }
}

static void bsts_prog_close_loop(BSTS_Prog_Runtime *runtime)
{
  int close_result = uv_loop_close(&runtime->loop);
  while (close_result == UV_EBUSY)
  {
    uv_walk(&runtime->loop, bsts_prog_close_handle, NULL);
    int run_result = uv_run(&runtime->loop, UV_RUN_DEFAULT);
    if (run_result != 0)
    {
      fprintf(stderr, "bosatsu Prog execution fault: uv_run during loop close returned %d\n", run_result);
      break;
    }
    close_result = uv_loop_close(&runtime->loop);
  }

  if (close_result != 0)
  {
    fprintf(stderr, "bosatsu Prog execution fault: uv_loop_close failed: %s\n", uv_strerror(close_result));
    abort();
  }
}

static void bsts_prog_runtime_check_finished(BSTS_Prog_Runtime *runtime)
{
  if (runtime->state != BSTS_PROG_RUNTIME_FINISHED ||
      runtime->pending_head != NULL ||
      runtime->resumed != NULL)
  {
    fprintf(stderr, "bosatsu Prog execution fault: suspended effect did not complete before runner returned\n");
    abort();
  }
}

static void bsts_prog_runtime_complete(BSTS_Prog_Runtime *runtime, _Bool is_error, BValue value)
{
  runtime->result = bsts_prog_result(is_error, value);
  runtime->state = BSTS_PROG_RUNTIME_FINISHED;
}

static void bsts_prog_pending_insert(BSTS_Prog_Runtime *runtime, BSTS_Prog_Suspended *suspended)
{
  suspended->prev = NULL;
  suspended->next = runtime->pending_head;
  if (runtime->pending_head != NULL)
  {
    runtime->pending_head->prev = suspended;
  }
  runtime->pending_head = suspended;
}

static void bsts_prog_pending_remove(BSTS_Prog_Runtime *runtime, BSTS_Prog_Suspended *suspended)
{
  if (suspended->prev != NULL)
  {
    suspended->prev->next = suspended->next;
  }
  else if (runtime->pending_head == suspended)
  {
    runtime->pending_head = suspended->next;
  }
  else
  {
    return;
  }

  if (suspended->next != NULL)
  {
    suspended->next->prev = suspended->prev;
  }

  suspended->prev = NULL;
  suspended->next = NULL;
}

static void bsts_prog_runtime_step(BSTS_Prog_Runtime *runtime);

BValue bsts_Bosatsu_Prog_suspended_request(BSTS_Prog_Suspended *suspended)
{
  return suspended->request;
}

uv_loop_t *bsts_Bosatsu_Prog_suspended_loop(BSTS_Prog_Suspended *suspended)
{
  return &suspended->runtime->loop;
}

static void bsts_prog_suspended_complete(BSTS_Prog_Suspended *suspended, _Bool is_error, BValue value)
{
  BSTS_Prog_Runtime *runtime = suspended->runtime;

  if (suspended->state != BSTS_PROG_SUSPEND_PENDING)
  {
    fprintf(stderr, "bosatsu Prog execution fault: suspended effect resumed more than once\n");
    abort();
  }
  if (runtime->state != BSTS_PROG_RUNTIME_SUSPENDED)
  {
    fprintf(stderr, "bosatsu Prog execution fault: suspended effect resumed while runtime was not suspended\n");
    abort();
  }

  suspended->state = BSTS_PROG_SUSPEND_RESUMED;
  suspended->result = value;
  suspended->is_error = is_error;
  runtime->resumed = suspended;
  runtime->state = is_error ? BSTS_PROG_RUNTIME_RESUMED_ERROR : BSTS_PROG_RUNTIME_RESUMED_SUCCESS;
  bsts_prog_runtime_step(runtime);
}

void bsts_Bosatsu_Prog_suspended_success(BSTS_Prog_Suspended *suspended, BValue value)
{
  bsts_prog_suspended_complete(suspended, 0, value);
}

void bsts_Bosatsu_Prog_suspended_error(BSTS_Prog_Suspended *suspended, BValue error)
{
  bsts_prog_suspended_complete(suspended, 1, error);
}

static void bsts_prog_runtime_consume_resume(BSTS_Prog_Runtime *runtime)
{
  BSTS_Prog_Suspended *suspended = runtime->resumed;
  if (suspended == NULL || suspended->state != BSTS_PROG_SUSPEND_RESUMED)
  {
    fprintf(stderr, "bosatsu Prog execution fault: missing suspended effect completion\n");
    abort();
  }

  if (runtime->state == BSTS_PROG_RUNTIME_RESUMED_SUCCESS)
  {
    runtime->arg = ___bsts_g_Bosatsu_l_Prog_l_pure(suspended->result);
  }
  else if (runtime->state == BSTS_PROG_RUNTIME_RESUMED_ERROR)
  {
    runtime->arg = ___bsts_g_Bosatsu_l_Prog_l_raise__error(suspended->result);
  }
  else
  {
    fprintf(stderr, "bosatsu Prog execution fault: invalid resume state\n");
    abort();
  }

  runtime->stack = suspended->stack;
  runtime->resumed = NULL;
  suspended->state = BSTS_PROG_SUSPEND_CONSUMED;
  bsts_prog_pending_remove(runtime, suspended);
  runtime->state = BSTS_PROG_RUNTIME_RUNNING;
}

static void bsts_prog_runtime_suspend(BSTS_Prog_Runtime *runtime, BValue effect_arg, BValue request)
{
  BSTS_Prog_Suspend_Request *suspend_request =
      BSTS_PTR(BSTS_Prog_Suspend_Request, get_enum_index(request, 0));
  BSTS_Prog_Suspended *suspended = (BSTS_Prog_Suspended *)GC_malloc(sizeof(BSTS_Prog_Suspended));
  if (suspended == NULL)
  {
    perror("GC_malloc failure in bsts_prog_runtime_suspend");
    abort();
  }

  suspended->runtime = runtime;
  suspended->next = NULL;
  suspended->prev = NULL;
  suspended->effect_arg = effect_arg;
  suspended->stack = runtime->stack;
  suspended->request = suspend_request->request;
  suspended->result = bsts_unit_value();
  suspended->is_error = 0;
  suspended->state = BSTS_PROG_SUSPEND_PENDING;
  bsts_prog_pending_insert(runtime, suspended);
  runtime->state = BSTS_PROG_RUNTIME_SUSPENDED;

  int start_result = suspend_request->start(suspended);
  if (start_result != 0)
  {
    bsts_prog_pending_remove(runtime, suspended);
    suspended->state = BSTS_PROG_SUSPEND_CONSUMED;
    bsts_prog_runtime_complete(runtime, 1, runtime->root_prog);
    runtime->runtime_status = start_result;
    return;
  }
}

static void bsts_prog_runtime_step(BSTS_Prog_Runtime *runtime)
{
  /*
  # the stack ADT:
  done = (0,)
  def fmstep(fn, stack): return (1, fn, stack)
  def recstep(fn, stack): return (2, fn, stack)
  */
  if (runtime->state == BSTS_PROG_RUNTIME_RESUMED_SUCCESS ||
      runtime->state == BSTS_PROG_RUNTIME_RESUMED_ERROR)
  {
    bsts_prog_runtime_consume_resume(runtime);
  }

  while (runtime->state == BSTS_PROG_RUNTIME_RUNNING)
  {
    switch (get_variant(runtime->arg))
    {
    case 0:
    {
      // pure
      BValue item = get_enum_index(runtime->arg, 0);
      _Bool search_stack = 1;
      while (search_stack)
      {
        switch (get_variant(runtime->stack))
        {
        case 0:
          // done, return the successful value.
          bsts_prog_runtime_complete(runtime, 0, item);
          search_stack = 0;
          break;
        case 1:
        {
          // fmstep
          BValue fn = get_enum_index(runtime->stack, 0);
          runtime->stack = get_enum_index(runtime->stack, 1);
          runtime->arg = call_fn1(fn, item);
          search_stack = 0;
          break;
        }
        case 2:
          // recstep, but this isn't an error
          runtime->stack = get_enum_index(runtime->stack, 1);
          break;
        }
      }
      break;
    }
    case 1:
    {
      // raise
      BValue error = get_enum_index(runtime->arg, 0);
      _Bool search_stack = 1;
      while (search_stack)
      {
        switch (get_variant(runtime->stack))
        {
        case 0:
          // done, this is an uncaught top-level error.
          bsts_prog_runtime_complete(runtime, 1, error);
          search_stack = 0;
          break;
        case 1:
          // fmstep, but we have an error
          runtime->stack = get_enum_index(runtime->stack, 1);
          break;
        case 2:
        {
          // recstep which will handle this error
          BValue fn = get_enum_index(runtime->stack, 0);
          runtime->stack = get_enum_index(runtime->stack, 1);
          runtime->arg = call_fn1(fn, error);
          search_stack = 0;
          break;
        }
        }
      }
      break;
    }
    case 2:
    {
      // flat_map
      BValue flatmap_fn = get_enum_index(runtime->arg, 1);
      runtime->arg = get_enum_index(runtime->arg, 0);
      runtime->stack = alloc_enum2(1, flatmap_fn, runtime->stack);
      break;
    }
    case 3:
    {
      // push recover onto stack
      BValue recover_fn = get_enum_index(runtime->arg, 1);
      runtime->arg = get_enum_index(runtime->arg, 0);
      runtime->stack = alloc_enum2(2, recover_fn, runtime->stack);
      break;
    }
    case 4:
      // apply_fix
      runtime->arg = bsts_prog_step_fix(
          get_enum_index(runtime->arg, 0),
          get_enum_index(runtime->arg, 1));
      break;
    case 5:
    {
      // Effect(arg: BValue, f: BValue => BValue) => (5, arg, f)
      BValue earg = get_enum_index(runtime->arg, 0);
      BValue efn = get_enum_index(runtime->arg, 1);
      BValue effect_result = call_fn1(efn, earg);
      if (get_variant(effect_result) == 6)
      {
        bsts_prog_runtime_suspend(runtime, earg, effect_result);
      }
      else
      {
        runtime->arg = effect_result;
      }
      break;
    }
    default:
      fprintf(stderr, "bosatsu Prog execution fault: invalid Prog tag: %u\n", get_variant(runtime->arg));
      bsts_prog_runtime_complete(runtime, 1, runtime->arg);
      break;
    }
  }
}

static void bsts_prog_start_close_cb(uv_handle_t *handle)
{
  (void)handle;
}

static void bsts_prog_start_cb(uv_idle_t *handle)
{
  BSTS_Prog_Runtime *runtime = (BSTS_Prog_Runtime *)handle->data;
  uv_idle_stop(handle);
  bsts_prog_runtime_step(runtime);
  uv_close((uv_handle_t *)handle, bsts_prog_start_close_cb);
}

static BSTS_Prog_Test_Result bsts_Bosatsu_Prog_run(BValue prog)
{
  BSTS_Prog_Runtime runtime = {
    .root_prog = prog,
    .arg = prog,
    .stack = alloc_enum0(0),
    .result = bsts_prog_result(1, prog),
    .pending_head = NULL,
    .resumed = NULL,
    .state = BSTS_PROG_RUNTIME_RUNNING,
    .runtime_status = 0,
  };

  int init_result = uv_loop_init(&runtime.loop);
  if (init_result != 0)
  {
    fprintf(stderr, "bosatsu Prog execution fault: uv_loop_init failed: %s\n", uv_strerror(init_result));
    return bsts_prog_result(1, prog);
  }

  runtime.runtime_status = uv_idle_init(&runtime.loop, &runtime.start_handle);
  if (runtime.runtime_status == 0)
  {
    runtime.start_handle.data = &runtime;
    runtime.runtime_status = uv_idle_start(&runtime.start_handle, bsts_prog_start_cb);
  }

  if (runtime.runtime_status == 0)
  {
    runtime.runtime_status = uv_run(&runtime.loop, UV_RUN_DEFAULT);
  }
  else
  {
    fprintf(stderr, "bosatsu Prog execution fault: failed to start libuv runner: %s\n", uv_strerror(runtime.runtime_status));
  }

  if (runtime.runtime_status == 0)
  {
    bsts_prog_runtime_check_finished(&runtime);
  }

  bsts_prog_close_loop(&runtime);

  if (runtime.runtime_status != 0)
  {
    return bsts_prog_result(1, prog);
  }
  return runtime.result;
}

int bsts_Bosatsu_Prog_run_main(BValue main_fn, int argc, char **argv)
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

  BSTS_Prog_Test_Result result = bsts_Bosatsu_Prog_run(call_fn1(main_fn, arg_list));
  if (result.is_error)
  {
    printf("unexpected top error");
    return 1;
  }
  return (int)bsts_integer_to_int32(result.value);
}

BSTS_Prog_Test_Result bsts_Bosatsu_Prog_run_test(BValue test_fn)
{
  BValue arg_list = alloc_enum0(0);
  return bsts_Bosatsu_Prog_run(call_fn1(test_fn, arg_list));
}
