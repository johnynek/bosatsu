#include "bosatsu_ext_Bosatsu_l_Prog.h"

#include <gc.h>
#include <stdio.h>
#include <stdatomic.h>
#include <stdlib.h>

/*
# Prog is an ADT with the following values:
# Pure(a) => (0, a)
# Raise(e) => (1, e)
# FlatMap(p, f) => (2, p, f)
# Recover(p, f) => (3, p, f)
# ApplyFix(a, f) => (4, a, f)
# Effect(arg: BValue, f: BValue => BValue) => (5, arg, f)
*/

typedef struct {
  _Atomic BValue value;
} BSTS_Prog_Var;

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
  return ___bsts_g_Bosatsu_l_Prog_l_pure(next_value);
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

static BSTS_Prog_Test_Result bsts_Bosatsu_Prog_run(BValue prog)
{
  /*
  # the stack ADT:
  done = (0,)
  def fmstep(fn, stack): return (1, fn, stack)
  def recstep(fn, stack): return (2, fn, stack)
  */
  BValue stack = alloc_enum0(0);
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
          // done, return the successful value.
          return bsts_prog_result(0, item);
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
          // done, this is an uncaught top-level error.
          return bsts_prog_result(1, error);
        case 1:
          // fmstep, but we have an error
          stack = get_enum_index(stack, 1);
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
    {
      // Effect(arg: BValue, f: BValue => BValue) => (5, arg, f)
      BValue earg = get_enum_index(arg, 0);
      BValue efn = get_enum_index(arg, 1);
      arg = call_fn1(efn, earg);
      break;
    }
    default:
      fprintf(stderr, "bosatsu Prog execution fault: invalid Prog tag: %u\n", get_variant(arg));
      return bsts_prog_result(1, arg);
    }
  }
  return bsts_prog_result(1, arg);
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
