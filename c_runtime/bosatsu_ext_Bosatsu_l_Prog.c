#include "bosatsu_runtime.h"

#include <stdio.h>

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

BValue ___bsts_g_Bosatsu_l_Prog_l_apply__fix(BValue a, BValue f) {
  return alloc_enum2(4, a, f);
}

BValue ___bsts_g_Bosatsu_l_Prog_l_flat__map(BValue p, BValue f) {
  return alloc_enum2(2, p, f);
}

BValue bsts_println_effect(BValue a) {
  bsts_string_println(a);
  return bsts_unit_value();
}

BValue ___bsts_g_Bosatsu_l_Prog_l_println(BValue a) {
  return alloc_enum2(7, a, alloc_boxed_pure_fn1(bsts_println_effect));
}

BValue ___bsts_g_Bosatsu_l_Prog_l_pure(BValue a) {
  return alloc_enum1(0, a);
}

BValue ___bsts_g_Bosatsu_l_Prog_l_raise__error(BValue a) {
  return alloc_enum1(1, a);
}

BValue ___bsts_g_Bosatsu_l_Prog_l_read__env() {
  return alloc_enum0(5);
}

BValue ___bsts_g_Bosatsu_l_Prog_l_recover(BValue p, BValue f) {
  return alloc_enum2(3, p, f);
}

BValue ___bsts_g_Bosatsu_l_Prog_l_remap__env(BValue f, BValue p) {
  return alloc_enum2(6, f, p);
}

BValue bsts_prog_step_fix_closure(BValue* slots, BValue a) {
  return ___bsts_g_Bosatsu_l_Prog_l_apply__fix(a, slots[0]);
}

BValue bsts_prog_step_fix(BValue arg, BValue fixfn) {
  /*
  # this is just apply_fix(a, fixfn)
  fixed = lambda a: (4, a, fixfn)
  return fixfn(fixed)(arg)
  */
  BValue captures[1] = { fixfn };
  BValue fixed = alloc_closure1(1, captures, bsts_prog_step_fix_closure);
  BValue ap1 = call_fn1(fixfn, fixed);
  return call_fn1(ap1, arg);
}

int bsts_Bosatsu_Prog_run_main(BValue prog, int argc, char** argv) {
  BValue arg_list = alloc_enum0(0);
  for (int i = argc; i > 0; i--) {
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
  while (1) {
    switch (get_variant(arg)) {
      case 0: {
        // pure
        BValue item = get_enum_index(arg, 0);
        _Bool search_stack = 1;
        while (search_stack) {
          switch (get_variant(stack)) {
            case 0:
              //done, the result must be an int
              return (int)bsts_integer_to_int32(item);
            case 1: {
              //fmstep
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
      case 1: {
        // raise
        BValue error = get_enum_index(arg, 0);
        _Bool search_stack = 1;
        while (search_stack) {
          switch (get_variant(stack)) {
            case 0:
              //done, the result must be an int
              printf("unexpected top error");
              return 1;
            case 1:
              // fmstep, but we have an error
              stack = get_enum_index(stack, 2);
              break;
            case 2: {
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
      case 2: {
        // flat_map
        BValue flatmap_fn = get_enum_index(arg, 1);
        arg = get_enum_index(arg, 0);
        stack = alloc_enum2(1, flatmap_fn, stack);
        break;
      }
      case 3: {
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
      case 6: {
        // RemapEnv(f, p) => (6, f, p)
        stack = alloc_enum2(3, env, stack);
        BValue remap_fn = get_enum_index(arg, 1);
        env = call_fn1(remap_fn, env);
        arg = get_enum_index(arg, 0);
        break;
      }
      case 7: {
        // Effect(arg: BValue, f: BValue => BValue) => (7, f)
        BValue earg = get_enum_index(arg, 0);
        BValue efn = get_enum_index(arg, 1);
        BValue result = call_fn1(efn, earg);
        arg = ___bsts_g_Bosatsu_l_Prog_l_pure(result);
        break;
      }
    }
  }
  return 0;
}