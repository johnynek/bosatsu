#include "bosatsu_ext_Bosatsu_l_Eval.h"

#include "bosatsu_ext_Bosatsu_l_Lazy.h"

#include <stdio.h>
#include <stdlib.h>

static const ENUM_TAG BSTS_EVAL_LEAF_DONE = 0;
static const ENUM_TAG BSTS_EVAL_LEAF_LAZY_LEAF = 1;
static const ENUM_TAG BSTS_EVAL_LEAF_ALWAYS = 2;

static const ENUM_TAG BSTS_EVAL_PURE = 0;
static const ENUM_TAG BSTS_EVAL_FLAT_MAP = 1;

static const ENUM_TAG BSTS_EVAL_STACK_LAST = 0;
static const ENUM_TAG BSTS_EVAL_STACK_MORE = 1;

static const ENUM_TAG BSTS_EVAL_LOOP_RUN_STACK = 0;
static const ENUM_TAG BSTS_EVAL_LOOP_RUN_EVAL = 1;

static void bsts_eval_invalid_tag(const char* name, ENUM_TAG tag) {
  fprintf(stderr, "bosatsu Eval execution fault: invalid %s tag: %u\n", name, tag);
  abort();
}

static BValue bsts_eval_force_leaf(BValue leaf) {
  switch (get_variant(leaf)) {
  case BSTS_EVAL_LEAF_DONE:
    return get_enum_index(leaf, 0);
  case BSTS_EVAL_LEAF_LAZY_LEAF:
    return ___bsts_g_Bosatsu_l_Lazy_l_get__Lazy(get_enum_index(leaf, 0));
  case BSTS_EVAL_LEAF_ALWAYS:
    return call_fn1(get_enum_index(leaf, 0), bsts_unit_value());
  default:
    bsts_eval_invalid_tag("Eval.Leaf", get_variant(leaf));
    return leaf;
  }
}

BValue ___bsts_g_Bosatsu_l_Eval_l_eval__loop(BValue loop) {
  _Bool run_eval = 0;
  BValue current = bsts_unit_value();
  BValue stack = bsts_unit_value();

  switch (get_variant(loop)) {
  case BSTS_EVAL_LOOP_RUN_STACK:
    run_eval = 0;
    current = get_enum_index(loop, 0);
    stack = get_enum_index(loop, 1);
    break;
  case BSTS_EVAL_LOOP_RUN_EVAL:
    run_eval = 1;
    current = get_enum_index(loop, 0);
    stack = get_enum_index(loop, 1);
    break;
  default:
    bsts_eval_invalid_tag("Eval.Loop", get_variant(loop));
  }

  while (1) {
    if (run_eval) {
      BValue eval = current;
      switch (get_variant(eval)) {
      case BSTS_EVAL_PURE:
        current = bsts_eval_force_leaf(get_enum_index(eval, 0));
        run_eval = 0;
        break;
      case BSTS_EVAL_FLAT_MAP: {
        BValue prev = get_enum_index(eval, 0);
        BValue fn = get_enum_index(eval, 1);
        current = prev;
        stack = alloc_enum2(BSTS_EVAL_STACK_MORE, fn, stack);
        run_eval = 1;
        break;
      }
      default:
        bsts_eval_invalid_tag("Eval", get_variant(eval));
      }
    } else {
      switch (get_variant(stack)) {
      case BSTS_EVAL_STACK_LAST: {
        BValue fn = get_enum_index(stack, 0);
        BValue next_eval = call_fn1(fn, current);
        switch (get_variant(next_eval)) {
        case BSTS_EVAL_PURE:
          return bsts_eval_force_leaf(get_enum_index(next_eval, 0));
        case BSTS_EVAL_FLAT_MAP: {
          current = get_enum_index(next_eval, 0);
          stack = alloc_enum1(BSTS_EVAL_STACK_LAST, get_enum_index(next_eval, 1));
          run_eval = 1;
          break;
        }
        default:
          bsts_eval_invalid_tag("Eval", get_variant(next_eval));
        }
        break;
      }
      case BSTS_EVAL_STACK_MORE: {
        BValue first = get_enum_index(stack, 0);
        stack = get_enum_index(stack, 1);
        current = call_fn1(first, current);
        run_eval = 1;
        break;
      }
      default:
        bsts_eval_invalid_tag("Eval.Stack", get_variant(stack));
      }
    }
  }

  return current;
}
