#include "bosatsu_runtime.h"

/*
# Prog is an ADT with the following values:
# Pure(a) => (0, a)
# Raise(e) => (1, e)
# FlatMap(p, f) => (2, p, f)
# Recover(p, f) => (3, p, f)
# ApplyFix(a, f) => (4, a, f)
# ReadEnv() => (5, )
# RemapEnv(f, p) => (6, f, p)
*/

BValue ___bsts_g_Bosatsu_l_Prog_l_apply__fix(BValue a, BValue f) {
  return alloc_enum2(4, a, f);
}

BValue ___bsts_g_Bosatsu_l_Prog_l_flat__map(BValue p, BValue f) {
  return alloc_enum2(2, p, f);
}

BValue ___bsts_g_Bosatsu_l_Prog_l_get__args() {
  // TODO
  return (BValue)1;
}

BValue ___bsts_g_Bosatsu_l_Prog_l_println(BValue a) {
  // TODO
  return (BValue)1;
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