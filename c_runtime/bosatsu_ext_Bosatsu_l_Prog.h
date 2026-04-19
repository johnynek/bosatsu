#include "bosatsu_runtime.h"

BValue ___bsts_g_Bosatsu_l_Prog_l_apply__fix(BValue a, BValue b);

BValue ___bsts_g_Bosatsu_l_Prog_l_flat__map(BValue a, BValue b);

BValue ___bsts_g_Bosatsu_l_Prog_l_get(BValue a);

BValue ___bsts_g_Bosatsu_l_Prog_l_new__var(BValue a);

BValue ___bsts_g_Bosatsu_l_Prog_l_observe(BValue a);

BValue ___bsts_g_Bosatsu_l_Prog_l_pure(BValue a);

BValue ___bsts_g_Bosatsu_l_Prog_l_raise__error(BValue a);

BValue ___bsts_g_Bosatsu_l_Prog_l_recover(BValue a, BValue b);

BValue ___bsts_g_Bosatsu_l_Prog_l_set(BValue a, BValue b);

BValue ___bsts_g_Bosatsu_l_Prog_l_swap(BValue a, BValue b);

BValue ___bsts_g_Bosatsu_l_Prog_l_update(BValue a, BValue b);

typedef struct BSTS_Prog_Test_Result {
  _Bool is_error;
  BValue value;
} BSTS_Prog_Test_Result;

int bsts_Bosatsu_Prog_run_main(BValue main_fn, int argc, char** argv);
BSTS_Prog_Test_Result bsts_Bosatsu_Prog_run_test(BValue test_fn);
