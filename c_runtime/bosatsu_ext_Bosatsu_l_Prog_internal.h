#ifndef BOSATSU_EXT_BOSATSU_L_PROG_INTERNAL_H
#define BOSATSU_EXT_BOSATSU_L_PROG_INTERNAL_H

#include "bosatsu_runtime.h"

#include <uv.h>

typedef struct BSTS_Prog_Suspended BSTS_Prog_Suspended;

typedef int (*BSTS_Prog_Suspend_Start)(BSTS_Prog_Suspended *suspended);

BValue bsts_Bosatsu_Prog_suspend(BValue request, BSTS_Prog_Suspend_Start start);
BValue bsts_Bosatsu_Prog_suspended_request(BSTS_Prog_Suspended *suspended);
uv_loop_t *bsts_Bosatsu_Prog_suspended_loop(BSTS_Prog_Suspended *suspended);
void bsts_Bosatsu_Prog_suspended_success(BSTS_Prog_Suspended *suspended, BValue value);
void bsts_Bosatsu_Prog_suspended_error(BSTS_Prog_Suspended *suspended, BValue error);

#endif
