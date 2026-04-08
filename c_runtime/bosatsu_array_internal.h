#ifndef BOSATSU_ARRAY_INTERNAL_H
#define BOSATSU_ARRAY_INTERNAL_H

#include "bosatsu_runtime.h"

typedef struct {
  BValue* data;
  int offset;
  int len;
} BSTS_Array;

#endif
