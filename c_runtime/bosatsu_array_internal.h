#ifndef BOSATSU_ARRAY_INTERNAL_H
#define BOSATSU_ARRAY_INTERNAL_H

#include "bosatsu_runtime.h"

typedef struct {
  BValue* data;
  // TODO: widen these to a 64-bit/index-sized type when the runtime lifts the
  // current INT_MAX-backed allocation cap for Arrays.
  int offset;
  int len;
} BSTS_Array;

#endif
