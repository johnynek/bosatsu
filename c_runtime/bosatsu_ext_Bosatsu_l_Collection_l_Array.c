#include "bosatsu_runtime.h"

#include <gc.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
  BValue* data;
  int offset;
  int len;
} BSTS_Array;

static void bsts_array_free(void* ptr) {
  (void)ptr;
}

static BSTS_Array* bsts_array_unbox(BValue array) {
  return (BSTS_Array*)get_external(array);
}

static BValue* bsts_array_alloc_data(int len) {
  if (len <= 0) {
    return NULL;
  }

  BValue* data = (BValue*)GC_malloc(sizeof(BValue) * (size_t)len);
  if (data == NULL) {
    perror("GC_malloc failure in bsts_array_alloc_data");
    abort();
  }
  return data;
}

static BValue bsts_array_wrap(BValue* data, int offset, int len) {
  BSTS_Array* arr = (BSTS_Array*)GC_malloc(sizeof(BSTS_Array));
  if (arr == NULL) {
    perror("GC_malloc failure in bsts_array_wrap");
    abort();
  }

  arr->data = data;
  arr->offset = offset;
  arr->len = len;
  return alloc_external(arr, bsts_array_free);
}

static BValue bsts_array_empty(void) {
  return bsts_array_wrap(NULL, 0, 0);
}

static _Bool bsts_array_index_in_range(BValue index, int len) {
  BValue zero = bsts_integer_from_int(0);
  BValue len_value = bsts_integer_from_int(len);

  return (bsts_integer_cmp(index, zero) >= 0) &&
         (bsts_integer_cmp(index, len_value) < 0);
}

static int bsts_array_cmp(BValue cmp_fn, BValue left, BValue right) {
  BValue cmp = call_fn2(cmp_fn, left, right);
  ENUM_TAG tag = get_variant(cmp);

  if (tag == 0) {
    return -1;
  }
  if (tag == 2) {
    return 1;
  }
  return 0;
}

static int bsts_min_int(int left, int right) {
  return left < right ? left : right;
}

static void bsts_array_merge_pass(
    BValue* src,
    BValue* dst,
    int len,
    int width,
    BValue cmp_fn) {
  int step = width * 2;

  for (int start = 0; start < len; start += step) {
    int mid = bsts_min_int(start + width, len);
    int end = bsts_min_int(start + step, len);

    int i = start;
    int j = mid;
    int out = start;

    while ((i < mid) && (j < end)) {
      if (bsts_array_cmp(cmp_fn, src[i], src[j]) <= 0) {
        dst[out++] = src[i++];
      }
      else {
        dst[out++] = src[j++];
      }
    }

    while (i < mid) {
      dst[out++] = src[i++];
    }
    while (j < end) {
      dst[out++] = src[j++];
    }
  }
}

static void bsts_array_stable_sort(BValue* data, int len, BValue cmp_fn) {
  if (len < 2) {
    return;
  }

  BValue* tmp = bsts_array_alloc_data(len);
  BValue* src = data;
  BValue* dst = tmp;

  for (int width = 1; width < len;) {
    bsts_array_merge_pass(src, dst, len, width, cmp_fn);

    BValue* old_src = src;
    src = dst;
    dst = old_src;

    if (width > (len / 2)) {
      width = len;
    }
    else {
      width = width * 2;
    }
  }

  if (src != data) {
    memcpy(data, src, sizeof(BValue) * (size_t)len);
  }
}

BValue ___bsts_g_Bosatsu_l_Collection_l_Array_l_empty__Array() {
  return bsts_array_empty();
}

BValue ___bsts_g_Bosatsu_l_Collection_l_Array_l_tabulate__Array(BValue n, BValue fn) {
  BValue zero = bsts_integer_from_int(0);
  if (bsts_integer_cmp(n, zero) <= 0) {
    return bsts_array_empty();
  }

  BValue max_int = bsts_integer_from_int(INT_MAX);
  if (bsts_integer_cmp(n, max_int) > 0) {
    return bsts_array_empty();
  }

  int len = (int)bsts_integer_to_int32(n);
  if (len <= 0) {
    return bsts_array_empty();
  }

  BValue* data = bsts_array_alloc_data(len);
  for (int idx = 0; idx < len; idx++) {
    data[idx] = call_fn1(fn, bsts_integer_from_int(idx));
  }

  return bsts_array_wrap(data, 0, len);
}

BValue ___bsts_g_Bosatsu_l_Collection_l_Array_l_from__List__Array(BValue items) {
  int count = 0;
  BValue current = items;

  while (get_variant(current) != 0) {
    count += 1;
    current = get_enum_index(current, 1);
  }

  if (count <= 0) {
    return bsts_array_empty();
  }

  BValue* data = bsts_array_alloc_data(count);
  current = items;

  int idx = 0;
  while (get_variant(current) != 0) {
    data[idx] = get_enum_index(current, 0);
    idx += 1;
    current = get_enum_index(current, 1);
  }

  return bsts_array_wrap(data, 0, count);
}

BValue ___bsts_g_Bosatsu_l_Collection_l_Array_l_to__List__Array(BValue array) {
  BSTS_Array* arr = bsts_array_unbox(array);

  BValue result = alloc_enum0(0);
  for (int idx = arr->len - 1; idx >= 0; idx--) {
    result = alloc_enum2(1, arr->data[arr->offset + idx], result);
  }

  return result;
}

BValue ___bsts_g_Bosatsu_l_Collection_l_Array_l_size__Array(BValue array) {
  BSTS_Array* arr = bsts_array_unbox(array);
  return bsts_integer_from_int(arr->len);
}

BValue ___bsts_g_Bosatsu_l_Collection_l_Array_l_get__map__Array(BValue array, BValue index, BValue default_fn, BValue map_fn) {
  BSTS_Array* arr = bsts_array_unbox(array);

  if (bsts_array_index_in_range(index, arr->len)) {
    int idx = (int)bsts_integer_to_int32(index);
    return call_fn1(map_fn, arr->data[arr->offset + idx]);
  }

  return call_fn1(default_fn, bsts_unit_value());
}

BValue ___bsts_g_Bosatsu_l_Collection_l_Array_l_get__or__Array(BValue array, BValue index, BValue default_fn) {
  BSTS_Array* arr = bsts_array_unbox(array);

  if (bsts_array_index_in_range(index, arr->len)) {
    int idx = (int)bsts_integer_to_int32(index);
    return arr->data[arr->offset + idx];
  }

  return call_fn1(default_fn, bsts_unit_value());
}

BValue ___bsts_g_Bosatsu_l_Collection_l_Array_l_foldl__Array(BValue array, BValue init, BValue fn) {
  BSTS_Array* arr = bsts_array_unbox(array);

  BValue acc = init;
  for (int idx = 0; idx < arr->len; idx++) {
    acc = call_fn2(fn, acc, arr->data[arr->offset + idx]);
  }

  return acc;
}

BValue ___bsts_g_Bosatsu_l_Collection_l_Array_l_map__Array(BValue array, BValue fn) {
  BSTS_Array* arr = bsts_array_unbox(array);

  if (arr->len <= 0) {
    return bsts_array_empty();
  }

  BValue* data = bsts_array_alloc_data(arr->len);
  for (int idx = 0; idx < arr->len; idx++) {
    data[idx] = call_fn1(fn, arr->data[arr->offset + idx]);
  }

  return bsts_array_wrap(data, 0, arr->len);
}

BValue ___bsts_g_Bosatsu_l_Collection_l_Array_l_set__or__self__Array(BValue array, BValue index, BValue value) {
  BSTS_Array* arr = bsts_array_unbox(array);

  if (!bsts_array_index_in_range(index, arr->len)) {
    return array;
  }

  int idx = (int)bsts_integer_to_int32(index);
  BValue* data = bsts_array_alloc_data(arr->len);

  if (arr->len > 0) {
    memcpy(data, arr->data + arr->offset, sizeof(BValue) * (size_t)arr->len);
  }
  data[idx] = value;

  return bsts_array_wrap(data, 0, arr->len);
}

BValue ___bsts_g_Bosatsu_l_Collection_l_Array_l_sort__Array(BValue array, BValue cmp_fn) {
  BSTS_Array* arr = bsts_array_unbox(array);

  if (arr->len < 2) {
    return array;
  }

  BValue* data = bsts_array_alloc_data(arr->len);
  memcpy(data, arr->data + arr->offset, sizeof(BValue) * (size_t)arr->len);

  bsts_array_stable_sort(data, arr->len, cmp_fn);
  return bsts_array_wrap(data, 0, arr->len);
}

BValue ___bsts_g_Bosatsu_l_Collection_l_Array_l_concat__all__Array(BValue arrays) {
  long total = 0;
  BValue current = arrays;

  while (get_variant(current) != 0) {
    BSTS_Array* arr = bsts_array_unbox(get_enum_index(current, 0));
    total += (long)arr->len;
    if (total > (long)INT_MAX) {
      return bsts_array_empty();
    }
    current = get_enum_index(current, 1);
  }

  if (total <= 0) {
    return bsts_array_empty();
  }

  int total_len = (int)total;
  BValue* data = bsts_array_alloc_data(total_len);

  int write = 0;
  current = arrays;
  while (get_variant(current) != 0) {
    BSTS_Array* arr = bsts_array_unbox(get_enum_index(current, 0));
    if (arr->len > 0) {
      memcpy(data + write, arr->data + arr->offset, sizeof(BValue) * (size_t)arr->len);
      write += arr->len;
    }
    current = get_enum_index(current, 1);
  }

  return bsts_array_wrap(data, 0, total_len);
}

BValue ___bsts_g_Bosatsu_l_Collection_l_Array_l_slice__Array(BValue array, BValue start, BValue end) {
  BSTS_Array* arr = bsts_array_unbox(array);

  BValue zero = bsts_integer_from_int(0);
  BValue len_value = bsts_integer_from_int(arr->len);

  BValue start1 = bsts_integer_cmp(start, zero) < 0 ? zero : start;
  BValue end1 = bsts_integer_cmp(end, len_value) > 0 ? len_value : end;

  _Bool valid =
      (bsts_integer_cmp(start1, zero) >= 0) &&
      (bsts_integer_cmp(end1, zero) >= 0) &&
      (bsts_integer_cmp(start1, end1) <= 0) &&
      (bsts_integer_cmp(end1, len_value) <= 0);

  if (!valid) {
    return bsts_array_empty();
  }

  BValue slice_len_value = bsts_integer_add(end1, bsts_integer_negate(start1));
  if (bsts_integer_cmp(slice_len_value, zero) <= 0) {
    return bsts_array_empty();
  }

  int start_idx = (int)bsts_integer_to_int32(start1);
  int slice_len = (int)bsts_integer_to_int32(slice_len_value);
  return bsts_array_wrap(arr->data, arr->offset + start_idx, slice_len);
}
