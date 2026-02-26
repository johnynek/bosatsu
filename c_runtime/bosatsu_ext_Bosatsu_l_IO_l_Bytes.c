#include "bosatsu_ext_Bosatsu_l_IO_l_Bytes.h"

#include <gc.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct
{
  BValue *data;
  int offset;
  int len;
} BSTS_Array;

static void bsts_bytes_free(void *ptr)
{
  (void)ptr;
}

BSTS_Bytes *bsts_bytes_unbox(BValue bytes)
{
  return (BSTS_Bytes *)get_external(bytes);
}

static uint8_t *bsts_bytes_alloc_data(int len)
{
  if (len <= 0)
  {
    return NULL;
  }

  uint8_t *data = (uint8_t *)GC_malloc_atomic((size_t)len);
  if (data == NULL)
  {
    perror("GC_malloc_atomic failure in bsts_bytes_alloc_data");
    abort();
  }
  return data;
}

BValue bsts_bytes_wrap(uint8_t *data, int offset, int len)
{
  BSTS_Bytes *bytes = (BSTS_Bytes *)GC_malloc(sizeof(BSTS_Bytes));
  if (bytes == NULL)
  {
    perror("GC_malloc failure in bsts_bytes_wrap");
    abort();
  }

  bytes->data = data;
  bytes->offset = offset;
  bytes->len = len;
  return alloc_external(bytes, bsts_bytes_free);
}

BValue bsts_bytes_empty(void)
{
  return bsts_bytes_wrap(NULL, 0, 0);
}

static BValue *bsts_array_alloc_data(int len)
{
  if (len <= 0)
  {
    return NULL;
  }

  BValue *data = (BValue *)GC_malloc(sizeof(BValue) * (size_t)len);
  if (data == NULL)
  {
    perror("GC_malloc failure in bsts_array_alloc_data");
    abort();
  }
  return data;
}

static void bsts_array_free(void *ptr)
{
  (void)ptr;
}

static BValue bsts_array_wrap(BValue *data, int offset, int len)
{
  BSTS_Array *arr = (BSTS_Array *)GC_malloc(sizeof(BSTS_Array));
  if (arr == NULL)
  {
    perror("GC_malloc failure in bsts_array_wrap");
    abort();
  }

  arr->data = data;
  arr->offset = offset;
  arr->len = len;
  return alloc_external(arr, bsts_array_free);
}

static _Bool bsts_bytes_index_in_range(BValue index, int len)
{
  BValue zero = bsts_integer_from_int(0);
  BValue len_value = bsts_integer_from_int(len);

  return (bsts_integer_cmp(index, zero) >= 0) &&
         (bsts_integer_cmp(index, len_value) < 0);
}

static uint8_t bsts_byte_from_int(BValue value)
{
  uint64_t low = bsts_integer_to_low_uint64(value);
  return (uint8_t)(low & 0xffULL);
}

static BValue bsts_bool(_Bool b)
{
  if (b)
  {
    return alloc_enum0(1);
  }
  return alloc_enum0(0);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Bytes_l_empty__Bytes()
{
  return bsts_bytes_empty();
}

BValue ___bsts_g_Bosatsu_l_IO_l_Bytes_l_from__List__Int(BValue ints)
{
  int count = 0;
  BValue current = ints;

  while (get_variant(current) != 0)
  {
    count += 1;
    current = get_enum_index(current, 1);
  }

  if (count <= 0)
  {
    return bsts_bytes_empty();
  }

  uint8_t *data = bsts_bytes_alloc_data(count);
  current = ints;
  int idx = 0;
  while (get_variant(current) != 0)
  {
    data[idx] = bsts_byte_from_int(get_enum_index(current, 0));
    idx += 1;
    current = get_enum_index(current, 1);
  }

  return bsts_bytes_wrap(data, 0, count);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Bytes_l_from__Array__Int(BValue ints)
{
  BSTS_Array *arr = (BSTS_Array *)get_external(ints);
  if (arr->len <= 0)
  {
    return bsts_bytes_empty();
  }

  uint8_t *data = bsts_bytes_alloc_data(arr->len);
  for (int idx = 0; idx < arr->len; idx++)
  {
    data[idx] = bsts_byte_from_int(arr->data[arr->offset + idx]);
  }

  return bsts_bytes_wrap(data, 0, arr->len);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Bytes_l_to__List__Int(BValue bytes)
{
  BSTS_Bytes *b = bsts_bytes_unbox(bytes);

  BValue result = alloc_enum0(0);
  for (int idx = b->len - 1; idx >= 0; idx--)
  {
    int value = (int)b->data[b->offset + idx];
    result = alloc_enum2(1, bsts_integer_from_int(value), result);
  }

  return result;
}

BValue ___bsts_g_Bosatsu_l_IO_l_Bytes_l_to__Array__Int(BValue bytes)
{
  BSTS_Bytes *b = bsts_bytes_unbox(bytes);
  if (b->len <= 0)
  {
    return bsts_array_wrap(NULL, 0, 0);
  }

  BValue *data = bsts_array_alloc_data(b->len);
  for (int idx = 0; idx < b->len; idx++)
  {
    int value = (int)b->data[b->offset + idx];
    data[idx] = bsts_integer_from_int(value);
  }

  return bsts_array_wrap(data, 0, b->len);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Bytes_l_size__Bytes(BValue bytes)
{
  BSTS_Bytes *b = bsts_bytes_unbox(bytes);
  return bsts_integer_from_int(b->len);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Bytes_l_get__map__Bytes(BValue bytes, BValue index, BValue default_fn, BValue map_fn)
{
  BSTS_Bytes *b = bsts_bytes_unbox(bytes);

  if (bsts_bytes_index_in_range(index, b->len))
  {
    int idx = (int)bsts_integer_to_int32(index);
    int value = (int)b->data[b->offset + idx];
    return call_fn1(map_fn, bsts_integer_from_int(value));
  }

  return call_fn1(default_fn, bsts_unit_value());
}

BValue ___bsts_g_Bosatsu_l_IO_l_Bytes_l_get__or__Bytes(BValue bytes, BValue index, BValue default_fn)
{
  BSTS_Bytes *b = bsts_bytes_unbox(bytes);

  if (bsts_bytes_index_in_range(index, b->len))
  {
    int idx = (int)bsts_integer_to_int32(index);
    int value = (int)b->data[b->offset + idx];
    return bsts_integer_from_int(value);
  }

  return call_fn1(default_fn, bsts_unit_value());
}

BValue ___bsts_g_Bosatsu_l_IO_l_Bytes_l_foldl__Bytes(BValue bytes, BValue init, BValue fn)
{
  BSTS_Bytes *b = bsts_bytes_unbox(bytes);

  BValue acc = init;
  for (int idx = 0; idx < b->len; idx++)
  {
    int value = (int)b->data[b->offset + idx];
    acc = call_fn2(fn, acc, bsts_integer_from_int(value));
  }

  return acc;
}

BValue ___bsts_g_Bosatsu_l_IO_l_Bytes_l_concat__all__Bytes(BValue chunks)
{
  long total = 0;
  BValue current = chunks;

  while (get_variant(current) != 0)
  {
    BSTS_Bytes *b = bsts_bytes_unbox(get_enum_index(current, 0));
    total += (long)b->len;
    if (total > (long)INT_MAX)
    {
      return bsts_bytes_empty();
    }
    current = get_enum_index(current, 1);
  }

  if (total <= 0)
  {
    return bsts_bytes_empty();
  }

  int total_len = (int)total;
  uint8_t *data = bsts_bytes_alloc_data(total_len);
  int write = 0;
  current = chunks;
  while (get_variant(current) != 0)
  {
    BSTS_Bytes *b = bsts_bytes_unbox(get_enum_index(current, 0));
    if (b->len > 0)
    {
      memcpy(data + write, b->data + b->offset, (size_t)b->len);
      write += b->len;
    }
    current = get_enum_index(current, 1);
  }

  return bsts_bytes_wrap(data, 0, total_len);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Bytes_l_slice__Bytes(BValue bytes, BValue start, BValue end)
{
  BSTS_Bytes *b = bsts_bytes_unbox(bytes);

  BValue zero = bsts_integer_from_int(0);
  BValue len_value = bsts_integer_from_int(b->len);

  BValue start1 = bsts_integer_cmp(start, zero) < 0 ? zero : start;
  BValue end1 = bsts_integer_cmp(end, len_value) > 0 ? len_value : end;

  _Bool valid =
      (bsts_integer_cmp(start1, zero) >= 0) &&
      (bsts_integer_cmp(end1, zero) >= 0) &&
      (bsts_integer_cmp(start1, end1) <= 0) &&
      (bsts_integer_cmp(end1, len_value) <= 0);

  if (!valid)
  {
    return bsts_bytes_empty();
  }

  BValue slice_len_value = bsts_integer_add(end1, bsts_integer_negate(start1));
  if (bsts_integer_cmp(slice_len_value, zero) <= 0)
  {
    return bsts_bytes_empty();
  }

  int start_idx = (int)bsts_integer_to_int32(start1);
  int slice_len = (int)bsts_integer_to_int32(slice_len_value);
  return bsts_bytes_wrap(b->data, b->offset + start_idx, slice_len);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Bytes_l_starts__with__Bytes(BValue bytes, BValue prefix)
{
  BSTS_Bytes *b = bsts_bytes_unbox(bytes);
  BSTS_Bytes *p = bsts_bytes_unbox(prefix);

  if (p->len > b->len)
  {
    return alloc_enum0(0);
  }
  if (p->len == 0)
  {
    return alloc_enum0(1);
  }

  _Bool equal = memcmp(
                    b->data + b->offset,
                    p->data + p->offset,
                    (size_t)p->len) == 0;
  return bsts_bool(equal);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Bytes_l_ends__with__Bytes(BValue bytes, BValue suffix)
{
  BSTS_Bytes *b = bsts_bytes_unbox(bytes);
  BSTS_Bytes *s = bsts_bytes_unbox(suffix);

  if (s->len > b->len)
  {
    return alloc_enum0(0);
  }
  if (s->len == 0)
  {
    return alloc_enum0(1);
  }

  int start = b->len - s->len;
  _Bool equal = memcmp(
                    b->data + b->offset + start,
                    s->data + s->offset,
                    (size_t)s->len) == 0;
  return bsts_bool(equal);
}

BValue ___bsts_g_Bosatsu_l_IO_l_Bytes_l_find__Bytes(BValue bytes, BValue needle, BValue start)
{
  BSTS_Bytes *b = bsts_bytes_unbox(bytes);
  BSTS_Bytes *n = bsts_bytes_unbox(needle);
  BValue zero = bsts_integer_from_int(0);
  BValue len_value = bsts_integer_from_int(b->len);

  BValue start1 = start;
  if (bsts_integer_cmp(start1, zero) < 0)
  {
    start1 = zero;
  }
  else if (bsts_integer_cmp(start1, len_value) > 0)
  {
    start1 = len_value;
  }

  if (n->len == 0)
  {
    return start1;
  }

  int start_idx = (int)bsts_integer_to_int32(start1);
  int max_start = b->len - n->len;
  if (start_idx > max_start)
  {
    return bsts_integer_from_int(-1);
  }

  const uint8_t *haystack = b->data + b->offset;
  const uint8_t *needle_data = n->data + n->offset;

  for (int idx = start_idx; idx <= max_start; idx++)
  {
    if (memcmp(haystack + idx, needle_data, (size_t)n->len) == 0)
    {
      return bsts_integer_from_int(idx);
    }
  }

  return bsts_integer_from_int(-1);
}
