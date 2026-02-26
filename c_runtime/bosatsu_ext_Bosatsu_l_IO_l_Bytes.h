#include "bosatsu_runtime.h"

#include <stdint.h>

typedef struct
{
  uint8_t *data;
  int offset;
  int len;
} BSTS_Bytes;

BSTS_Bytes *bsts_bytes_unbox(BValue bytes);
BValue bsts_bytes_wrap(uint8_t *data, int offset, int len);
BValue bsts_bytes_empty(void);

BValue ___bsts_g_Bosatsu_l_IO_l_Bytes_l_empty__Bytes();
BValue ___bsts_g_Bosatsu_l_IO_l_Bytes_l_from__List__Int(BValue ints);
BValue ___bsts_g_Bosatsu_l_IO_l_Bytes_l_from__Array__Int(BValue ints);
BValue ___bsts_g_Bosatsu_l_IO_l_Bytes_l_to__List__Int(BValue bytes);
BValue ___bsts_g_Bosatsu_l_IO_l_Bytes_l_to__Array__Int(BValue bytes);
BValue ___bsts_g_Bosatsu_l_IO_l_Bytes_l_size__Bytes(BValue bytes);
BValue ___bsts_g_Bosatsu_l_IO_l_Bytes_l_get__map__Bytes(BValue bytes, BValue index, BValue default_fn, BValue map_fn);
BValue ___bsts_g_Bosatsu_l_IO_l_Bytes_l_get__or__Bytes(BValue bytes, BValue index, BValue default_fn);
BValue ___bsts_g_Bosatsu_l_IO_l_Bytes_l_foldl__Bytes(BValue bytes, BValue init, BValue fn);
BValue ___bsts_g_Bosatsu_l_IO_l_Bytes_l_concat__all__Bytes(BValue chunks);
BValue ___bsts_g_Bosatsu_l_IO_l_Bytes_l_slice__Bytes(BValue bytes, BValue start, BValue end);
BValue ___bsts_g_Bosatsu_l_IO_l_Bytes_l_starts__with__Bytes(BValue bytes, BValue prefix);
BValue ___bsts_g_Bosatsu_l_IO_l_Bytes_l_ends__with__Bytes(BValue bytes, BValue suffix);
BValue ___bsts_g_Bosatsu_l_IO_l_Bytes_l_find__Bytes(BValue bytes, BValue needle, BValue start);
BValue ___bsts_g_Bosatsu_l_IO_l_Bytes_l_utf8__bytes__from__String(BValue str);
BValue ___bsts_g_Bosatsu_l_IO_l_Bytes_l_utf8__bytes__to__String(BValue bytes);
BValue ___bsts_g_Bosatsu_l_IO_l_Bytes_l_utf8__Char__at(BValue bytes, BValue index);
