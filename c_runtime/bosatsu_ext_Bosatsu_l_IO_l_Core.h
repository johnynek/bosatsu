#include "bosatsu_runtime.h"

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_path__sep();

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_stdin();
BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_stdout();
BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_stderr();

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_read__utf8(BValue h, BValue max_chars);
BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_write__utf8(BValue h, BValue s);
BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_flush(BValue h);
BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_close(BValue h);

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_open__file(BValue path, BValue mode);
BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_list__dir(BValue path);
BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_stat(BValue path);
BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_mkdir(BValue path, BValue recursive);
BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_remove(BValue path, BValue recursive);
BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_rename(BValue from, BValue to);
BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_get__env(BValue name);
BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_spawn(BValue cmd, BValue args, BValue stdio);
BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_wait(BValue process);

BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_now__wall();
BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_now__mono();
BValue ___bsts_g_Bosatsu_l_IO_l_Core_l_sleep(BValue duration);
