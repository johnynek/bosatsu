package dev.bosatsu

import dev.bosatsu.ReservedPackage.IoCore

private[bosatsu] object PredefIoCorePlatform {
  def addIoCoreExternals(externals: Externals): Externals =
    externals
      .add(
        IoCore.external("path_sep"),
        FfiCall.Const(PredefImpl.core_path_sep)
      )
      .add(IoCore.external("stdin"), FfiCall.Const(PredefImpl.core_stdin))
      .add(
        IoCore.external("stdout"),
        FfiCall.Const(PredefImpl.core_stdout)
      )
      .add(
        IoCore.external("stderr"),
        FfiCall.Const(PredefImpl.core_stderr)
      )
      .add(
        IoCore.external("read_utf8"),
        FfiCall.Fn2(PredefImpl.prog_core_read_utf8(_, _))
      )
      .add(
        IoCore.external("write_utf8"),
        FfiCall.Fn2(PredefImpl.prog_core_write_utf8(_, _))
      )
      .add(
        IoCore.external("read_bytes"),
        FfiCall.Fn2(PredefImpl.prog_core_read_bytes(_, _))
      )
      .add(
        IoCore.external("write_bytes"),
        FfiCall.Fn2(PredefImpl.prog_core_write_bytes(_, _))
      )
      .add(
        IoCore.external("read_all_bytes"),
        FfiCall.Fn2(PredefImpl.prog_core_read_all_bytes(_, _))
      )
      .add(
        IoCore.external("copy_bytes"),
        FfiCall.Fn4(PredefImpl.prog_core_copy_bytes(_, _, _, _))
      )
      .add(
        IoCore.external("flush"),
        FfiCall.Fn1(PredefImpl.prog_core_flush(_))
      )
      .add(
        IoCore.external("close"),
        FfiCall.Fn1(PredefImpl.prog_core_close(_))
      )
      .add(
        IoCore.external("open_file"),
        FfiCall.Fn2(PredefImpl.prog_core_open_file(_, _))
      )
      .add(
        IoCore.external("create_temp_file"),
        FfiCall.Fn3(PredefImpl.prog_core_create_temp_file(_, _, _))
      )
      .add(
        IoCore.external("create_temp_dir"),
        FfiCall.Fn2(PredefImpl.prog_core_create_temp_dir(_, _))
      )
      .add(
        IoCore.external("list_dir"),
        FfiCall.Fn1(PredefImpl.prog_core_list_dir(_))
      )
      .add(
        IoCore.external("stat"),
        FfiCall.Fn1(PredefImpl.prog_core_stat(_))
      )
      .add(
        IoCore.external("mkdir"),
        FfiCall.Fn2(PredefImpl.prog_core_mkdir(_, _))
      )
      .add(
        IoCore.external("mkdir_with_mode"),
        FfiCall.Fn3(PredefImpl.prog_core_mkdir_with_mode(_, _, _))
      )
      .add(
        IoCore.external("remove"),
        FfiCall.Fn2(PredefImpl.prog_core_remove(_, _))
      )
      .add(
        IoCore.external("rename"),
        FfiCall.Fn2(PredefImpl.prog_core_rename(_, _))
      )
      .add(
        IoCore.external("get_env"),
        FfiCall.Fn1(PredefImpl.prog_core_get_env(_))
      )
      .add(
        IoCore.external("spawn"),
        FfiCall.Fn3(PredefImpl.prog_core_spawn(_, _, _))
      )
      .add(
        IoCore.external("wait"),
        FfiCall.Fn1(PredefImpl.prog_core_wait(_))
      )
      .add(
        IoCore.external("terminate"),
        FfiCall.Fn1(PredefImpl.prog_core_terminate(_))
      )
      .add(
        IoCore.external("kill"),
        FfiCall.Fn1(PredefImpl.prog_core_kill(_))
      )
      .add(
        IoCore.external("poll"),
        FfiCall.Fn1(PredefImpl.prog_core_poll(_))
      )
      .add(
        IoCore.external("wait_timeout"),
        FfiCall.Fn2(PredefImpl.prog_core_wait_timeout(_, _))
      )
      .add(
        IoCore.external("now_wall"),
        FfiCall.Const(PredefImpl.prog_core_now_wall)
      )
      .add(
        IoCore.external("now_mono"),
        FfiCall.Const(PredefImpl.prog_core_now_mono)
      )
      .add(
        IoCore.external("sleep"),
        FfiCall.Fn1(PredefImpl.prog_core_sleep(_))
      )
}
