package dev.bosatsu

private[bosatsu] object PredefIoCorePlatform {
  def addIoCoreExternals(
      externals: Externals,
      ioCorePackageName: PackageName
  ): Externals =
    externals
      .add(
        ioCorePackageName,
        "path_sep",
        FfiCall.Const(PredefImpl.core_path_sep)
      )
      .add(ioCorePackageName, "stdin", FfiCall.Const(PredefImpl.core_stdin))
      .add(
        ioCorePackageName,
        "stdout",
        FfiCall.Const(PredefImpl.core_stdout)
      )
      .add(
        ioCorePackageName,
        "stderr",
        FfiCall.Const(PredefImpl.core_stderr)
      )
      .add(
        ioCorePackageName,
        "read_utf8",
        FfiCall.Fn2(PredefImpl.prog_core_read_utf8(_, _))
      )
      .add(
        ioCorePackageName,
        "write_utf8",
        FfiCall.Fn2(PredefImpl.prog_core_write_utf8(_, _))
      )
      .add(
        ioCorePackageName,
        "read_bytes",
        FfiCall.Fn2(PredefImpl.prog_core_read_bytes(_, _))
      )
      .add(
        ioCorePackageName,
        "write_bytes",
        FfiCall.Fn2(PredefImpl.prog_core_write_bytes(_, _))
      )
      .add(
        ioCorePackageName,
        "read_all_bytes",
        FfiCall.Fn2(PredefImpl.prog_core_read_all_bytes(_, _))
      )
      .add(
        ioCorePackageName,
        "copy_bytes",
        FfiCall.Fn4(PredefImpl.prog_core_copy_bytes(_, _, _, _))
      )
      .add(
        ioCorePackageName,
        "flush",
        FfiCall.Fn1(PredefImpl.prog_core_flush(_))
      )
      .add(
        ioCorePackageName,
        "close",
        FfiCall.Fn1(PredefImpl.prog_core_close(_))
      )
      .add(
        ioCorePackageName,
        "open_file",
        FfiCall.Fn2(PredefImpl.prog_core_open_file(_, _))
      )
      .add(
        ioCorePackageName,
        "create_temp_file",
        FfiCall.Fn3(PredefImpl.prog_core_create_temp_file(_, _, _))
      )
      .add(
        ioCorePackageName,
        "create_temp_dir",
        FfiCall.Fn2(PredefImpl.prog_core_create_temp_dir(_, _))
      )
      .add(
        ioCorePackageName,
        "list_dir",
        FfiCall.Fn1(PredefImpl.prog_core_list_dir(_))
      )
      .add(
        ioCorePackageName,
        "stat",
        FfiCall.Fn1(PredefImpl.prog_core_stat(_))
      )
      .add(
        ioCorePackageName,
        "mkdir",
        FfiCall.Fn2(PredefImpl.prog_core_mkdir(_, _))
      )
      .add(
        ioCorePackageName,
        "remove",
        FfiCall.Fn2(PredefImpl.prog_core_remove(_, _))
      )
      .add(
        ioCorePackageName,
        "rename",
        FfiCall.Fn2(PredefImpl.prog_core_rename(_, _))
      )
      .add(
        ioCorePackageName,
        "get_env",
        FfiCall.Fn1(PredefImpl.prog_core_get_env(_))
      )
      .add(
        ioCorePackageName,
        "spawn",
        FfiCall.Fn3(PredefImpl.prog_core_spawn(_, _, _))
      )
      .add(
        ioCorePackageName,
        "wait",
        FfiCall.Fn1(PredefImpl.prog_core_wait(_))
      )
      .add(
        ioCorePackageName,
        "now_wall",
        FfiCall.Const(PredefImpl.prog_core_now_wall)
      )
      .add(
        ioCorePackageName,
        "now_mono",
        FfiCall.Const(PredefImpl.prog_core_now_mono)
      )
      .add(
        ioCorePackageName,
        "sleep",
        FfiCall.Fn1(PredefImpl.prog_core_sleep(_))
      )
}
