package org.bykn.bosatsu

object Platform {
  @inline
  final val isScalaJs = false
  @inline
  final val isScalaJvm = true

  def detectOs(): OsPlatformId = {
    val osName0 = System.getProperty("os.name")
    val osName = osName0.toLowerCase
    if (osName.contains("win")) OsPlatformId.Windows
    else if (osName.contains("mac")) OsPlatformId.MacOS
    else if (osName.contains("linux")) OsPlatformId.Linux
    else OsPlatformId.Unknown(osName0)
  }
}
