package org.bykn.bosatsu

object Platform {
  @inline
  final val isScalaJs = true
  @inline
  final val isScalaJvm = false

  def detectOs(): OsPlatformId = {
    import scala.scalajs.js
    import scala.scalajs.js.Dynamic.global

    if (
      js.typeOf(global.process) != "undefined" && js.typeOf(
        global.process.platform
      ) != "undefined"
    ) {
      // Node.js environment
      global.process.platform.asInstanceOf[String] match {
        case "win32"  => OsPlatformId.Windows
        case "darwin" => OsPlatformId.MacOS
        case "linux"  => OsPlatformId.Linux
        case plat     => OsPlatformId.Unknown(plat)
      }
    } else if (js.typeOf(global.navigator) != "undefined") {
      // Browser environment
      val userAgent0 = global.navigator.userAgent.asInstanceOf[String]
      val userAgent = userAgent0.toLowerCase
      if (userAgent.contains("win")) OsPlatformId.Windows
      else if (userAgent.contains("mac")) OsPlatformId.MacOS
      else if (userAgent.contains("linux")) OsPlatformId.Linux
      else OsPlatformId.Unknown(userAgent0)
    } else {
      OsPlatformId.Unknown("unknown js")
    }
  }
}
