package dev.bosatsu

import fs2.io.file.Path
import munit.FunSuite
import scala.scalajs.js

class Fs2PlatformIOTest extends FunSuite {
  test("relativize handles src and ./src under an absolute root") {
    val root = Path(js.Dynamic.global.process.cwd().asInstanceOf[String])
    val srcAbs = Fs2PlatformIO.resolve(root, Path("src"))
    val srcDotAbs = Fs2PlatformIO.resolve(root, Path("./src"))

    val relSrc = Fs2PlatformIO.relativize(root, srcAbs)
    assert(relSrc.nonEmpty)
    assertEquals(Fs2PlatformIO.resolve(root, relSrc.get), srcAbs)

    val relDotSrc = Fs2PlatformIO.relativize(root, srcDotAbs)
    assert(relDotSrc.nonEmpty)
    assertEquals(Fs2PlatformIO.resolve(root, relDotSrc.get), srcDotAbs)
  }
}
