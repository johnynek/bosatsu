package dev.bosatsu

import cats.data.Chain
import org.scalacheck.Prop.forAll

class MemoryPlatformIOTest extends munit.ScalaCheckSuite {
  private type Result[A] = Either[Throwable, A]
  private val platform = MemoryMain.memoryPlatformIO[Result]

  property("binary file reads cannot mutate stored file contents") {
    forAll { (head: Byte, tail: List[Byte]) =>
      val values = head :: tail
      val paths = List("direct", "write", "atomic").map(Chain.one)
      val initial = MemoryMain.State.empty
        .withFile(
          paths.head,
          MemoryMain.FileContent.Bytes(Bytes.copyFromArray(values.toArray))
        )
        .get
      val state = platform
        .writeBytes(paths(1), values.toArray)
        .flatMap(_ =>
          platform.writeFileSetAtomic(
            List(paths(2) -> Bytes.copyFromArray(values.toArray))
          )
        )
        .runS(initial)
        .toOption
        .get

      paths.foreach { path =>
        val read = platform.readBytes(path)
        val first = read.runA(state).toOption.get
        first(0) = (first(0) ^ 0xff).toByte
        val second = read.runA(state).toOption.get
        assertEquals(second.toList, values)
      }
    }
  }
}
