package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.data.Chain
import org.scalacheck.Prop.forAll

class MemoryPlatformIOTest extends munit.ScalaCheckSuite {
  private type Result[A] = Either[Throwable, A]
  private val platform = MemoryMain.memoryPlatformIO[Result]

  test("interface bundles round trip through binary file contents") {
    val path = Chain.one("bundle.sig")
    val bundle = proto.Interfaces(
      List(proto.Interface(strings = List("A", "Ünicode"), packageName = 1))
    )
    val read = platform
      .writeInterfaceBundle(bundle, path)
      .flatMap(_ => platform.readInterfaceBundle(path))
      .runA(MemoryMain.State.empty)
    assertEquals(read, Right(bundle))
  }

  property("binary file contents compare by content, not by reference") {
    forAll { (left: List[Byte], right: List[Byte]) =>
      val content =
        MemoryMain.FileContent.Bytes(Bytes.copyFromArray(left.toArray))
      val copy = MemoryMain.FileContent.Bytes(Bytes.copyFromArray(left.toArray))
      val other =
        MemoryMain.FileContent.Bytes(Bytes.copyFromArray(right.toArray))
      assert(content.equals(copy))
      assertEquals(content.hashCode, copy.hashCode)
      assertEquals(content.equals(other), left == right)
      assert(!content.equals(MemoryMain.FileContent.Str("")))
    }
  }

  property("stored binary file contents are isolated from caller arrays") {
    forAll { (head: Byte, tail: List[Byte]) =>
      val values = head :: tail
      val paths = List("direct", "write", "atomic").map(Chain.one)
      val source = values.toArray
      val initial = MemoryMain.State.empty
        .withFile(
          paths.head,
          MemoryMain.FileContent.Bytes(Bytes.copyFromArray(source))
        )
        .get
      val state = platform
        .writeBytes(paths(1), Bytes.copyFromArray(source))
        .flatMap(_ =>
          platform.writeFileSetAtomic(
            List(paths(2) -> Bytes.copyFromArray(source))
          )
        )
        .runS(initial)
        .toOption
        .get
      source(0) = (source(0) ^ 0xff).toByte

      paths.foreach { path =>
        val read = platform.readBytes(path)
        val first = read.runA(state).toOption.get.copyToArray
        first(0) = (first(0) ^ 0xff).toByte
        val second = read.runA(state).toOption.get
        assertEquals(second.copyToArray.toList, values)
      }
    }
  }
}
