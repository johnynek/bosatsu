package dev.bosatsu

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.DurationInt

class ParJvmTest extends munit.FunSuite {
  // A pool that cannot replace blocked workers deadlocks on these. munit runs
  // a synchronous test body on the test thread and cannot time it out, so run
  // the body elsewhere and fail on a timeout instead of hanging.
  private def completesWithin20s[A](body: => A): A =
    Await.result(Future(body)(using ExecutionContext.global), 20.seconds)

  // Every outer task awaits inner tasks started on the same pool.
  private def nestedSum(outer: Int, inner: Int)(using Par.EC): Int =
    (0 until outer).toList
      .map { i =>
        Par.start {
          (0 until inner).toList
            .map(j => Par.start(i * inner + j))
            .map(Par.await)
            .sum
        }
      }
      .map(Par.await)
      .sum

  private def triangle(n: Int): Int = n * (n - 1) / 2

  List(1, 2).foreach { n =>
    val parallelism = Parallelism.fromInt(n).get

    test(s"nested start and await completes with parallelism $n") {
      val (outer, inner) = (8, 8)
      val sum = completesWithin20s {
        Par.withParallelism(parallelism)(nestedSum(outer, inner))
      }
      assertEquals(sum, triangle(outer * inner))
    }

    test(s"three levels of nested await complete with parallelism $n") {
      val sum = completesWithin20s {
        Par.withParallelism(parallelism) {
          (0 until 4).toList
            .map(i => Par.start(nestedSum(4, 4) + i))
            .map(Par.await)
            .sum
        }
      }
      assertEquals(sum, 4 * triangle(16) + triangle(4))
    }
  }

  test("Parallelism.fromInt accepts only positive counts") {
    assertEquals(Parallelism.fromInt(0), None)
    assertEquals(Parallelism.fromInt(-1), None)
    assertEquals(Parallelism.fromInt(3).map(_.toInt), Some(3))
    assertEquals(Parallelism.one.toInt, 1)
    assert(Parallelism.availableProcessors.toInt >= 1)
  }
}
