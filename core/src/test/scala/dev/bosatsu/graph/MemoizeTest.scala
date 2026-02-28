package dev.bosatsu.graph

import cats.syntax.all._
import dev.bosatsu.{Par, ParTest}

class MemoizeTest extends munit.FunSuite with ParTest {

  private def fibExpected(n: Int): Long = {
    @annotation.tailrec
    def loop(rem: Int, a: Long, b: Long): Long =
      if (rem <= 0) a else loop(rem - 1, b, a + b)

    loop(n, 0L, 1L)
  }

  private def memoFib: Int => Par.F[Long] =
    Memoize.memoizeDag[Par.F, Int, Long] { (n, recur) =>
      if (n <= 1) n.toLong.pure[Par.F]
      else (recur(n - 1), recur(n - 2)).mapN(_ + _)
    }

  private def memoFibFutureCompat: Int => Par.F[Long] =
    Memoize.memoizeDagFuture[Int, Long] { (n, recur) =>
      if (n <= 1) n.toLong.pure[Par.F]
      else (recur(n - 1), recur(n - 2)).mapN(_ + _)
    }

  test("memoizeDag computes naive recursive fibonacci correctly") {
    val fib = memoFib
    val inputs = List(0, 1, 2, 3, 5, 10, 20, 30)
    val got =
      Par.await(inputs.traverse(n => fib(n).map(v => (n, v))))

    val expected = inputs.map(n => (n, fibExpected(n)))
    assertEquals(got, expected)
  }

  test("memoizeDag handles repeated same-key requests without deadlock") {
    val fib = memoFib
    val n = 35
    val requests = 64

    val got = Par.await(List.fill(requests)(fib(n)).sequence)
    assertEquals(got, List.fill(requests)(fibExpected(n)))
  }

  test("memoizeDagFuture remains compatible for fibonacci") {
    val fib = memoFibFutureCompat
    val n = 35
    val got = Par.await(fib(n))

    assertEquals(got, fibExpected(n))
  }
}
