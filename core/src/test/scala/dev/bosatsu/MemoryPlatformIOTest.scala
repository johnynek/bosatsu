package dev.bosatsu

import cats.data.Chain
import cats.syntax.all._
import dev.bosatsu.graph.Memoize
import org.scalacheck.Prop.forAll

class MemoryPlatformIOTest extends munit.ScalaCheckSuite {
  private type Result[A] = Either[Throwable, A]
  private val platform = MemoryMain.memoryPlatformIO[Result]

  property("binary file reads cannot mutate stored file contents") {
    forAll { (head: Byte, tail: List[Byte]) =>
      val values = head :: tail
      val paths = List("direct", "write", "atomic").map(Chain.one)
      val initial = MemoryMain.State.empty.withFile(
        paths.head,
        MemoryMain.FileContent.Bytes(Bytes.copyFromArray(values.toArray))
      ).get
      val state = platform.writeBytes(paths(1), values.toArray)
        .flatMap(_ => platform.writeFileSetAtomic(
          List(paths(2) -> Bytes.copyFromArray(values.toArray))
        ))
        .runS(initial).toOption.get

      paths.foreach { path =>
        val read = platform.readBytes(path)
        val first = read.runA(state).toOption.get
        first(0) = (first(0) ^ 0xff).toByte
        val second = read.runA(state).toOption.get
        assertEquals(second.toList, values)
      }
    }
  }

  test("promise effects are deferred until their StateT actions run") {
    val canPromise = platform.canPromiseF

    var delayedRuns = 0
    val delayed = canPromise.delay {
      delayedRuns += 1
      41
    }
    assertEquals(delayedRuns, 0)
    assertEquals(delayed.runA(MemoryMain.State.empty), Right(41))
    assertEquals(delayedRuns, 1)

    var computedRuns = 0
    val computed = canPromise.compute {
      computedRuns += 1
      42
    }
    assertEquals(computedRuns, 0)
    assertEquals(computed.runA(MemoryMain.State.empty), Right(42))
    assertEquals(computedRuns, 1)

    val promiseResult = canPromise.delay(canPromise.unsafeNewPromise[Int]).flatMap {
      promise =>
        val wait = canPromise.wait(promise)
        val complete = canPromise.completeWith(promise, canPromise.delay(43))
        complete *> wait
    }
    assertEquals(promiseResult.runA(MemoryMain.State.empty), Right(43))

    val writtenPath = Chain.one("promise-output")
    val stateThreaded = canPromise.delay(canPromise.unsafeNewPromise[Unit]).flatMap {
      promise =>
        val write = canPromise.completeWith(
          promise,
          platform.writeBytes(writtenPath, Array[Byte](44))
        )
        write *> canPromise.wait(promise) *>
          platform.readBytes(writtenPath).map(_.toList)
    }
    assertEquals(stateThreaded.runA(MemoryMain.State.empty), Right(List[Byte](44)))

    val failure = new Exception("memoized failure")
    var failedRuns = 0
    val failedWaits = canPromise.delay(canPromise.unsafeNewPromise[Int]).flatMap {
      promise =>
        val complete = canPromise.completeWith(promise, canPromise.compute {
          failedRuns += 1
          throw failure
        })
        complete *> canPromise.wait(promise).attempt.flatMap { first =>
          canPromise.wait(promise).attempt.map(second => (first, second))
        }
    }
    val failedResult = failedWaits.runA(MemoryMain.State.empty)
    assert(failedResult.exists { case (first, second) =>
      first.left.toOption.exists(_ eq failure) &&
      second.left.toOption.exists(_ eq failure)
    })
    assertEquals(failedRuns, 1)

    val emptyWait = canPromise.delay(canPromise.unsafeNewPromise[Int]).flatMap {
      promise => canPromise.wait(promise)
    }
    assertEquals(
      emptyWait.runA(MemoryMain.State.empty).left.toOption.map(_.getMessage),
      Some("invariant violation: waited on uninitialized promise")
    )
  }

  test("memoized DAG roots can be constructed before shared dependencies run") {
    val canPromise = platform.canPromiseF
    val moduleIOMonad = platform.moduleIOMonad

    type F[A] = MemoryMain.StateF[Result][A]
    given cats.Monad[F] = moduleIOMonad
    given dev.bosatsu.graph.CanPromise[F] = canPromise

    def run(order: List[Int]): Either[Throwable, (List[Int], List[Int])] = {
      val runs = Array.fill(5)(0)
      val memoized = Memoize.memoizeDag[F, Int, Int] { (node, recur) =>
        node match {
          case 0 => canPromise.compute {
            runs(0) += 1
            10
          }
          case 1 => canPromise.compute {
            runs(1) += 1
            20
          }
          case 2 => recur(0).flatMap { left =>
            recur(1).flatMap { right =>
              canPromise.compute {
                runs(2) += 1
                left + right
              }
            }
          }
          case 3 => recur(0).flatMap { left =>
            recur(1).flatMap { right =>
              canPromise.compute {
                runs(3) += 1
                left + right
              }
            }
          }
          case 4 => recur(2).flatMap { left =>
            recur(3).flatMap { right =>
              canPromise.compute {
                runs(4) += 1
                left + right
              }
            }
          }
          case other =>
            moduleIOMonad.raiseError[Int](new Exception(s"unknown node $other"))
        }
      }
      val roots = (0 to 4).iterator.map(node => node -> memoized(node)).toMap
      order.traverse(roots(_)).map(values => (values, runs.toList))
        .runA(MemoryMain.State.empty)
    }

    val dependencyFirst = run(List(4, 2, 3, 1, 0))
    val rootsFirst = run(List(0, 1, 2, 3, 4))
    val expected = List(60, 30, 30, 20, 10)
    val expectedRuns = List(1, 1, 1, 1, 1)
    assertEquals(dependencyFirst, Right((expected, expectedRuns)))
    assertEquals(rootsFirst, Right((List(10, 20, 30, 30, 60), expectedRuns)))
  }
}
