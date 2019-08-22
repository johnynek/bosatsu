package org.bykn.bosatsu

import cats.data.{Const, Validated}
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

import cats.implicits._

@State(Scope.Thread)
class TestBench {
  val unitImplicits: Evaluation.UnitImplicits[Declaration] = Evaluation.UnitImplicits()
  import unitImplicits._

  private def prepPackages(packages: List[String], mainPackS: String): (PackageMap.Inferred, PackageName) = {
    val mainPack = PackageName.parse(mainPackS).get

    val parsed = packages.zipWithIndex.traverse { case (pack, i) =>
      Parser.parse(Package.parser, pack).map { case (lm, parsed) =>
        ((i.toString, lm), parsed)
      }
    }

    val parsedPaths = parsed match {
      case Validated.Valid(vs) => vs
      case Validated.Invalid(errs) =>
        errs.toList.foreach { p =>
          p.showContext.foreach(System.err.println)
        }
        sys.error("failed to parse") //errs.toString)
    }

    PackageMap.resolveThenInfer(Predef.withPredefA(("predef", LocationMap("")), parsedPaths), Nil) match {
      case (dups, Validated.Valid(packMap)) if dups.isEmpty =>
        (packMap, mainPack)
      case other => sys.error(s"expected clean compilation: $other")
    }
  }

  def gauss(n: Int) = prepPackages(
      List(s"""
package Gauss

gauss$n = range($n).foldLeft(0, add)
"""), "Gauss")

  val compiled0: (PackageMap.Inferred, PackageName) =
    gauss(10)

  @Benchmark def bench0(): Unit = {
    val c = compiled0
    val ev = Evaluation(c._1, Predef.jvmExternals[Const[Unit, ?]])
    // run the evaluation
    val res = ev.evaluateLast(c._2).get._1.value
    ()
  }

  val compiled1: (PackageMap.Inferred, PackageName) =
    gauss(20)

  @Benchmark def bench1(): Unit = {
    val c = compiled1
    val ev = Evaluation(c._1, Predef.jvmExternals[Const[Unit, ?]])
    // run the evaluation
    val res = ev.evaluateLast(c._2).get._1.value
    ()
  }

  val compiled2 =
    prepPackages(List("""
package Euler4

def operator >(a, b):
  match a.cmp_Int(b):
    GT: True
    _ : False

operator - = sub

# given a maximum value, and a function to Option[Int], return
# the maximum value of the function for inputs greater than 0
# if the starting number is <= 0, we return None
def max_of(n, fn):
  int_loop(n, None, \i, res ->
    next_i = i.sub(1)
    res1 = match fn(i):
      None: res
      sm1 @ Some(m1):
        match res:
          None: sm1
          sm @ Some(m): sm1 if m1 > m else sm
    (next_i, res1))

# return the first defined value from largest to smallest
# of the given function, if it is defined
def first_of(n, fn):
  int_loop(n, None, \i, res ->
    match fn(i):
      None: (i - 1, None)
      nonNone: (0, nonNone))

def digit_list(n):
  rev_list = int_loop(n, [], \n, acc ->
    this_digit = n.mod_Int(10)
    next_acc = [this_digit, *acc]
    next_n = match n.div(10):
      None:
        # can't really happen because 10 is not zero
        n
      Some(next): next
    (next_n, next_acc))
  reverse(rev_list)

def is_palindrome(lst, eq_fn):
  (res, _) = lst.foldLeft((True, reverse(lst)), \res, item ->
    match res:
      (False, _): res
      (_, []):
        # can't really happen, the lists are the same length
        (False, [])
      (True, [h, *t]): (eq_fn(item, h), t))
  res

def num_is_palindrome(n):
  digits = digit_list(n)
  is_palindrome(digits, eq_Int)

def product_palindrome(n1, n2):
  prod = n1.times(n2)
  Some(prod) if num_is_palindrome(prod) else None

max_pal_opt = max_of(99, \n1 -> first_of(99, product_palindrome(n1)))

max_pal = match max_pal_opt:
  Some(m): m
  None: 0
"""), "Euler4")

  @Benchmark def bench2(): Unit = {
    val c = compiled2
    val ev = Evaluation(c._1, Predef.jvmExternals)
    // run the evaluation
    val res = ev.evaluateLast(c._2).get._1.value
    ()
  }
}
