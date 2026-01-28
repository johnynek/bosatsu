package dev.bosatsu.ui

import org.scalacheck.{Arbitrary, Gen}

/**
 * ScalaCheck generators for ReactiveState testing.
 */
object ReactiveStateGen {

  // Generator for state names
  val genStateName: Gen[String] = for {
    first <- Gen.alphaChar
    len <- Gen.choose(0, 10)
    rest <- Gen.listOfN(len, Gen.alphaNumChar)
  } yield (first :: rest).mkString

  // Generator for integer state values
  val genIntState: Gen[Int] = Gen.chooseNum(-10000, 10000)

  // Generator for string state values
  val genStringState: Gen[String] = Gen.alphaNumStr.map(_.take(50))

  // Generator for boolean state values
  val genBoolState: Gen[Boolean] = Gen.oneOf(true, false)

  // Generator for state update functions (Int -> Int)
  val genIntUpdate: Gen[Int => Int] = Gen.oneOf(
    Gen.const((x: Int) => x + 1),
    Gen.const((x: Int) => x - 1),
    Gen.const((x: Int) => x * 2),
    Gen.const((x: Int) => x / 2),
    Gen.const((x: Int) => -x),
    Gen.const((x: Int) => 0),
    Gen.chooseNum(-100, 100).map(n => (x: Int) => x + n)
  )

  // Generator for a sequence of state updates
  def genUpdateSequence(length: Int): Gen[List[Int]] =
    Gen.listOfN(length, genIntState)

  // Generator for StateBinding.TextBinding
  val genTextBinding: Gen[StateBinding.TextBinding] = for {
    stateVar <- genStateName
    elementId <- genStateName.map(_ + "-element")
    hasTransform <- Gen.oneOf(true, false)
    transform <- if (hasTransform) Gen.alphaStr.map(Some(_)) else Gen.const(None)
  } yield StateBinding.TextBinding(stateVar, elementId, transform)

  // Generator for StateBinding.InputBinding
  val genInputBinding: Gen[StateBinding.InputBinding] = for {
    elementId <- genStateName.map(_ + "-input")
    stateVar <- genStateName
    eventType <- Gen.oneOf("input", "change", "blur")
    hasTransform <- Gen.oneOf(true, false)
    transform <- if (hasTransform) Gen.alphaStr.map(Some(_)) else Gen.const(None)
  } yield StateBinding.InputBinding(elementId, stateVar, eventType, transform)

  // Arbitrary instances
  implicit val arbIntState: Arbitrary[Int] = Arbitrary(genIntState)
  implicit val arbStringState: Arbitrary[String] = Arbitrary(genStringState)
  implicit val arbTextBinding: Arbitrary[StateBinding.TextBinding] = Arbitrary(genTextBinding)
  implicit val arbInputBinding: Arbitrary[StateBinding.InputBinding] = Arbitrary(genInputBinding)
}
