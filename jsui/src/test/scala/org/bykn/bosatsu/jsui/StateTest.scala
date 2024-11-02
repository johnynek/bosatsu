package org.bykn.bosatsu.jsui

import org.scalacheck.{Gen, Prop}
import scala.concurrent.duration.Duration

class StateTest extends munit.ScalaCheckSuite {

  val genState: Gen[State] = {
    val genWithText: Gen[State.HasText] =
      Gen.oneOf(
        Gen.asciiStr.map(State.WithText(_)),
        Gen
          .zip(
            Gen.asciiStr,
            Gen.asciiStr,
            Gen.choose(0L, 1L << 10).map(Duration(_, "millis"))
          )
          .map { case (a, b, c) => State.Compiled(a, b, c) }
      )

    Gen.oneOf(
      Gen.const(State.Init),
      genWithText,
      genWithText.map(State.Compiling(_))
    )
  }

  property("json encoding/decoding works") {
    Prop.forAll(genState) { state =>
      val str = State.stateToJsonString(state)
      assertEquals(
        State.stringToState(str),
        Right(state),
        s"encoded $state to $str"
      )
    }
  }
}
