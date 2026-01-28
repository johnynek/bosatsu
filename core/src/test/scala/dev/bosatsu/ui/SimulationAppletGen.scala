package dev.bosatsu.ui

import org.scalacheck.{Arbitrary, Gen}
import SimulationApplet._

/**
 * ScalaCheck generators for simulation applet types.
 *
 * These generators enable property-based testing of:
 * - Derivation chain correctness
 * - "Why?" explanation generation
 * - "What if?" analysis
 * - Parameter sweep bounds
 */
object SimulationAppletGen {

  /**
   * Generate arbitrary assumption names.
   */
  val genAssumptionName: Gen[String] = Gen.oneOf(
    "gravity", "bounciness", "mass", "velocity", "angle",
    "friction", "elasticity", "temperature", "pressure", "volume"
  )

  /**
   * Generate an Int assumption.
   */
  val genIntAssumption: Gen[Assumption[Int]] = for {
    name <- genAssumptionName
    value <- Gen.choose(-1000, 1000)
    desc <- Gen.alphaNumStr.map(s => s.take(50))
  } yield Assumption(name, value, desc)

  /**
   * Generate a Double assumption.
   */
  val genDoubleAssumption: Gen[Assumption[Double]] = for {
    name <- genAssumptionName
    value <- Gen.choose(-1000.0, 1000.0)
    desc <- Gen.alphaNumStr.map(s => s.take(50))
  } yield Assumption(name, value, desc)

  /**
   * Generate a Boolean assumption.
   */
  val genBoolAssumption: Gen[Assumption[Boolean]] = for {
    name <- genAssumptionName
    value <- Gen.oneOf(true, false)
    desc <- Gen.alphaNumStr.map(s => s.take(50))
  } yield Assumption(name, value, desc)

  /**
   * Generate a String assumption.
   */
  val genStringAssumption: Gen[Assumption[String]] = for {
    name <- genAssumptionName
    value <- Gen.alphaNumStr.map(s => s.take(20))
    desc <- Gen.alphaNumStr.map(s => s.take(50))
  } yield Assumption(name, value, desc)

  /**
   * Generate arbitrary assumptions.
   */
  val genAssumption: Gen[Assumption[?]] = Gen.oneOf(
    genIntAssumption,
    genDoubleAssumption,
    genBoolAssumption,
    genStringAssumption
  )

  /**
   * Generate a computed derivation with given depth.
   */
  def genComputed(maxDepth: Int): Gen[Derivation[Int]] = {
    if (maxDepth <= 0) {
      genIntAssumption
    } else {
      Gen.frequency(
        (1, genIntAssumption),
        (2, for {
          name <- genAssumptionName.map(_ + "_computed")
          formula <- Gen.oneOf("a + b", "a * b", "a - b", "a / b", "max(a, b)", "min(a, b)")
          numDeps <- Gen.choose(1, 3)
          deps <- Gen.listOfN(numDeps, genComputed(maxDepth - 1))
          value <- Gen.choose(-1000, 1000)
        } yield Computed(name, value, formula, deps))
      )
    }
  }

  /**
   * Generate a conditional derivation.
   */
  val genConditional: Gen[Conditional[Int]] = for {
    name <- genAssumptionName.map(_ + "_conditional")
    condName <- Gen.alphaNumStr.map(s => s.take(10) + "_cond")
    condValue <- Arbitrary.arbitrary[Boolean]
    condAssumption = Assumption(condName, condValue, "condition")
    trueBranch <- Gen.alphaNumStr.map(s => "then: " + s.take(20))
    falseBranch <- Gen.alphaNumStr.map(s => "else: " + s.take(20))
    value <- Gen.choose(-1000, 1000)
    deps <- Gen.listOfN(2, genIntAssumption)
  } yield Conditional(name, value, condAssumption, condName, trueBranch, falseBranch, condAssumption :: deps)

  /**
   * Generate any derivation type.
   */
  def genDerivation(maxDepth: Int): Gen[Derivation[?]] = Gen.oneOf(
    genIntAssumption,
    genComputed(maxDepth),
    genConditional
  )

  /**
   * Generate a sweep configuration.
   */
  val genSweepConfig: Gen[ParameterSweep.SweepConfig[Double]] = for {
    name <- genAssumptionName
    min <- Gen.choose(-100.0, 0.0)
    max <- Gen.choose(1.0, 100.0)
    steps <- Gen.choose(2, 20)
    desc <- Gen.alphaNumStr.map(s => s.take(50))
  } yield ParameterSweep.SweepConfig(name, min, max, steps, desc)

  /**
   * Generate embed configuration.
   */
  val genEmbedConfig: Gen[EmbedGenerator.EmbedConfig] = for {
    title <- Gen.alphaNumStr.map(s => s.take(30))
    width <- Gen.choose(300, 1200)
    height <- Gen.choose(200, 800)
    theme <- Gen.oneOf(EmbedGenerator.LightTheme, EmbedGenerator.DarkTheme)
    showWhy <- Arbitrary.arbitrary[Boolean]
    showWhatIf <- Arbitrary.arbitrary[Boolean]
    showSweeps <- Arbitrary.arbitrary[Boolean]
    libraryMode <- Arbitrary.arbitrary[Boolean]
  } yield EmbedGenerator.EmbedConfig(
    title, width, height, theme, showWhy, showWhatIf, showSweeps, libraryMode
  )

  /**
   * Generate initial state maps.
   */
  val genInitialState: Gen[Map[String, String]] = for {
    size <- Gen.choose(1, 5)
    entries <- Gen.listOfN(size, for {
      name <- genAssumptionName
      value <- Gen.oneOf(
        Gen.choose(-1000, 1000).map(_.toString),
        Gen.choose(-1000.0, 1000.0).map(_.toString),
        Gen.oneOf("true", "false"),
        Gen.alphaNumStr.map(s => s""""${s.take(10)}"""")
      )
    } yield (name, value))
  } yield entries.toMap

  // Arbitrary instances
  implicit val arbAssumption: Arbitrary[Assumption[?]] = Arbitrary(genAssumption)
  implicit val arbDerivation: Arbitrary[Derivation[?]] = Arbitrary(genDerivation(3))
  implicit val arbSweepConfig: Arbitrary[ParameterSweep.SweepConfig[Double]] = Arbitrary(genSweepConfig)
  implicit val arbEmbedConfig: Arbitrary[EmbedGenerator.EmbedConfig] = Arbitrary(genEmbedConfig)
  implicit val arbInitialState: Arbitrary[Map[String, String]] = Arbitrary(genInitialState)
}
