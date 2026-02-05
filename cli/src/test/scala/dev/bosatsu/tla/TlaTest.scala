package dev.bosatsu.tla

import dev.bosatsu.service.{ServiceAnalysis, ServiceOperation, OperationKind, BatchGroup}
import io.circe.syntax._
import io.circe.parser.decode

class TlaTest extends munit.FunSuite {

  // ==========================================================================
  // TlaJson tests - encoder/decoder roundtrips
  // ==========================================================================
  import TlaJson.given

  test("TlaJson - TlaInt roundtrip") {
    val v: TlaValue = TlaValue.TlaInt(42)
    val json = v.asJson
    val decoded = decode[TlaValue](json.noSpaces)
    assertEquals(decoded, Right(v))
  }

  test("TlaJson - TlaString roundtrip") {
    val v: TlaValue = TlaValue.TlaString("hello world")
    val json = v.asJson
    val decoded = decode[TlaValue](json.noSpaces)
    assertEquals(decoded, Right(v))
  }

  test("TlaJson - TlaBool true roundtrip") {
    val v: TlaValue = TlaValue.TlaBool(true)
    val json = v.asJson
    val decoded = decode[TlaValue](json.noSpaces)
    assertEquals(decoded, Right(v))
  }

  test("TlaJson - TlaBool false roundtrip") {
    val v: TlaValue = TlaValue.TlaBool(false)
    val json = v.asJson
    val decoded = decode[TlaValue](json.noSpaces)
    assertEquals(decoded, Right(v))
  }

  test("TlaJson - TlaSeq roundtrip") {
    val v: TlaValue = TlaValue.TlaSeq(List(TlaValue.TlaInt(1), TlaValue.TlaInt(2), TlaValue.TlaInt(3)))
    val json = v.asJson
    val decoded = decode[TlaValue](json.noSpaces)
    assertEquals(decoded, Right(v))
  }

  test("TlaJson - TlaSet roundtrip") {
    val v: TlaValue = TlaValue.TlaSet(Set(TlaValue.TlaInt(1), TlaValue.TlaInt(2)))
    val json = v.asJson
    val decoded = decode[TlaValue](json.noSpaces)
    assertEquals(decoded, Right(v))
  }

  test("TlaJson - TlaRecord roundtrip") {
    val v: TlaValue = TlaValue.TlaRecord(Map("x" -> TlaValue.TlaInt(1), "y" -> TlaValue.TlaString("test")))
    val json = v.asJson
    val decoded = decode[TlaValue](json.noSpaces)
    assertEquals(decoded, Right(v))
  }

  test("TlaJson - TlaFunction roundtrip") {
    val v: TlaValue = TlaValue.TlaFunction("x \\in 1..10", "x * 2")
    val json = v.asJson
    val decoded = decode[TlaValue](json.noSpaces)
    assertEquals(decoded, Right(v))
  }

  test("TlaJson - nested TlaValue roundtrip") {
    val v: TlaValue = TlaValue.TlaRecord(Map(
      "list" -> TlaValue.TlaSeq(List(TlaValue.TlaInt(1), TlaValue.TlaInt(2))),
      "set" -> TlaValue.TlaSet(Set(TlaValue.TlaBool(true))),
      "nested" -> TlaValue.TlaRecord(Map("inner" -> TlaValue.TlaString("deep")))
    ))
    val json = v.asJson
    val decoded = decode[TlaValue](json.noSpaces)
    assertEquals(decoded, Right(v))
  }

  test("TlaJson - TlaValue decode error for unknown type") {
    val json = """{"type":"unknown","value":123}"""
    val decoded = decode[TlaValue](json)
    assert(decoded.isLeft)
  }

  test("TlaJson - TlaOptions roundtrip") {
    val opts = TlaOptions(
      initialState = Map("x" -> TlaValue.TlaInt(0), "y" -> TlaValue.TlaString("init")),
      invariant = Some("x >= 0"),
      stateVariable = "myState",
      checkDeadlock = false
    )
    val json = opts.asJson
    val decoded = decode[TlaOptions](json.noSpaces)
    assertEquals(decoded, Right(opts))
  }

  test("TlaJson - TlaOptions with defaults roundtrip") {
    val opts = TlaOptions()
    val json = opts.asJson
    val decoded = decode[TlaOptions](json.noSpaces)
    assertEquals(decoded.map(_.stateVariable), Right("state"))
    assertEquals(decoded.map(_.checkDeadlock), Right(true))
  }

  test("TlaJson - TlaAction roundtrip") {
    val action = TlaAction("DoSomething", "x > 0", "x' = x - 1", "running", "done")
    val json = action.asJson
    val decoded = decode[TlaAction](json.noSpaces)
    assertEquals(decoded, Right(action))
  }

  test("TlaJson - TlaSpec encode") {
    val spec = TlaSpec(
      moduleName = "TestSpec",
      extends_ = List("Integers", "Sequences"),
      variables = List("x", "y"),
      init = "Init == x = 0 /\\ y = 0",
      actions = List(TlaAction("Inc", "TRUE", "x' = x + 1", "start", "end")),
      next = "Next == Inc",
      spec = "Spec == Init /\\ [][Next]_vars",
      invariants = List("x >= 0")
    )
    val json = spec.asJson
    assert(json.hcursor.downField("moduleName").as[String] == Right("TestSpec"))
    assert(json.hcursor.downField("extends").as[List[String]] == Right(List("Integers", "Sequences")))
  }

  test("TlaJson - TlcTraceState roundtrip") {
    val state = TlcTraceState(1, "Init", "x = 0 /\\ y = 1")
    val json = state.asJson
    val decoded = decode[TlcTraceState](json.noSpaces)
    assertEquals(decoded, Right(state))
  }

  test("TlaJson - TlcResult roundtrip - success") {
    val result = TlcResult(
      success = true,
      statesGenerated = Some(100),
      distinctStates = Some(50)
    )
    val json = result.asJson
    val decoded = decode[TlcResult](json.noSpaces)
    assertEquals(decoded.map(_.success), Right(true))
    assertEquals(decoded.map(_.statesGenerated), Right(Some(100)))
  }

  test("TlaJson - TlcResult roundtrip - failure with trace") {
    val trace = List(
      TlcTraceState(1, "Init", "x = 0"),
      TlcTraceState(2, "Inc", "x = 1"),
      TlcTraceState(3, "Inc", "x = 2")
    )
    val result = TlcResult(
      success = false,
      invariantViolation = true,
      errorMessage = Some("Invariant violated"),
      errorTrace = trace
    )
    val json = result.asJson
    val decoded = decode[TlcResult](json.noSpaces)
    assertEquals(decoded.map(_.success), Right(false))
    assertEquals(decoded.map(_.invariantViolation), Right(true))
    assertEquals(decoded.map(_.errorTrace.length), Right(3))
  }

  test("TlaJson - TlcResult roundtrip - deadlock") {
    val result = TlcResult(success = false, deadlock = true)
    val json = result.asJson
    val decoded = decode[TlcResult](json.noSpaces)
    assertEquals(decoded.map(_.deadlock), Right(true))
  }

  test("TlaJson - TlcResult roundtrip - skipped") {
    val result = TlcResult(success = true, skipped = true, skipReason = Some("TLC not available"))
    val json = result.asJson
    val decoded = decode[TlcResult](json.noSpaces)
    assertEquals(decoded.map(_.skipped), Right(true))
    assertEquals(decoded.map(_.skipReason), Right(Some("TLC not available")))
  }

  test("TlaJson - TlcOptions roundtrip") {
    val opts = TlcOptions(
      workers = 4,
      checkDeadlock = false,
      depth = Some(100),
      timeout = Some(60),
      skipIfUnavailable = false
    )
    val json = opts.asJson
    val decoded = decode[TlcOptions](json.noSpaces)
    assertEquals(decoded.map(_.workers), Right(4))
    assertEquals(decoded.map(_.checkDeadlock), Right(false))
    assertEquals(decoded.map(_.depth), Right(Some(100)))
  }

  test("TlaJson - TlcOptions with defaults roundtrip") {
    val opts = TlcOptions()
    val json = opts.asJson
    val decoded = decode[TlcOptions](json.noSpaces)
    assertEquals(decoded.map(_.workers), Right(1))
    assertEquals(decoded.map(_.skipIfUnavailable), Right(true))
  }

  test("TlaJson - RaceViolation encode") {
    val violation = RaceViolation(
      trace = List("Init: x = 0", "Step: x = -1"),
      finalState = Map("x" -> TlaValue.TlaInt(-1))
    )
    val json = violation.asJson
    assert(json.hcursor.downField("trace").as[List[String]].isRight)
    assert(json.hcursor.downField("finalState").as[Map[String, TlaValue]].isRight)
  }

  test("TlaJson - TlaGenResult encode") {
    val result = TlaGenResult(
      file = "test.bosatsu",
      tlaSpec = "---- MODULE test ----\n====",
      moduleName = "test",
      instances = 2,
      invariant = Some("x >= 0"),
      tlcResult = Some(TlcResult(success = true))
    )
    val json = result.asJson
    assert(json.hcursor.downField("file").as[String] == Right("test.bosatsu"))
    assert(json.hcursor.downField("instances").as[Int] == Right(2))
  }

  test("TlaJson - RaceAnalysisResult encode") {
    val result = RaceAnalysisResult(
      file = "handler.bosatsu",
      handlerName = "update",
      instances = 3,
      totalInterleavings = 100,
      violatingInterleavings = 5,
      invariant = "balance >= 0",
      exampleViolation = None
    )
    val json = result.asJson
    assert(json.hcursor.downField("handlerName").as[String] == Right("update"))
    assert(json.hcursor.downField("violatingInterleavings").as[Int] == Right(5))
  }

  // ==========================================================================
  // TlaValue tests
  // ==========================================================================

  test("TlaValue.TlaInt - render") {
    val int = TlaValue.TlaInt(42)
    assertEquals(int.render, "42")
  }

  test("TlaValue.TlaString - render") {
    val str = TlaValue.TlaString("hello")
    assertEquals(str.render, "\"hello\"")
  }

  test("TlaValue.TlaBool - render TRUE") {
    val bool = TlaValue.TlaBool(true)
    assertEquals(bool.render, "TRUE")
  }

  test("TlaValue.TlaBool - render FALSE") {
    val bool = TlaValue.TlaBool(false)
    assertEquals(bool.render, "FALSE")
  }

  test("TlaValue.TlaSeq - render") {
    val seq = TlaValue.TlaSeq(List(TlaValue.TlaInt(1), TlaValue.TlaInt(2)))
    assertEquals(seq.render, "<<1, 2>>")
  }

  test("TlaValue.TlaSet - render") {
    val set = TlaValue.TlaSet(Set(TlaValue.TlaInt(1), TlaValue.TlaInt(2)))
    assert(set.render.startsWith("{"))
    assert(set.render.endsWith("}"))
    assert(set.render.contains("1"))
    assert(set.render.contains("2"))
  }

  test("TlaValue.TlaRecord - render") {
    val record = TlaValue.TlaRecord(Map("x" -> TlaValue.TlaInt(1)))
    assertEquals(record.render, "[x |-> 1]")
  }

  test("TlaValue.TlaFunction - render") {
    val fn = TlaValue.TlaFunction("x \\in 1..5", "x * 2")
    assertEquals(fn.render, "[x \\in 1..5 |-> x * 2]")
  }

  test("TlaValue.fromAny - Int") {
    val result = TlaValue.fromAny(42)
    assertEquals(result.render, "42")
  }

  test("TlaValue.fromAny - Long") {
    val result = TlaValue.fromAny(100L)
    assertEquals(result.render, "100")
  }

  test("TlaValue.fromAny - String") {
    val result = TlaValue.fromAny("test")
    assertEquals(result.render, "\"test\"")
  }

  test("TlaValue.fromAny - Boolean") {
    val result = TlaValue.fromAny(true)
    assertEquals(result.render, "TRUE")
  }

  test("TlaValue.fromAny - Seq") {
    val result = TlaValue.fromAny(Seq(1, 2, 3))
    assertEquals(result.render, "<<1, 2, 3>>")
  }

  test("TlaValue.fromAny - Set") {
    val result = TlaValue.fromAny(Set(1))
    assertEquals(result.render, "{1}")
  }

  test("TlaValue.fromAny - Map") {
    val result = TlaValue.fromAny(Map("x" -> 1, "y" -> 2))
    assert(result.isInstanceOf[TlaValue.TlaRecord])
    val record = result.asInstanceOf[TlaValue.TlaRecord]
    assertEquals(record.fields.size, 2)
  }

  test("TlaValue.fromAny - unknown type falls back to String") {
    case class Custom(x: Int)
    val result = TlaValue.fromAny(Custom(42))
    assertEquals(result.render, "\"Custom(42)\"")
  }

  // ==========================================================================
  // TlaCommand tests
  // ==========================================================================

  test("TlaCommand.Generate - default values") {
    val cmd = TlaCommand.Generate("test.bosatsu")
    assertEquals(cmd.file, "test.bosatsu")
    assertEquals(cmd.output, None)
    assertEquals(cmd.instances, 1)
    assertEquals(cmd.invariant, None)
  }

  test("TlaCommand.Generate - with all options") {
    val cmd = TlaCommand.Generate("test.bosatsu", Some("out.tla"), 3, Some("x >= 0"))
    assertEquals(cmd.file, "test.bosatsu")
    assertEquals(cmd.output, Some("out.tla"))
    assertEquals(cmd.instances, 3)
    assertEquals(cmd.invariant, Some("x >= 0"))
  }

  test("TlaCommand.Check - default values") {
    val cmd = TlaCommand.Check("test.tla")
    assertEquals(cmd.file, "test.tla")
    assertEquals(cmd.workers, 1)
    assertEquals(cmd.depth, None)
    assertEquals(cmd.timeout, None)
  }

  test("TlaCommand.Check - with all options") {
    val cmd = TlaCommand.Check("test.tla", 4, Some(100), Some(30000))
    assertEquals(cmd.workers, 4)
    assertEquals(cmd.depth, Some(100))
    assertEquals(cmd.timeout, Some(30000))
  }

  test("TlaCommand.Race - default values") {
    val cmd = TlaCommand.Race("handler.bosatsu")
    assertEquals(cmd.file, "handler.bosatsu")
    assertEquals(cmd.instances, 2)
    assertEquals(cmd.invariant, None)
  }

  test("TlaCommand.Race - with all options") {
    val cmd = TlaCommand.Race("handler.bosatsu", 5, Some("balance >= 0"))
    assertEquals(cmd.instances, 5)
    assertEquals(cmd.invariant, Some("balance >= 0"))
  }

  test("TlaCommand - equality") {
    val cmd1: TlaCommand = TlaCommand.Generate("a.bosatsu")
    val cmd2: TlaCommand = TlaCommand.Generate("a.bosatsu")
    val cmd3: TlaCommand = TlaCommand.Check("a.bosatsu")

    assertEquals(cmd1, cmd2)
    assertNotEquals(cmd1, cmd3)
  }

  test("TlaGenResult - construction") {
    val result = TlaGenResult(
      file = "test.bosatsu",
      tlaSpec = "---- MODULE Test ----\n====",
      moduleName = "Test",
      instances = 2,
      invariant = Some("x >= 0"),
      tlcResult = Some(TlcResult(success = true))
    )
    assertEquals(result.file, "test.bosatsu")
    assertEquals(result.moduleName, "Test")
    assertEquals(result.instances, 2)
    assert(result.tlcResult.isDefined)
  }

  test("RaceAnalysisResult - construction") {
    val result = RaceAnalysisResult(
      file = "handler.bosatsu",
      handlerName = "update",
      instances = 3,
      totalInterleavings = 100,
      violatingInterleavings = 5,
      invariant = "balance >= 0",
      exampleViolation = Some(RaceViolation(List("step1", "step2"), Map("x" -> TlaValue.TlaInt(-1))))
    )
    assertEquals(result.handlerName, "update")
    assertEquals(result.violatingInterleavings, 5)
    assert(result.exampleViolation.isDefined)
  }

  test("TlcResult - with all error types") {
    val syntaxErr = TlcResult(success = false, syntaxError = true, errorMessage = Some("parse error"))
    assertEquals(syntaxErr.syntaxError, true)
    assertEquals(syntaxErr.errorMessage, Some("parse error"))

    val invariantErr = TlcResult(success = false, invariantViolation = true)
    assertEquals(invariantErr.invariantViolation, true)

    val deadlockErr = TlcResult(success = false, deadlock = true)
    assertEquals(deadlockErr.deadlock, true)

    val temporalErr = TlcResult(success = false, temporalPropertyViolation = true)
    assertEquals(temporalErr.temporalPropertyViolation, true)

    val skipped = TlcResult(success = true, skipped = true, skipReason = Some("TLC not available"))
    assertEquals(skipped.skipped, true)
    assertEquals(skipped.skipReason, Some("TLC not available"))
  }

  test("TlcResult - with raw output") {
    val result = TlcResult(
      success = true,
      statesGenerated = Some(1000),
      distinctStates = Some(500),
      rawOutput = Some("TLC output here...")
    )
    assertEquals(result.rawOutput, Some("TLC output here..."))
    assertEquals(result.statesGenerated, Some(1000))
    assertEquals(result.distinctStates, Some(500))
  }

  test("TlaOptions - default values") {
    val opts = TlaOptions()
    assertEquals(opts.stateVariable, "state")
    assertEquals(opts.checkDeadlock, true)
    assertEquals(opts.invariant, None)
    assertEquals(opts.initialState, Map.empty)
  }

  test("TlaAction - default multiInstance is false") {
    val action = TlaAction("Test", "TRUE", "UNCHANGED state", "start", "done")
    assertEquals(action.multiInstance, false)
  }

  test("TlaAction - with multiInstance true") {
    val action = TlaAction("Test", "TRUE", "UNCHANGED state", "start", "done", multiInstance = true)
    assertEquals(action.multiInstance, true)
  }

  test("TlaGen.generate - empty operations") {
    val analysis = ServiceAnalysis(
      handlerName = "test_handler",
      sourceFile = "test.bosatsu",
      operations = Nil,
      batchGroups = Nil,
      canBatch = false,
      totalQueries = 0,
      batchedQueries = 0,
      queriesSaved = 0
    )
    val options = TlaOptions()
    val spec = TlaGen.generate(analysis, options, 1)

    assertEquals(spec.moduleName, "test_handler")
    assert(spec.actions.nonEmpty)
    assertEquals(spec.actions.head.name, "Complete")
  }

  test("TlaGen.generate - with read operations") {
    val analysis = ServiceAnalysis(
      handlerName = "reader",
      sourceFile = "test.bosatsu",
      operations = List(
        ServiceOperation("DB", "get", OperationKind.Read, false, None)
      ),
      batchGroups = Nil,
      canBatch = false,
      totalQueries = 1,
      batchedQueries = 0,
      queriesSaved = 0
    )
    val options = TlaOptions()
    val spec = TlaGen.generate(analysis, options, 1)

    // Should have read action + Complete action
    assert(spec.actions.exists(_.name.startsWith("Read_")))
    assert(spec.actions.exists(_.name == "Complete"))
  }

  test("TlaGen.generate - with write operations") {
    val analysis = ServiceAnalysis(
      handlerName = "writer",
      sourceFile = "test.bosatsu",
      operations = List(
        ServiceOperation("DB", "set", OperationKind.Write, false, None)
      ),
      batchGroups = Nil,
      canBatch = false,
      totalQueries = 1,
      batchedQueries = 0,
      queriesSaved = 0
    )
    val options = TlaOptions()
    val spec = TlaGen.generate(analysis, options, 1)

    assert(spec.actions.exists(_.name.startsWith("Write_")))
    // Last write transitions to done
    val writeAction = spec.actions.find(_.name.startsWith("Write_")).get
    assertEquals(writeAction.pcTo, "done")
  }

  test("TlaGen.generate - multi-instance") {
    val analysis = ServiceAnalysis(
      handlerName = "concurrent_handler",
      sourceFile = "test.bosatsu",
      operations = List(
        ServiceOperation("DB", "get", OperationKind.Read, false, None)
      ),
      batchGroups = Nil,
      canBatch = false,
      totalQueries = 1,
      batchedQueries = 0,
      queriesSaved = 0
    )
    val options = TlaOptions()
    val spec = TlaGen.generate(analysis, options, instances = 2)

    // Actions should have multiInstance=true
    assert(spec.actions.forall(_.multiInstance == true))
    assert(spec.variables.contains("instance"))
  }

  test("TlaGen.generateRaceSpec") {
    val analysis = ServiceAnalysis(
      handlerName = "race_test",
      sourceFile = "test.bosatsu",
      operations = List(
        ServiceOperation("DB", "get", OperationKind.Read, false, None),
        ServiceOperation("DB", "set", OperationKind.Write, false, None)
      ),
      batchGroups = Nil,
      canBatch = false,
      totalQueries = 2,
      batchedQueries = 0,
      queriesSaved = 0
    )

    val spec = TlaGen.generateRaceSpec(analysis, instances = 2, invariant = "state[\"x\"] >= 0")

    assertEquals(spec.moduleName, "race_test")
    assert(spec.invariants.nonEmpty)
    assertEquals(spec.invariants.head, "state[\"x\"] >= 0")
  }

  test("TlaSpec.render - single instance") {
    val spec = TlaSpec(
      moduleName = "TestModule",
      extends_ = List("Integers"),
      variables = List("state", "pc"),
      init = "Init == state = 0 /\\ pc = \"start\"",
      actions = List(
        TlaAction("Step", "TRUE", "state' = state + 1", "start", "done")
      ),
      next = "Next == Step \\/ Done",
      spec = "Spec == Init /\\ [][Next]_vars"
    )

    val rendered = spec.render
    assert(rendered.contains("---- MODULE TestModule ----"))
    assert(rendered.contains("EXTENDS Integers"))
    assert(rendered.contains("VARIABLES state, pc"))
    assert(rendered.contains("Step =="))
    assert(rendered.contains("pc = \"start\""))
    assert(rendered.contains("pc' = \"done\""))
    assert(rendered.contains("Done =="))
    assert(rendered.contains("===="))
  }

  test("TlaSpec.render - multi-instance with parameterized actions") {
    val spec = TlaSpec(
      moduleName = "MultiTest",
      extends_ = List("Integers"),
      variables = List("state", "pc", "instance"),
      init = "Init == state = 0 /\\ pc = [i \\in 1..2 |-> \"start\"]",
      actions = List(
        TlaAction("Step", "TRUE", "state' = state + 1", "start", "done", multiInstance = true)
      ),
      next = "Next == \\E self \\in 1..2: Step(self) \\/ Done",
      spec = "Spec == Init /\\ [][Next]_vars"
    )

    val rendered = spec.render
    assert(rendered.contains("Step(self) =="))
    assert(rendered.contains("pc[self] = \"start\""))
    assert(rendered.contains("pc' = [pc EXCEPT ![self] = \"done\"]"))
  }

  test("TlaGen.generateConfig") {
    val spec = TlaSpec(
      moduleName = "TestModule",
      extends_ = List("Integers"),
      variables = List("state", "pc"),
      init = "Init == TRUE",
      actions = Nil,
      next = "Next == TRUE",
      spec = "Spec == Init /\\ [][Next]_vars",
      invariants = List("state >= 0", "pc \\in {\"start\", \"done\"}")
    )

    val options = TlcOptions(checkDeadlock = true)
    val config = TlaGen.generateConfig(spec, options)

    assert(config.contains("SPECIFICATION Spec"))
    assert(config.contains("INVARIANT Inv0"))
    assert(config.contains("INVARIANT Inv1"))
    assert(!config.contains("CHECK_DEADLOCK FALSE"))
  }

  test("TlaGen.generateConfig - no deadlock check") {
    val spec = TlaSpec(
      moduleName = "TestModule",
      extends_ = Nil,
      variables = Nil,
      init = "",
      actions = Nil,
      next = "",
      spec = "",
      invariants = Nil
    )

    val options = TlcOptions(checkDeadlock = false)
    val config = TlaGen.generateConfig(spec, options)

    assert(config.contains("CHECK_DEADLOCK FALSE"))
  }

  test("TlcResult - default values") {
    val result = TlcResult(success = true)
    assertEquals(result.invariantViolation, false)
    assertEquals(result.deadlock, false)
    assertEquals(result.skipped, false)
  }

  test("TlcOptions - default values") {
    val options = TlcOptions()
    assertEquals(options.workers, 1)
    assertEquals(options.checkDeadlock, true)
    assertEquals(options.depth, None)
    assertEquals(options.skipIfUnavailable, true)
  }

  test("TlcTraceState") {
    val state = TlcTraceState(1, "Init", "x = 0")
    assertEquals(state.stateNumber, 1)
    assertEquals(state.action, "Init")
    assertEquals(state.variables, "x = 0")
  }

  // ==========================================================================
  // TlaGen additional tests
  // ==========================================================================

  test("TlaGen.generate - with unknown operations") {
    val analysis = ServiceAnalysis(
      handlerName = "unknown_ops",
      sourceFile = "test.bosatsu",
      operations = List(
        ServiceOperation("API", "call", OperationKind.Unknown, false, None)
      ),
      batchGroups = Nil,
      canBatch = false,
      totalQueries = 1,
      batchedQueries = 0,
      queriesSaved = 0
    )
    val options = TlaOptions()
    val spec = TlaGen.generate(analysis, options, 1)

    // Unknown operations should still generate an action
    assert(spec.actions.nonEmpty)
  }

  test("TlaGen.generate - with mixed read/write operations") {
    val analysis = ServiceAnalysis(
      handlerName = "mixed",
      sourceFile = "test.bosatsu",
      operations = List(
        ServiceOperation("DB", "get", OperationKind.Read, false, None),
        ServiceOperation("DB", "get", OperationKind.Read, false, None),
        ServiceOperation("DB", "set", OperationKind.Write, false, None)
      ),
      batchGroups = Nil,
      canBatch = false,
      totalQueries = 3,
      batchedQueries = 0,
      queriesSaved = 0
    )
    val options = TlaOptions()
    val spec = TlaGen.generate(analysis, options, 1)

    // Should have multiple actions
    assert(spec.actions.size >= 3)
  }

  test("TlaGen.generate - with custom invariant") {
    val analysis = ServiceAnalysis(
      handlerName = "inv_test",
      sourceFile = "test.bosatsu",
      operations = Nil,
      batchGroups = Nil,
      canBatch = false,
      totalQueries = 0,
      batchedQueries = 0,
      queriesSaved = 0
    )
    val options = TlaOptions(invariant = Some("state[\"count\"] >= 0"))
    val spec = TlaGen.generate(analysis, options, 1)

    assert(spec.invariants.contains("state[\"count\"] >= 0"))
  }

  test("TlaGen.generate - with initial state") {
    val analysis = ServiceAnalysis(
      handlerName = "init_test",
      sourceFile = "test.bosatsu",
      operations = Nil,
      batchGroups = Nil,
      canBatch = false,
      totalQueries = 0,
      batchedQueries = 0,
      queriesSaved = 0
    )
    val options = TlaOptions(initialState = Map(
      "x" -> TlaValue.TlaInt(0),
      "y" -> TlaValue.TlaString("init")
    ))
    val spec = TlaGen.generate(analysis, options, 1)

    assert(spec.init.contains("x"))
  }

  test("TlaGen.sanitizeName") {
    // Test via generate - names with special chars should be sanitized
    val analysis = ServiceAnalysis(
      handlerName = "test-handler_v2",
      sourceFile = "test.bosatsu",
      operations = Nil,
      batchGroups = Nil,
      canBatch = false,
      totalQueries = 0,
      batchedQueries = 0,
      queriesSaved = 0
    )
    val options = TlaOptions()
    val spec = TlaGen.generate(analysis, options, 1)
    // Module name should be sanitized (hyphens replaced)
    assert(!spec.moduleName.contains("-"))
  }

  test("TlaSpec.render - with invariants") {
    val spec = TlaSpec(
      moduleName = "InvTest",
      extends_ = List("Integers"),
      variables = List("x"),
      init = "Init == x = 0",
      actions = Nil,
      next = "Next == TRUE",
      spec = "Spec == Init /\\ [][Next]_vars",
      invariants = List("x >= 0", "x < 100")
    )

    val rendered = spec.render
    assert(rendered.contains("Inv0 == x >= 0"))
    assert(rendered.contains("Inv1 == x < 100"))
  }

  test("TlaSpec.render - empty extends") {
    val spec = TlaSpec(
      moduleName = "NoExtends",
      extends_ = Nil,
      variables = List("x"),
      init = "Init == x = 0",
      actions = Nil,
      next = "Next == TRUE",
      spec = "Spec == Init"
    )

    val rendered = spec.render
    // Should not have EXTENDS line when empty
    assert(!rendered.contains("EXTENDS"))
  }

  // ==========================================================================
  // TlaProtocol additional tests
  // ==========================================================================

  test("TlaGenResult - equality") {
    val r1 = TlaGenResult("file.bosatsu", "spec", "mod", 1, None, None)
    val r2 = TlaGenResult("file.bosatsu", "spec", "mod", 1, None, None)
    val r3 = TlaGenResult("other.bosatsu", "spec", "mod", 1, None, None)

    assertEquals(r1, r2)
    assertNotEquals(r1, r3)
  }

  test("RaceAnalysisResult - equality") {
    val r1 = RaceAnalysisResult("f", "h", 2, 10, 0, "inv", None)
    val r2 = RaceAnalysisResult("f", "h", 2, 10, 0, "inv", None)
    val r3 = RaceAnalysisResult("f", "h", 2, 10, 1, "inv", None)

    assertEquals(r1, r2)
    assertNotEquals(r1, r3)
  }

  test("RaceViolation - equality") {
    val v1 = RaceViolation(List("Init: x=0"), Map("x" -> TlaValue.TlaInt(-1)))
    val v2 = RaceViolation(List("Init: x=0"), Map("x" -> TlaValue.TlaInt(-1)))
    val v3 = RaceViolation(List("Init: x=0"), Map("x" -> TlaValue.TlaInt(0)))

    assertEquals(v1, v2)
    assertNotEquals(v1, v3)
  }

  test("TlcResult - with all fields") {
    val result = TlcResult(
      success = false,
      invariantViolation = true,
      deadlock = false,
      temporalPropertyViolation = false,
      syntaxError = false,
      errorMessage = Some("Invariant Inv0 violated"),
      statesGenerated = Some(1000),
      distinctStates = Some(500),
      errorTrace = List(TlcTraceState(1, "Init", "x=0"), TlcTraceState(2, "Bad", "x=-1")),
      skipped = false,
      skipReason = None
    )

    assertEquals(result.success, false)
    assertEquals(result.invariantViolation, true)
    assertEquals(result.errorTrace.length, 2)
  }

  test("TlcOptions - with depth and timeout") {
    val opts = TlcOptions(
      workers = 8,
      checkDeadlock = true,
      depth = Some(50),
      timeout = Some(120),
      skipIfUnavailable = false
    )

    assertEquals(opts.workers, 8)
    assertEquals(opts.depth, Some(50))
    assertEquals(opts.timeout, Some(120))
    assertEquals(opts.skipIfUnavailable, false)
  }

  test("TlaValue.fromAny - unknown type returns string") {
    case class Custom(x: Int)
    val result = TlaValue.fromAny(Custom(42))
    assert(result.isInstanceOf[TlaValue.TlaString])
  }

  test("TlaValue.TlaRecord - empty") {
    val record = TlaValue.TlaRecord(Map.empty)
    assertEquals(record.render, "[]")
  }

  test("TlaValue.TlaSeq - empty") {
    val seq = TlaValue.TlaSeq(Nil)
    assertEquals(seq.render, "<<>>")
  }

  test("TlaValue.TlaSet - empty") {
    val set = TlaValue.TlaSet(Set.empty)
    assertEquals(set.render, "{}")
  }
}
