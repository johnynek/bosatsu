package dev.bosatsu.tla

import dev.bosatsu.service.{ServiceAnalysis, ServiceOperation, OperationKind, BatchGroup}

class TlaTest extends munit.FunSuite {

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
}
