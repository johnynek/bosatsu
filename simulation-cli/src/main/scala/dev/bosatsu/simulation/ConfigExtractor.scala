package dev.bosatsu.simulation

import dev.bosatsu.Value
import dev.bosatsu.Value.{ProductValue, VInt, Str, VList}

/**
 * Extracts SimConfig values from Bosatsu Value representations.
 *
 * The config file defines structs like:
 *   struct InputConfig(label: String, default_value: Int, ...)
 *
 * These are represented at runtime as ProductValue with fields
 * accessed by position (matching struct definition order).
 */
object ConfigExtractor {

  case class InputConfig(
    label: String,
    defaultValue: Int,
    minValue: Int,
    maxValue: Int,
    step: Int,
    widget: String
  )

  case class OutputConfig(
    label: String,
    format: String,
    primary: Boolean
  )

  case class SimConfig(
    name: String,
    description: String,
    packageName: String,
    functionName: String,
    inputs: List[(String, InputConfig)],
    outputs: List[(String, OutputConfig)]
  )

  /**
   * Extract SimConfig from a Bosatsu Value.
   *
   * Expects a ProductValue matching the SimConfig struct:
   *   struct SimConfig(
   *     name: String,
   *     description: String,
   *     package_name: String,
   *     function_name: String,
   *     inputs: List[(String, InputConfig)],
   *     outputs: List[(String, OutputConfig)]
   *   )
   */
  def extractSimConfig(value: Value): SimConfig = {
    value match {
      case p: ProductValue if p.values.length >= 6 =>
        SimConfig(
          name = extractString(p.get(0)),
          description = extractString(p.get(1)),
          packageName = extractString(p.get(2)),
          functionName = extractString(p.get(3)),
          inputs = extractInputList(p.get(4)),
          outputs = extractOutputList(p.get(5))
        )
      case other =>
        throw new RuntimeException(s"Expected SimConfig struct (ProductValue with 6 fields), got: $other")
    }
  }

  private def extractString(v: Value): String = v match {
    case Str(s) => s
    case other => throw new RuntimeException(s"Expected String, got: $other")
  }

  private def extractInt(v: Value): Int = v match {
    case VInt(bi) => bi.intValue()
    case other => throw new RuntimeException(s"Expected Int, got: $other")
  }

  private def extractBool(v: Value): Boolean = v match {
    case Value.True  => true
    case Value.False => false
    case other => throw new RuntimeException(s"Expected Bool, got: $other")
  }

  /**
   * Extract a List[(String, InputConfig)] from a Bosatsu List value.
   * Each element is a tuple (ProductValue with 2 elements).
   */
  private def extractInputList(v: Value): List[(String, InputConfig)] = {
    VList.unapply(v) match {
      case Some(items) =>
        items.map { item =>
          val (name, configValue) = extractTuple2(item)
          (extractString(name), extractInputConfig(configValue))
        }
      case None =>
        throw new RuntimeException(s"Expected List for inputs, got: $v")
    }
  }

  /**
   * Extract a List[(String, OutputConfig)] from a Bosatsu List value.
   */
  private def extractOutputList(v: Value): List[(String, OutputConfig)] = {
    VList.unapply(v) match {
      case Some(items) =>
        items.map { item =>
          val (name, configValue) = extractTuple2(item)
          (extractString(name), extractOutputConfig(configValue))
        }
      case None =>
        throw new RuntimeException(s"Expected List for outputs, got: $v")
    }
  }

  /**
   * Extract a 2-tuple from a ProductValue.
   */
  private def extractTuple2(v: Value): (Value, Value) = v match {
    case p: ProductValue if p.values.length == 2 =>
      (p.get(0), p.get(1))
    case other =>
      throw new RuntimeException(s"Expected tuple (ProductValue with 2 elements), got: $other")
  }

  /**
   * Extract InputConfig from a ProductValue.
   *
   * Matches struct:
   *   struct InputConfig(
   *     label: String,
   *     default_value: Int,
   *     min_value: Int,
   *     max_value: Int,
   *     step: Int,
   *     widget: String
   *   )
   */
  private def extractInputConfig(v: Value): InputConfig = v match {
    case p: ProductValue if p.values.length >= 6 =>
      InputConfig(
        label = extractString(p.get(0)),
        defaultValue = extractInt(p.get(1)),
        minValue = extractInt(p.get(2)),
        maxValue = extractInt(p.get(3)),
        step = extractInt(p.get(4)),
        widget = extractString(p.get(5))
      )
    case other =>
      throw new RuntimeException(s"Expected InputConfig struct (ProductValue with 6 fields), got: $other")
  }

  /**
   * Extract OutputConfig from a ProductValue.
   *
   * Matches struct:
   *   struct OutputConfig(
   *     label: String,
   *     format: String,
   *     primary: Bool
   *   )
   */
  private def extractOutputConfig(v: Value): OutputConfig = v match {
    case p: ProductValue if p.values.length >= 3 =>
      OutputConfig(
        label = extractString(p.get(0)),
        format = extractString(p.get(1)),
        primary = extractBool(p.get(2))
      )
    case other =>
      throw new RuntimeException(s"Expected OutputConfig struct (ProductValue with 3 fields), got: $other")
  }
}
