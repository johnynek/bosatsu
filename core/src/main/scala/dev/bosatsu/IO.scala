package dev.bosatsu

import Value._
import cats.data.NonEmptyList
import java.math.BigInteger

/** IO module for deferred execution with provenance tracking.
  *
  * IO represents a computation that can be analyzed statically (structure)
  * and executed with tracking (runtime values). This enables "Why?" explanations
  * in simulations by capturing both formulas and their computed values.
  *
  * Design:
  * - IO[A] is represented as ExternalValue containing an IOEffect[A]
  * - IOEffect is an ADT describing the computation structure
  * - runWithProvenance interprets the structure and captures values
  */
object IO {

  /** Loads a file *at compile time* as a means of embedding external files into
    * strings.
    */
  private[bosatsu] inline def loadFileInCompile(file: String): String =
    ${ Macro.loadFileInCompileImpl('file) }

  /** String representation of the IO module */
  val ioString: String =
    loadFileInCompile("core/src/main/resources/bosatsu/io.bosatsu")

  val packageName: PackageName =
    PackageName.parse("Bosatsu/IO").get

  /** IOEffect ADT - describes a computation without executing it */
  sealed trait IOEffect[+A]
  case class Pure[A](value: A) extends IOEffect[A]
  case class FlatMap[A, B](io: IOEffect[A], f: Value => IOEffect[B]) extends IOEffect[B]
  case class Capture[A](name: String, value: A, formula: Option[String]) extends IOEffect[A]
  case class Trace() extends IOEffect[String]
  case class RandomInt(min: Int, max: Int) extends IOEffect[Int]

  /** Provenance node - represents a captured value in the computation trace */
  case class ProvenanceNode(
    id: Long,
    name: String,
    valueStr: String,  // Serialized value for display
    dependencies: List[Long],
    formula: String
  )

  /** Provenance trace - the full computation trace */
  case class ProvenanceTrace(
    nodes: Map[Long, ProvenanceNode],
    root: Long
  )

  /** Runtime interpreter that executes IO and captures provenance */
  def runWithProvenance[A](io: IOEffect[A], rng: scala.util.Random = new scala.util.Random()): (A, ProvenanceTrace) = {
    var nodeId = 0L
    val nodes = scala.collection.mutable.Map[Long, ProvenanceNode]()
    var currentDeps: List[Long] = Nil

    def valueToString(v: Any): String = v match {
      case VInt(bi) => bi.toString
      case Str(s) => s"\"$s\""
      case True => "True"
      case False => "False"
      case ExternalValue(d: java.lang.Double) => d.toString
      case p: ProductValue => s"(${p.values.map(valueToString).mkString(", ")})"
      case other => other.toString
    }

    def interpret[B](effect: IOEffect[B]): B = effect match {
      case Pure(v) => v
      case FlatMap(io, f) =>
        val a = interpret(io)
        // Convert result to Value if needed for the function application
        val aVal = a match {
          case v: Value => v
          case i: Int => VInt(BigInteger.valueOf(i))
          case d: Double => ExternalValue(java.lang.Double.valueOf(d))
          case s: String => Str(s)
          case other => ExternalValue(other)
        }
        val nextIO = f(aVal)
        interpret(nextIO.asInstanceOf[IOEffect[B]])
      case Capture(name, value, formula) =>
        val id = { nodeId += 1; nodeId }
        nodes(id) = ProvenanceNode(
          id = id,
          name = name,
          valueStr = valueToString(value),
          dependencies = currentDeps,
          formula = formula.getOrElse(name)
        )
        currentDeps = List(id)  // This value is now a dependency for next
        value
      case Trace() =>
        // Return current trace as string
        val traceStr = nodes.values.map(n => s"${n.name}=${n.valueStr}").mkString(", ")
        traceStr.asInstanceOf[B]
      case RandomInt(min, max) =>
        val result = rng.nextInt(max - min + 1) + min
        result.asInstanceOf[B]
    }

    val result = interpret(io)
    (result, ProvenanceTrace(nodes.toMap, nodeId))
  }

  /** JVM externals for IO operations */
  val jvmExternals: Externals =
    Externals.empty
      // pure :: a -> IO a
      .add(packageName, "pure", FfiCall.Fn1 { v =>
        ExternalValue(Pure(v))
      })
      // flatMap :: IO a -> (a -> IO b) -> IO b
      .add(packageName, "flatMap", FfiCall.Fn2 { (ioVal, f) =>
        val io = ioVal.asExternal.toAny.asInstanceOf[IOEffect[Any]]
        ExternalValue(FlatMap(io, { v =>
          // Apply the Bosatsu function to get the next IO
          val result = f.applyAll(NonEmptyList.one(v))
          result.asExternal.toAny.asInstanceOf[IOEffect[Any]]
        }))
      })
      // sequence :: List[IO a] -> IO[List a]
      .add(packageName, "sequence", FfiCall.Fn1 { listVal =>
        // Convert Bosatsu List to Scala List of IOEffects
        val ios: List[IOEffect[Value]] = VList.unapply(listVal) match {
          case Some(items) => items.map(_.asExternal.toAny.asInstanceOf[IOEffect[Value]])
          case None => Nil
        }
        // Create an IO that sequences all the IOs using a recursive approach
        def go(remaining: List[IOEffect[Value]], acc: List[Value]): IOEffect[Value] = {
          remaining match {
            case Nil => Pure(VList(acc.reverse))
            case head :: tail =>
              FlatMap[Value, Value](head, v => go(tail, v :: acc))
          }
        }
        ExternalValue(go(ios, Nil))
      })
      // capture :: String -> a -> IO a
      .add(packageName, "capture", FfiCall.Fn2 { (name, value) =>
        val nameStr = name match { case Str(s) => s; case _ => "?" }
        ExternalValue(Capture(nameStr, value, None))
      })
      // captureFormula :: String -> String -> a -> IO a
      .add(packageName, "captureFormula", FfiCall.Fn3 { (name, formula, value) =>
        val nameStr = name match { case Str(s) => s; case _ => "?" }
        val formulaStr = formula match { case Str(s) => s; case _ => "?" }
        ExternalValue(Capture(nameStr, value, Some(formulaStr)))
      })
      // trace :: () -> IO String
      .add(packageName, "trace", FfiCall.Fn1 { _ =>
        ExternalValue(Trace())
      })
      // random_Int :: Int -> Int -> IO Int
      .add(packageName, "random_Int", FfiCall.Fn2 { (min, max) =>
        val minInt = min match { case VInt(bi) => bi.intValue; case _ => 0 }
        val maxInt = max match { case VInt(bi) => bi.intValue; case _ => 100 }
        ExternalValue(RandomInt(minInt, maxInt))
      })
}
