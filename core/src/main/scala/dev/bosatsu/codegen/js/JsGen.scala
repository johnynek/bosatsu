package dev.bosatsu.codegen.js

import cats.Monad
import cats.data.{NonEmptyList, State}
import dev.bosatsu.{Identifier, Lit, Matchless, PackageName}
import dev.bosatsu.Identifier.Bindable
import dev.bosatsu.Matchless.Expr
import dev.bosatsu.rankn.DataRepr
import org.typelevel.paiges.Doc

import cats.implicits._

/**
 * JavaScript code generator from Matchless IR.
 * Follows the same architecture as PythonGen/ClangGen.
 *
 * Uses an Env monad for:
 * - Variable binding and renaming to avoid JS reserved words
 * - Generating temporary variables
 * - Tracking closure slots
 *
 * Predef intrinsics are inlined as native JS operations.
 */
object JsGen {

  // ==================
  // Environment Monad
  // ==================

  sealed abstract class Env[+A]

  object Env {
    def pure[A](a: A): Env[A] = envMonad.pure(a)

    // Type alias for State monad with our EnvState
    private type EnvStateT[A] = State[EnvState, A]

    implicit def envMonad: Monad[Env] =
      new Monad[Env] {
        private val m = Monad[EnvStateT]
        def pure[A](a: A): Env[A] = EnvImpl(m.pure(a))
        override def map[A, B](ea: Env[A])(fn: A => B): Env[B] =
          EnvImpl(ea.asInstanceOf[EnvImpl[A]].state.map(fn))
        def flatMap[A, B](ea: Env[A])(fn: A => Env[B]): Env[B] =
          EnvImpl(ea.asInstanceOf[EnvImpl[A]].state.flatMap(a => fn(a).asInstanceOf[EnvImpl[B]].state))
        def tailRecM[A, B](a: A)(fn: A => Env[Either[A, B]]): Env[B] =
          EnvImpl(m.tailRecM(a)(aa => fn(aa).asInstanceOf[EnvImpl[Either[A, B]]].state))
      }

    /** State for the environment monad */
    case class EnvState(
      // Stack-based bindings: each Bindable maps to a non-empty list of identifiers
      // The head of the list is the current binding, tail contains shadowed bindings
      bindings: Map[Bindable, List[Code.Ident]],
      nextTmp: Long,
      anonNames: Map[Long, Code.Ident],
      usedNames: Set[String] = Set.empty  // Track all names ever used to prevent shadowing
    ) {
      def bind(b: Bindable): (EnvState, Code.Ident) = {
        val baseIdent = escape(b)
        // Check if this name has ever been used (prevents JS shadowing issues)
        val ident = if (usedNames.contains(baseIdent.name)) {
          // Find a unique suffix
          var suffix = 1
          var candidate = s"${baseIdent.name}_$suffix"
          while (usedNames.contains(candidate)) {
            suffix += 1
            candidate = s"${baseIdent.name}_$suffix"
          }
          Code.Ident(candidate)
        } else {
          baseIdent
        }
        // Push the new binding onto the stack for this Bindable
        val newStack = ident :: bindings.getOrElse(b, Nil)
        (copy(bindings = bindings + (b -> newStack), usedNames = usedNames + ident.name), ident)
      }

      /** Bind a Bindable to a specific identifier (for pre-qualified names) */
      def bindWithIdent(b: Bindable, ident: Code.Ident): (EnvState, Unit) = {
        val newStack = ident :: bindings.getOrElse(b, Nil)
        (copy(bindings = bindings + (b -> newStack), usedNames = usedNames + ident.name), ())
      }

      def deref(b: Bindable): Code.Ident =
        bindings.get(b).flatMap(_.headOption).getOrElse(escape(b))

      def unbind(b: Bindable): EnvState = {
        // Pop from the stack - if there are shadowed bindings, restore them
        bindings.get(b) match {
          case Some(_ :: rest) if rest.nonEmpty =>
            copy(bindings = bindings + (b -> rest))
          case _ =>
            copy(bindings = bindings - b)
        }
      }

      def getNextTmp: (EnvState, Long) = {
        val id = nextTmp
        (copy(nextTmp = nextTmp + 1), id)
      }

      def anonName(id: Long): (EnvState, Code.Ident) = {
        anonNames.get(id) match {
          case Some(ident) => (this, ident)
          case None =>
            val ident = Code.Ident(s"_anon$id")
            (copy(anonNames = anonNames + (id -> ident), usedNames = usedNames + ident.name), ident)
        }
      }
    }

    object EnvState {
      def empty: EnvState = EnvState(Map.empty, 0L, Map.empty, Set.empty)
    }

    private case class EnvImpl[A](state: State[EnvState, A]) extends Env[A]

    private def env[A](fn: EnvState => (EnvState, A)): Env[A] =
      EnvImpl(State(fn))

    private def read[A](fn: EnvState => A): Env[A] =
      EnvImpl(State.inspect(fn))

    def bind(b: Bindable): Env[Code.Ident] = env(_.bind(b))
    def bindWithIdent(b: Bindable, ident: Code.Ident): Env[Unit] = env(_.bindWithIdent(b, ident))
    def deref(b: Bindable): Env[Code.Ident] = read(_.deref(b))
    def unbind(b: Bindable): Env[Unit] = env(s => (s.unbind(b), ()))
    def newTmp: Env[Code.Ident] = env { s =>
      val (s1, id) = s.getNextTmp
      (s1, Code.Ident(s"_tmp$id"))
    }
    def anonName(id: Long): Env[Code.Ident] = env(_.anonName(id))
    def anonMutName(id: Long): Env[Code.Ident] = env(_.anonName(id)) // Same for now

    def run[A](env: Env[A]): (EnvState, A) =
      env.asInstanceOf[EnvImpl[A]].state.run(EnvState.empty).value

    def render(env: Env[Code.Statement]): Doc = {
      val (_, stmt) = run(env)
      Code.toDoc(stmt)
    }
  }

  // ==================
  // Name Escaping
  // ==================

  /** JavaScript reserved words (including Node.js globals) */
  val jsReservedWords: Set[String] = Set(
    // ECMAScript reserved words
    "break", "case", "catch", "class", "const", "continue", "debugger", "default",
    "delete", "do", "else", "enum", "export", "extends", "false", "finally", "for",
    "function", "if", "import", "in", "instanceof", "let", "new", "null", "return",
    "static", "super", "switch", "this", "throw", "true", "try", "typeof", "undefined",
    "var", "void", "while", "with", "yield", "await", "arguments", "eval",
    // Node.js / CommonJS globals
    "module", "exports", "require", "global", "__dirname", "__filename",
    // Browser globals
    "window", "document", "console", "self",
    // Common standard library names that could conflict
    "Array", "Object", "String", "Number", "Boolean", "Function", "Symbol",
    "Error", "Map", "Set", "Promise", "Proxy", "Reflect", "Math", "JSON", "Date",
    "RegExp", "parseInt", "parseFloat", "isNaN", "isFinite", "Infinity", "NaN"
  )

  /** Escape a Bindable name to a valid JavaScript identifier */
  def escape(b: Bindable): Code.Ident = {
    val base = b match {
      case Identifier.Name(n) => n
      case Identifier.Backticked(n) => n
      case op: Identifier.Operator => s"op_${op.asString.hashCode.abs}"
    }

    val escaped = base.flatMap {
      case c if c.isLetterOrDigit => c.toString
      case '_' => "_"
      case '$' => "$"
      case _ => "_"
    }

    // Ensure starts with letter or underscore
    val withPrefix = if (escaped.isEmpty || escaped.head.isDigit) s"_$escaped" else escaped

    // Avoid reserved words
    val safe = if (jsReservedWords.contains(withPrefix)) s"_$withPrefix" else withPrefix

    Code.Ident(safe)
  }

  /** Escape a package name for module naming */
  def escapePackage(pn: PackageName): String =
    pn.parts.toList.map(_.replace(".", "_")).mkString("_")

  /**
   * Create a qualified JS identifier for a global name.
   * Uses format: package$name to avoid collisions between packages.
   * e.g., Bosatsu/Nat::times2 becomes Bosatsu_Nat$times2
   */
  def qualifiedName(pack: PackageName, name: Bindable): Code.Ident = {
    val packPrefix = escapePackage(pack)
    val baseName = escape(name).name
    Code.Ident(s"${packPrefix}$$${baseName}")
  }

  // ==================
  // Predef Intrinsics
  // ==================

  /**
   * Intrinsic functions from Bosatsu/Predef that are inlined as native JS operations.
   * This mirrors PythonGen.PredefExternal.
   */
  object PredefExternal {
    // Type for intrinsic functions: takes list of JS expressions, returns JS expression
    type IntrinsicFn = List[Code.Expression] => Code.Expression

    // Compare function returns [0] (LT), [1] (EQ), or [2] (GT) - boxed for pattern matching
    private val cmpFn: IntrinsicFn = {
      case List(a, b) =>
        // a < b ? [0] : (a === b ? [1] : [2])
        Code.Ternary(a < b, Code.ArrayLiteral(List(Code.IntLiteral(0))),
          Code.Ternary(a === b, Code.ArrayLiteral(List(Code.IntLiteral(1))),
            Code.ArrayLiteral(List(Code.IntLiteral(2)))))
      case args => throw new IllegalArgumentException(s"cmp expects 2 args, got ${args.length}")
    }

    /** Map of intrinsic function names to (implementation, arity) */
    val results: Map[Bindable, (IntrinsicFn, Int)] = Map(
      // Arithmetic
      Identifier.unsafeBindable("add") -> ((args: List[Code.Expression]) => args.head + args(1), 2),
      Identifier.unsafeBindable("sub") -> ((args: List[Code.Expression]) => args.head - args(1), 2),
      Identifier.unsafeBindable("times") -> ((args: List[Code.Expression]) => args.head * args(1), 2),
      Identifier.unsafeBindable("div") -> ((args: List[Code.Expression]) =>
        // Division by zero returns 0, use Math.trunc for integer division
        Code.Ternary(args(1),
          Code.Call(Code.Ident("Math").dot("trunc"), List(Code.BinExpr(args.head, Code.BinOp.Div, args(1)))),
          Code.IntLiteral(0)), 2),
      Identifier.unsafeBindable("mod_Int") -> ((args: List[Code.Expression]) =>
        // Mod by zero returns the original value
        Code.Ternary(args(1), Code.BinExpr(args.head, Code.BinOp.Mod, args(1)), args.head), 2),

      // Comparison
      Identifier.unsafeBindable("cmp_Int") -> (cmpFn, 2),
      Identifier.unsafeBindable("eq_Int") -> ((args: List[Code.Expression]) =>
        // Return [1] for true (Some), [0] for false (None) to match Bosatsu Bool
        Code.Ternary(args.head === args(1), Code.ArrayLiteral(List(Code.IntLiteral(1))),
          Code.ArrayLiteral(List(Code.IntLiteral(0)))), 2),

      // Bitwise
      Identifier.unsafeBindable("shift_left_Int") -> ((args: List[Code.Expression]) =>
        Code.BinExpr(args.head, Code.BinOp.BitShiftLeft, args(1)), 2),
      Identifier.unsafeBindable("shift_right_Int") -> ((args: List[Code.Expression]) =>
        Code.BinExpr(args.head, Code.BinOp.BitShiftRight, args(1)), 2),
      Identifier.unsafeBindable("and_Int") -> ((args: List[Code.Expression]) =>
        Code.BinExpr(args.head, Code.BinOp.BitAnd, args(1)), 2),
      Identifier.unsafeBindable("or_Int") -> ((args: List[Code.Expression]) =>
        Code.BinExpr(args.head, Code.BinOp.BitOr, args(1)), 2),
      Identifier.unsafeBindable("xor_Int") -> ((args: List[Code.Expression]) =>
        Code.BinExpr(args.head, Code.BinOp.BitXor, args(1)), 2),
      Identifier.unsafeBindable("not_Int") -> ((args: List[Code.Expression]) =>
        Code.PrefixExpr(Code.PrefixOp.BitNot, args.head), 1),

      // GCD using Euclidean algorithm
      Identifier.unsafeBindable("gcd_Int") -> ((args: List[Code.Expression]) =>
        // Inline a simple GCD: we'll use a helper function
        // For now, just use a simple IIFE with while loop
        Code.Call(Code.Ident("_gcd"), args), 2),

      // int_loop(i, state, fn) - countdown loop with accumulator
      Identifier.unsafeBindable("int_loop") -> ((args: List[Code.Expression]) =>
        Code.Call(Code.Ident("_int_loop"), args), 3),

      // String operations
      Identifier.unsafeBindable("concat_String") -> ((args: List[Code.Expression]) =>
        // Bosatsu strings are linked lists, need to convert and concat
        Code.Call(Code.Ident("_concat_String"), args), 1),
      Identifier.unsafeBindable("int_to_String") -> ((args: List[Code.Expression]) =>
        // Convert int to string, then to Bosatsu string list
        Code.Call(Code.Ident("_int_to_String"), args), 1),
      Identifier.unsafeBindable("string_to_Int") -> ((args: List[Code.Expression]) =>
        Code.Call(Code.Ident("_string_to_Int"), args), 1),
      Identifier.unsafeBindable("char_to_String") -> ((args: List[Code.Expression]) =>
        // char is already a string in JS
        Code.Call(Code.Ident("_char_to_String"), args), 1),

      // Trace for debugging
      Identifier.unsafeBindable("trace") -> ((args: List[Code.Expression]) =>
        Code.Call(Code.Ident("_trace"), args), 2),

      // String comparison
      Identifier.unsafeBindable("cmp_String") -> ((args: List[Code.Expression]) =>
        Code.Call(Code.Ident("_cmp_String"), args), 2),

      // String partitioning
      Identifier.unsafeBindable("partition_String") -> ((args: List[Code.Expression]) =>
        Code.Call(Code.Ident("_partition_String"), args), 2),
      Identifier.unsafeBindable("rpartition_String") -> ((args: List[Code.Expression]) =>
        Code.Call(Code.Ident("_rpartition_String"), args), 2)
    )

    /** Check if an expression is a predef external and extract its function */
    def unapply[A](expr: Expr[A]): Option[(IntrinsicFn, Int)] =
      expr match {
        case Matchless.Global(_, PackageName.PredefName, name) => results.get(name)
        case _ => None
      }

    /** Create a lambda wrapper for a standalone intrinsic reference */
    def makeLambda(arity: Int)(fn: IntrinsicFn): Code.Expression = {
      val argNames = (0 until arity).map(i => s"_a$i").toList
      val argExprs = argNames.map(Code.Ident(_))
      Code.ArrowFunction(argNames, Left(fn(argExprs)))
    }
  }

  // ==================
  // Numeric Intrinsics
  // ==================

  /**
   * Intrinsic functions from Bosatsu/Numeric that are inlined as native JS operations.
   * These use symbolic operators (+., -., *., /.) to distinguish from Int ops.
   * Operations bypass RingOpt algebraic expansion - preserves original formulas.
   */
  object NumericExternal {
    import PredefExternal.IntrinsicFn

    val NumericPackage: PackageName = PackageName.parse("Bosatsu/Numeric").get

    // Compare function for Doubles - returns [0] (LT), [1] (EQ), or [2] (GT)
    // NaN comparisons return GT for consistency with IEEE 754 totalOrder
    private val cmpDoubleFn: IntrinsicFn = {
      case List(a, b) =>
        // Check for NaN first - Number.isNaN(a) || Number.isNaN(b) => GT
        val isNaN = Code.BinExpr(
          Code.Call(Code.PropertyAccess(Code.Ident("Number"), "isNaN"), List(a)),
          Code.BinOp.Or,
          Code.Call(Code.PropertyAccess(Code.Ident("Number"), "isNaN"), List(b))
        )
        Code.Ternary(isNaN, Code.ArrayLiteral(List(Code.IntLiteral(2))),
          Code.Ternary(a < b, Code.ArrayLiteral(List(Code.IntLiteral(0))),
            Code.Ternary(a === b, Code.ArrayLiteral(List(Code.IntLiteral(1))),
              Code.ArrayLiteral(List(Code.IntLiteral(2))))))
      case args => throw new IllegalArgumentException(s"cmp_Double expects 2 args, got ${args.length}")
    }

    /** Map of intrinsic function names to (implementation, arity) */
    val results: Map[Bindable, (IntrinsicFn, Int)] = Map(
      // Arithmetic - symbolic operators for Double (use Operator for symbolic names)
      Identifier.Operator("+.") -> ((args: List[Code.Expression]) => args.head + args(1), 2),
      Identifier.Operator("-.") -> ((args: List[Code.Expression]) => args.head - args(1), 2),
      Identifier.Operator("*.") -> ((args: List[Code.Expression]) => args.head * args(1), 2),
      Identifier.Operator("/.") -> ((args: List[Code.Expression]) =>
        // Double division - no truncation needed
        Code.BinExpr(args.head, Code.BinOp.Div, args(1)), 2),

      // Conversion (use Name for regular identifiers)
      Identifier.Name("from_Int") -> ((args: List[Code.Expression]) =>
        // JS numbers are already floats, no conversion needed
        args.head, 1),
      Identifier.Name("to_Int") -> ((args: List[Code.Expression]) =>
        // Truncate to integer
        Code.Call(Code.Ident("Math").dot("trunc"), List(args.head)), 1),

      // Comparison
      Identifier.Name("cmp_Double") -> (cmpDoubleFn, 2),
      Identifier.Name("eq_Double") -> ((args: List[Code.Expression]) =>
        Code.Ternary(args.head === args(1), Code.ArrayLiteral(List(Code.IntLiteral(1))),
          Code.ArrayLiteral(List(Code.IntLiteral(0)))), 2),

      // Unary operations
      Identifier.Name("neg_Double") -> ((args: List[Code.Expression]) =>
        Code.PrefixExpr(Code.PrefixOp.Neg, args.head), 1),
      Identifier.Name("abs_Double") -> ((args: List[Code.Expression]) =>
        Code.Call(Code.Ident("Math").dot("abs"), List(args.head)), 1)
    )

    /** Check if an expression is a numeric external and extract its function */
    def unapply[A](expr: Expr[A]): Option[(IntrinsicFn, Int)] =
      expr match {
        case Matchless.Global(_, pack, name) if pack == NumericPackage => results.get(name)
        case _ => None
      }

    /** Create a lambda wrapper for a standalone intrinsic reference */
    def makeLambda(arity: Int)(fn: IntrinsicFn): Code.Expression =
      PredefExternal.makeLambda(arity)(fn)
  }

  /** These are values replaced with JS operations (for intrinsicValues method) */
  def intrinsicValues: Map[PackageName, Set[Bindable]] =
    Map(
      PackageName.PredefName -> PredefExternal.results.keySet,
      NumericExternal.NumericPackage -> NumericExternal.results.keySet
    )

  // ==================
  // Code Generation
  // ==================

  /** Convert a Matchless literal to a JS literal */
  def literal(lit: Lit): Code.Expression = lit match {
    case Lit.Integer(i) => Code.IntLiteral(i)
    case Lit.Str(s) =>
      // Convert JS string to Bosatsu string (linked list)
      Code.Call(Code.Ident("_js_to_bosatsu_string"), List(Code.StringLiteral(s)))
    case Lit.Chr(c) =>
      // Char as a single-character Bosatsu string: [1, char, [0]]
      Code.ArrayLiteral(List(Code.IntLiteral(1), Code.StringLiteral(c.toString), Code.ArrayLiteral(List(Code.IntLiteral(0)))))
  }

  /** Convert a Matchless literal to a raw JS literal (for comparisons) */
  def literalRaw(lit: Lit): Code.Expression = lit match {
    case Lit.Integer(i) => Code.IntLiteral(i)
    case Lit.Str(s) => Code.StringLiteral(s)
    case Lit.Chr(c) => Code.StringLiteral(c.toString)
  }

  /**
   * Generate JavaScript for a Matchless expression with a pre-bound top-level name.
   * This is used when generating top-level bindings that may be recursive.
   * The topLevelName is bound in the environment so that Local references to it
   * (from recursive calls) will resolve to the correct qualified name.
   */
  def exprToJsWithTopLevel[A](
      expr: Matchless.Expr[A],
      topLevelName: Bindable,
      qualifiedIdent: Code.Ident
  ): Env[Code.Expression] = {
    for {
      _ <- Env.bindWithIdent(topLevelName, qualifiedIdent)
      result <- exprToJs(expr)
      _ <- Env.unbind(topLevelName)
    } yield result
  }

  /** Generate JavaScript for a Matchless expression */
  def exprToJs[A](expr: Matchless.Expr[A]): Env[Code.Expression] = {
    import Matchless._

    expr match {
      case Literal(lit) =>
        Env.pure(literal(lit))

      case Local(arg) =>
        Env.deref(arg)

      case PredefExternal((fn, arity)) =>
        // Standalone reference to a predef intrinsic - wrap in lambda
        Env.pure(PredefExternal.makeLambda(arity)(fn))

      case NumericExternal((fn, arity)) =>
        // Standalone reference to a numeric intrinsic - wrap in lambda
        Env.pure(NumericExternal.makeLambda(arity)(fn))

      case Global(_, pack, name) =>
        // Runtime-provided functions from Predef - use unqualified names
        val runtimeProvided = Set("foldl_List", "range", "flat_map_List")
        val nameStr = name match {
          case Identifier.Name(n) => n
          case Identifier.Backticked(n) => n
          case op: Identifier.Operator => op.asString
        }
        val ident = if (pack == PackageName.PredefName && runtimeProvided.contains(nameStr)) {
          escape(name)  // Use unqualified name for runtime functions
        } else {
          qualifiedName(pack, name)  // Use qualified name for all others
        }
        Env.pure(ident)

      case LocalAnon(id) =>
        Env.anonName(id)

      case LocalAnonMut(id) =>
        Env.anonMutName(id)

      case ClosureSlot(idx) =>
        // Closure slots are accessed via a special array
        Env.pure(Code.IndexAccess(Code.Ident("_slots"), Code.IntLiteral(idx)))

      case Lambda(captures, recName, args, body) =>
        // Generate a function
        // If there are captures, wrap in a closure that binds them to _slots
        // If there's a recName, use a named function expression for self-recursion
        for {
          captureExprs <- captures.traverse(exprToJs)
          // Bind recName if present so body can reference it
          recIdent <- recName match {
            case Some(name) => Env.bind(name).map(Some(_))
            case None => Env.pure(None)
          }
          argIdents <- args.toList.traverse(Env.bind)
          bodyJs <- exprToJs(body)
          _ <- args.toList.traverse_(Env.unbind)
          _ <- recName match {
            case Some(name) => Env.unbind(name)
            case None => Env.pure(())
          }
        } yield {
          val argNames = argIdents.map(_.name)

          // If we have a recName, use a named function expression
          val innerFn = recIdent match {
            case Some(ident) =>
              // Use named function expression: (function name(args) { return body; })
              val returnStmt = Code.Return(Some(bodyJs))
              val block = Code.Block(cats.data.NonEmptyList.one(returnStmt))
              Code.Function(Some(ident.name), argNames, block)
            case None =>
              Code.ArrowFunction(argNames, Left(bodyJs))
          }

          if (captures.isEmpty) {
            innerFn
          } else {
            // Wrap: ((_slots) => innerFn)([cap1, cap2, ...])
            val wrapperFn = Code.ArrowFunction(List("_slots"), Left(innerFn))
            Code.Call(wrapperFn, List(Code.ArrayLiteral(captureExprs)))
          }
        }

      case App(PredefExternal((fn, _)), args) =>
        // Inline application of predef intrinsic
        for {
          argsJs <- args.toList.traverse(exprToJs)
        } yield fn(argsJs)

      case App(NumericExternal((fn, _)), args) =>
        // Inline application of numeric intrinsic
        for {
          argsJs <- args.toList.traverse(exprToJs)
        } yield fn(argsJs)

      case App(fn, args) =>
        for {
          fnJs <- exprToJs(fn)
          argsJs <- args.toList.traverse(exprToJs)
        } yield Code.Call(fnJs, argsJs)

      case Let(Right(name), value, body) =>
        // const name = value; body
        for {
          valueJs <- exprToJs(value)
          ident <- Env.bind(name)
          bodyJs <- exprToJs(body)
          _ <- Env.unbind(name)
        } yield {
          // Use IIFE to create a scope for const
          val iife = Code.ArrowFunction(Nil, Right(Code.Block(NonEmptyList.of(
            Code.Const(ident.name, valueJs),
            Code.Return(Some(bodyJs))
          ))))
          Code.Call(iife, Nil)
        }

      case Let(Left(LocalAnon(id)), value, body) =>
        for {
          valueJs <- exprToJs(value)
          ident <- Env.anonName(id)
          bodyJs <- exprToJs(body)
        } yield {
          val iife = Code.ArrowFunction(Nil, Right(Code.Block(NonEmptyList.of(
            Code.Const(ident.name, valueJs),
            Code.Return(Some(bodyJs))
          ))))
          Code.Call(iife, Nil)
        }

      case LetMut(LocalAnonMut(id), body) =>
        // let name; body
        for {
          ident <- Env.anonMutName(id)
          bodyJs <- exprToJs(body)
        } yield {
          val iife = Code.ArrowFunction(Nil, Right(Code.Block(NonEmptyList.of(
            Code.Let(ident.name, None),
            Code.Return(Some(bodyJs))
          ))))
          Code.Call(iife, Nil)
        }

      case If(cond, thenExpr, elseExpr) =>
        for {
          condJs <- boolExprToJs(cond)
          thenJs <- exprToJs(thenExpr)
          elseJs <- exprToJs(elseExpr)
        } yield Code.Ternary(condJs, thenJs, elseJs)

      case Always(cond, result) =>
        // Evaluate cond for side effects, then return result
        for {
          condJs <- boolExprToJs(cond)
          resultJs <- exprToJs(result)
        } yield {
          // Use comma operator: (cond, result)
          // Or IIFE if cond has side effects
          Code.Call(
            Code.ArrowFunction(Nil, Right(Code.Block(NonEmptyList.of(
              Code.ExprStatement(condJs),
              Code.Return(Some(resultJs))
            )))),
            Nil
          )
        }

      case MakeEnum(variant, arity, _) =>
        // Create an array [variant, arg0, arg1, ...]
        // This is a constructor function
        if (arity == 0) {
          Env.pure(Code.ArrayLiteral(List(Code.IntLiteral(variant))))
        } else {
          // Return a function that creates the enum
          val args = (0 until arity).map(i => s"_a$i").toList
          val elements = Code.IntLiteral(variant) :: args.map(Code.Ident(_))
          Env.pure(Code.ArrowFunction(args, Left(Code.ArrayLiteral(elements))))
        }

      case MakeStruct(arity) =>
        // Create an array [arg0, arg1, ...]
        if (arity == 0) {
          Env.pure(Code.ArrayLiteral(Nil))
        } else {
          val args = (0 until arity).map(i => s"_a$i").toList
          val elements = args.map(Code.Ident(_))
          Env.pure(Code.ArrowFunction(args, Left(Code.ArrayLiteral(elements))))
        }

      case ZeroNat =>
        Env.pure(Code.IntLiteral(0))

      case SuccNat =>
        // Return a function n => n + 1
        Env.pure(Code.ArrowFunction(List("n"), Left(Code.Ident("n") + Code.IntLiteral(1))))

      case PrevNat(n) =>
        for {
          nJs <- exprToJs(n)
        } yield nJs - Code.IntLiteral(1)

      case GetEnumElement(arg, _, index, _) =>
        // arg[index + 1] (index 0 is the variant tag)
        for {
          argJs <- exprToJs(arg)
        } yield Code.IndexAccess(argJs, Code.IntLiteral(index + 1))

      case GetStructElement(arg, index, _) =>
        // arg[index]
        for {
          argJs <- exprToJs(arg)
        } yield Code.IndexAccess(argJs, Code.IntLiteral(index))

      case WhileExpr(cond, effect, LocalAnonMut(resultId)) =>
        for {
          resultIdent <- Env.anonMutName(resultId)
          condJs <- boolExprToJs(cond)
          effectJs <- exprToJs(effect)
        } yield {
          // Use IIFE with while loop
          val whileLoop = Code.WhileLoop(condJs, Code.block(Code.ExprStatement(effectJs)))
          val iife = Code.ArrowFunction(Nil, Right(Code.Block(NonEmptyList.of(
            whileLoop,
            Code.Return(Some(resultIdent))
          ))))
          Code.Call(iife, Nil)
        }
    }
  }

  /** Generate JavaScript for a Matchless boolean expression */
  def boolExprToJs[A](bexpr: Matchless.BoolExpr[A]): Env[Code.Expression] = {
    import Matchless._

    bexpr match {
      case TrueConst =>
        Env.pure(Code.TrueLit)

      case EqualsLit(expr, lit) =>
        for {
          exprJs <- exprToJs(expr)
        } yield lit match {
          case Lit.Str(_) | Lit.Chr(_) =>
            // Bosatsu strings are linked lists, need to convert to JS string for comparison
            Code.Call(Code.Ident("_bosatsu_to_js_string"), List(exprJs)) === literalRaw(lit)
          case _ =>
            exprJs === literalRaw(lit)
        }

      case EqualsNat(expr, nat) =>
        for {
          exprJs <- exprToJs(expr)
        } yield nat match {
          case DataRepr.ZeroNat => exprJs === Code.IntLiteral(0)
          case DataRepr.SuccNat => exprJs > Code.IntLiteral(0)
        }

      case And(e1, e2) =>
        for {
          e1Js <- boolExprToJs(e1)
          e2Js <- boolExprToJs(e2)
        } yield e1Js && e2Js

      case CheckVariant(expr, expect, _, _) =>
        // expr[0] === expect
        for {
          exprJs <- exprToJs(expr)
        } yield Code.IndexAccess(exprJs, Code.IntLiteral(0)) === Code.IntLiteral(expect)

      case SetMut(LocalAnonMut(id), value) =>
        // (name = value, true)
        for {
          ident <- Env.anonMutName(id)
          valueJs <- exprToJs(value)
        } yield {
          // Assignment expression returns the value, but we need to return true
          // Use comma operator in parens
          Code.Call(
            Code.ArrowFunction(Nil, Right(Code.Block(NonEmptyList.of(
              Code.Assignment(ident, valueJs),
              Code.Return(Some(Code.TrueLit))
            )))),
            Nil
          )
        }

      case MatchString(arg, parts, binds, mustMatch) =>
        // String pattern matching
        // Bosatsu strings are linked lists: [0] for empty, [1, char, tail] for cons
        // We need to convert to JS string for matching, then convert captures back
        import dev.bosatsu.pattern.StrPart
        import dev.bosatsu.pattern.StrPart._

        for {
          argJs <- exprToJs(arg)
          bindIdents <- binds.traverse { case LocalAnonMut(id) => Env.anonMutName(id) }
        } yield {
          val bindArray = bindIdents.toArray

          // We'll use _str as the converted JS string inside an IIFE
          val strIdent = Code.Ident("_str")

          // Convert Bosatsu string to JS string
          val convertToJs = Code.Call(Code.Ident("_bosatsu_to_js_string"), List(argJs))

          // Convert JS char to Bosatsu single-char string: [1, char, [0]]
          def charToBosatsu(charExpr: Code.Expression): Code.Expression = {
            Code.ArrayLiteral(List(Code.IntLiteral(1), charExpr, Code.ArrayLiteral(List(Code.IntLiteral(0)))))
          }

          // Convert JS substring to Bosatsu string
          def substringToBosatsu(substrExpr: Code.Expression): Code.Expression = {
            Code.Call(Code.Ident("_js_to_bosatsu_string"), List(substrExpr))
          }

          // Generate matching code based on pattern (using strIdent for the JS string)
          def matchParts(
              parts: List[StrPart],
              offset: Code.Expression,
              bindIdx: Int
          ): Code.Expression = {
            parts match {
              case Nil =>
                // Empty pattern - string must be empty or at end
                if (mustMatch) Code.TrueLit
                else strIdent.dot("length") === offset

              case LitStr(expect) :: Nil =>
                // Just a literal - string equals literal starting at offset
                if (mustMatch) Code.TrueLit
                else if (offset == Code.IntLiteral(0)) {
                  strIdent === Code.StringLiteral(expect)
                } else {
                  strIdent.dot("substring")(offset) === Code.StringLiteral(expect)
                }

              case LitStr(expect) :: tail =>
                // Literal followed by more - check startsWith and continue
                // Use .length (UTF-16 code units) to match JavaScript's string indexing
                val expectLen = expect.length
                val startsCheck = if (offset == Code.IntLiteral(0)) {
                  strIdent.dot("startsWith")(Code.StringLiteral(expect))
                } else {
                  strIdent.dot("startsWith")(Code.StringLiteral(expect), offset)
                }
                val newOffset = offset + Code.IntLiteral(expectLen)
                startsCheck.bin(Code.BinOp.And, matchParts(tail, newOffset, bindIdx))

              case (c: CharPart) :: Nil =>
                // Last character capture/check
                val lenCheck =
                  if (mustMatch) Code.TrueLit
                  else strIdent.dot("length") === (offset + Code.IntLiteral(1))
                if (c.capture && bindIdx < bindArray.length) {
                  // Capture char as Bosatsu string and return true
                  val jsChar = Code.IndexAccess(strIdent, offset)
                  val assign = Code.Assignment(bindArray(bindIdx), charToBosatsu(jsChar))
                  Code.Call(
                    Code.ArrowFunction(Nil, Right(Code.Block(cats.data.NonEmptyList.of(
                      assign,
                      Code.Return(Some(lenCheck))
                    )))),
                    Nil
                  )
                } else lenCheck

              case (c: CharPart) :: tail =>
                // Character followed by more
                val hasCharCheck =
                  if (mustMatch) Code.TrueLit
                  else offset < strIdent.dot("length")
                val newIdx = if (c.capture) bindIdx + 1 else bindIdx
                val newOffset = offset + Code.IntLiteral(1)
                if (c.capture && bindIdx < bindArray.length) {
                  // Capture char as Bosatsu string, then check rest
                  val jsChar = Code.IndexAccess(strIdent, offset)
                  val assign = Code.Assignment(bindArray(bindIdx), charToBosatsu(jsChar))
                  val rest = matchParts(tail, newOffset, newIdx)
                  Code.Call(
                    Code.ArrowFunction(Nil, Right(Code.Block(cats.data.NonEmptyList.of(
                      assign,
                      Code.Return(Some(hasCharCheck.bin(Code.BinOp.And, rest)))
                    )))),
                    Nil
                  )
                } else {
                  hasCharCheck.bin(Code.BinOp.And, matchParts(tail, newOffset, newIdx))
                }

              case WildStr :: Nil =>
                // Wildcard at end - always matches remaining
                Code.TrueLit

              case IndexStr :: Nil =>
                // Capture remaining string - convert back to Bosatsu string
                if (bindIdx < bindArray.length) {
                  val jsSubstr = if (offset == Code.IntLiteral(0)) strIdent
                    else strIdent.dot("substring")(offset)
                  val assign = Code.Assignment(bindArray(bindIdx), substringToBosatsu(jsSubstr))
                  Code.Call(
                    Code.ArrowFunction(Nil, Right(Code.Block(cats.data.NonEmptyList.of(
                      assign,
                      Code.Return(Some(Code.TrueLit))
                    )))),
                    Nil
                  )
                } else Code.TrueLit

              case _ =>
                // Other complex patterns - not implemented yet
                // Fall back to false (pattern doesn't match)
                Code.FalseLit
            }
          }

          val matchExpr = matchParts(parts, Code.IntLiteral(0), 0)

          // Wrap in IIFE that converts the string first
          Code.Call(
            Code.ArrowFunction(Nil, Right(Code.Block(cats.data.NonEmptyList.of(
              Code.Const(strIdent.name, convertToJs),
              Code.Return(Some(matchExpr))
            )))),
            Nil
          )
        }

      case LetBool(Right(name), value, in) =>
        for {
          valueJs <- exprToJs(value)
          ident <- Env.bind(name)
          inJs <- boolExprToJs(in)
          _ <- Env.unbind(name)
        } yield {
          val iife = Code.ArrowFunction(Nil, Right(Code.Block(NonEmptyList.of(
            Code.Const(ident.name, valueJs),
            Code.Return(Some(inJs))
          ))))
          Code.Call(iife, Nil)
        }

      case LetBool(Left(LocalAnon(id)), value, in) =>
        for {
          valueJs <- exprToJs(value)
          ident <- Env.anonName(id)
          inJs <- boolExprToJs(in)
        } yield {
          val iife = Code.ArrowFunction(Nil, Right(Code.Block(NonEmptyList.of(
            Code.Const(ident.name, valueJs),
            Code.Return(Some(inJs))
          ))))
          Code.Call(iife, Nil)
        }

      case LetMutBool(LocalAnonMut(id), in) =>
        for {
          ident <- Env.anonMutName(id)
          inJs <- boolExprToJs(in)
        } yield {
          val iife = Code.ArrowFunction(Nil, Right(Code.Block(NonEmptyList.of(
            Code.Let(ident.name, None),
            Code.Return(Some(inJs))
          ))))
          Code.Call(iife, Nil)
        }
    }
  }

  // ==================
  // Top-level rendering
  // ==================

  /** Render a single Matchless expression to JavaScript */
  def renderExpr[A](expr: Matchless.Expr[A]): String = {
    val (_, jsExpr) = Env.run(exprToJs(expr))
    Code.render(jsExpr)
  }

  /** Render a named binding (let or def) to JavaScript */
  def renderBinding[A](name: Bindable, expr: Matchless.Expr[A]): String = {
    val (_, jsExpr) = Env.run(exprToJs(expr))
    val ident = escape(name)
    Code.render(Code.Const(ident.name, jsExpr))
  }

  /** Render multiple bindings as an ES module */
  def renderModule[A](bindings: List[(Bindable, Matchless.Expr[A])]): String = {
    val statements = bindings.map { case (name, expr) =>
      val (_, jsExpr) = Env.run(exprToJs(expr))
      val ident = escape(name)
      Code.Const(ident.name, jsExpr)
    }

    val exports = bindings.map { case (name, _) =>
      escape(name).name
    }

    val allCode = if (statements.isEmpty) {
      Code.Statements(Nil)
    } else {
      val exportStmts = exports.map(Code.Export(_))
      Code.Statements(statements ++ exportStmts)
    }

    Code.render(allCode)
  }

  // ==================
  // Runtime Library
  // ==================

  /**
   * JavaScript runtime code that provides helper functions for intrinsics.
   * This should be included at the top of generated modules or in a separate runtime file.
   *
   * Bosatsu data representations:
   * - Lists are represented as: [0] for empty, [1, head, tail] for cons
   * - Bool/Option: [0] for False/None, [1] or [1, value] for True/Some
   * - Strings in Bosatsu are linked lists of characters
   */
  val runtimeCode: String =
    """// Bosatsu JS Runtime
// Note: Using 'var' for all declarations to create true globals accessible from generated code
// GCD using Euclidean algorithm
var _gcd = (a, b) => {
  a = Math.abs(a);
  b = Math.abs(b);
  while (b !== 0) {
    const t = b;
    b = a % b;
    a = t;
  }
  return a;
};

// int_loop(i, state, fn) - countdown loop with accumulator
// fn(i, state) returns [newI, newState]
// continues while newI > 0 AND newI < i (ensures progress)
var _int_loop = (i, state, fn) => {
  let _i = i;
  let _state = state;
  while (_i > 0) {
    const result = fn(_i, _state);
    const newI = result[0];
    const newState = result[1];
    // Update state regardless
    _state = newState;
    // Check if we should continue
    if (newI <= 0 || newI >= _i) {
      // Reached 0 or no progress, return new state
      return _state;
    }
    _i = newI;
  }
  return _state;
};

// Convert Bosatsu string list to JS string
var _bosatsu_to_js_string = (bstr) => {
  let result = '';
  let current = bstr;
  while (current[0] === 1) {
    result += current[1];
    current = current[2];
  }
  return result;
};

// Convert JS string to Bosatsu string list
var _js_to_bosatsu_string = (str) => {
  let result = [0]; // Empty list
  for (let i = str.length - 1; i >= 0; i--) {
    result = [1, str[i], result];
  }
  return result;
};

// concat_String - takes a Bosatsu list of strings and concatenates
var _concat_String = (strList) => {
  let result = '';
  let current = strList;
  while (current[0] === 1) {
    result += _bosatsu_to_js_string(current[1]);
    current = current[2];
  }
  return _js_to_bosatsu_string(result);
};

// int_to_String
var _int_to_String = (n) => _js_to_bosatsu_string(String(n));

// string_to_Int - returns Option: [0] for None, [1, value] for Some
var _string_to_Int = (bstr) => {
  const str = _bosatsu_to_js_string(bstr);
  const n = parseInt(str, 10);
  return isNaN(n) ? [0] : [1, n];
};

// char_to_String - char is already a single-char string (identity function)
var _char_to_String = (c) => c;

// trace - log message and return value
var _trace = (msg, value) => {
  console.log(_bosatsu_to_js_string(msg));
  return value;
};

// cmp_String - compare two Bosatsu strings, return [0] (LT), [1] (EQ), or [2] (GT)
// Returns boxed values for pattern matching consistency with cmp_Int
var _cmp_String = (a, b) => {
  const sa = _bosatsu_to_js_string(a);
  const sb = _bosatsu_to_js_string(b);
  return sa < sb ? [0] : (sa === sb ? [1] : [2]);
};

// partition_String - split string on first occurrence of separator
// Returns tuple of (before, sep, after) or (original, empty, empty) if not found
// partition_String - returns Option[(String, String)]
// None if sep is empty or not found, Some((before, after)) otherwise
var _partition_String = (str, sep) => {
  const s = _bosatsu_to_js_string(str);
  const sp = _bosatsu_to_js_string(sep);
  // Empty separator returns None
  if (sp.length === 0) return [0];
  const idx = s.indexOf(sp);
  if (idx === -1) return [0]; // Not found: None
  // Found: Some((before, after))
  return [1, [
    _js_to_bosatsu_string(s.substring(0, idx)),
    _js_to_bosatsu_string(s.substring(idx + sp.length))
  ]];
};

// rpartition_String - returns Option[(String, String)]
// None if sep is empty or not found, Some((before, after)) otherwise
var _rpartition_String = (str, sep) => {
  const s = _bosatsu_to_js_string(str);
  const sp = _bosatsu_to_js_string(sep);
  // Empty separator returns None
  if (sp.length === 0) return [0];
  const idx = s.lastIndexOf(sp);
  if (idx === -1) return [0]; // Not found: None
  // Found: Some((before, after))
  return [1, [
    _js_to_bosatsu_string(s.substring(0, idx)),
    _js_to_bosatsu_string(s.substring(idx + sp.length))
  ]];
};

// range(n) - generate list [0, 1, 2, ..., n-1]
var range = (n) => {
  let result = [0];
  for (let i = n - 1; i >= 0; i--) {
    result = [1, i, result];
  }
  return result;
};

// foldl_List(list, init, fn) - left fold over a list
var foldl_List = (list, init, fn) => {
  let acc = init;
  let current = list;
  while (current[0] === 1) {
    acc = fn(acc, current[1]);
    current = current[2];
  }
  return acc;
};

// flat_map_List(list, fn) - flatMap over a list
var flat_map_List = (list, fn) => {
  let result = [0];
  let current = list;
  // First collect all results in reverse order
  let reversed = [0];
  while (current[0] === 1) {
    const mapped = fn(current[1]);
    // Prepend mapped items to reversed
    let m = mapped;
    while (m[0] === 1) {
      reversed = [1, m[1], reversed];
      m = m[2];
    }
    current = current[2];
  }
  // Now reverse to get correct order
  current = reversed;
  while (current[0] === 1) {
    result = [1, current[1], result];
    current = current[2];
  }
  return result;
};

// Bosatsu/Prog external functions
// Prog is represented as a thunk that takes an environment and returns [result_type, value_or_error]
// result_type: 0 = success, 1 = error
var Bosatsu_Prog$pure = a => _env => [0, a];
var Bosatsu_Prog$raise_error = e => _env => [1, e];
var Bosatsu_Prog$read_env = env => [0, env];
var Bosatsu_Prog$flat_map = (prog, fn) => env => {
  const result = prog(env);
  if (result[0] === 0) {
    return fn(result[1])(env);
  }
  return result; // propagate error
};
var Bosatsu_Prog$recover = (prog, fn) => env => {
  const result = prog(env);
  if (result[0] === 1) {
    return fn(result[1])(env);
  }
  return result;
};
var Bosatsu_Prog$apply_fix = (a, fn) => {
  const fixed = x => fn(fixed)(x);
  return fixed(a);
};
var Bosatsu_Prog$remap_env = (p, f) => env => p(f(env));
var Bosatsu_Prog$println = str => _env => { console.log(_bosatsu_to_js_string(str)); return [0, []]; };
var Bosatsu_Prog$print = str => _env => { process.stdout.write(_bosatsu_to_js_string(str)); return [0, []]; };
var Bosatsu_Prog$read_stdin_utf8_bytes = n => _env => [0, [0]]; // Return empty string for now
"""

  /** Render the runtime library code */
  def renderRuntime: String = runtimeCode
}
