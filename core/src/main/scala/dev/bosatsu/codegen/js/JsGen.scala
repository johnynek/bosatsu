package dev.bosatsu.codegen.js

import cats.Monad
import cats.data.{NonEmptyList, State}
import dev.bosatsu.{Identifier, Lit, Matchless, PackageName}
import dev.bosatsu.Identifier.Bindable
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
      bindings: Map[Bindable, Code.Ident],
      nextTmp: Long,
      anonNames: Map[Long, Code.Ident]
    ) {
      def bind(b: Bindable): (EnvState, Code.Ident) = {
        val ident = escape(b)
        (copy(bindings = bindings + (b -> ident)), ident)
      }

      def deref(b: Bindable): Code.Ident =
        bindings.getOrElse(b, escape(b))

      def unbind(b: Bindable): EnvState =
        copy(bindings = bindings - b)

      def getNextTmp: (EnvState, Long) = {
        val id = nextTmp
        (copy(nextTmp = nextTmp + 1), id)
      }

      def anonName(id: Long): (EnvState, Code.Ident) = {
        anonNames.get(id) match {
          case Some(ident) => (this, ident)
          case None =>
            val ident = Code.Ident(s"_anon$id")
            (copy(anonNames = anonNames + (id -> ident)), ident)
        }
      }
    }

    object EnvState {
      def empty: EnvState = EnvState(Map.empty, 0L, Map.empty)
    }

    private case class EnvImpl[A](state: State[EnvState, A]) extends Env[A]

    private def env[A](fn: EnvState => (EnvState, A)): Env[A] =
      EnvImpl(State(fn))

    private def read[A](fn: EnvState => A): Env[A] =
      EnvImpl(State.inspect(fn))

    def bind(b: Bindable): Env[Code.Ident] = env(_.bind(b))
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

  /** JavaScript reserved words */
  val jsReservedWords: Set[String] = Set(
    "break", "case", "catch", "class", "const", "continue", "debugger", "default",
    "delete", "do", "else", "enum", "export", "extends", "false", "finally", "for",
    "function", "if", "import", "in", "instanceof", "let", "new", "null", "return",
    "static", "super", "switch", "this", "throw", "true", "try", "typeof", "undefined",
    "var", "void", "while", "with", "yield", "await", "arguments", "eval"
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

  // ==================
  // Code Generation
  // ==================

  /** Convert a Matchless literal to a JS literal */
  def literal(lit: Lit): Code.Expression = lit match {
    case Lit.Integer(i) => Code.IntLiteral(i)
    case Lit.Str(s) => Code.StringLiteral(s)
    case Lit.Chr(c) => Code.StringLiteral(c.toString)
  }

  /** Generate JavaScript for a Matchless expression */
  def exprToJs[A](expr: Matchless.Expr[A]): Env[Code.Expression] = {
    import Matchless._

    expr match {
      case Literal(lit) =>
        Env.pure(literal(lit))

      case Local(arg) =>
        Env.deref(arg)

      case Global(_, pack, name) =>
        // For now, just use the escaped name
        // Later we'll add module prefixes
        val ident = escape(name)
        Env.pure(ident)

      case LocalAnon(id) =>
        Env.anonName(id)

      case LocalAnonMut(id) =>
        Env.anonMutName(id)

      case ClosureSlot(idx) =>
        // Closure slots are accessed via a special array
        Env.pure(Code.IndexAccess(Code.Ident("_slots"), Code.IntLiteral(idx)))

      case Lambda(captures, recName, args, body) =>
        // Generate an arrow function
        // If there are captures, we need to wrap in a closure
        for {
          argIdents <- args.toList.traverse(Env.bind)
          bodyJs <- exprToJs(body)
          _ <- args.toList.traverse_(Env.unbind)
        } yield {
          // For now, simple arrow function
          val argNames = argIdents.map(_.name)
          Code.ArrowFunction(argNames, Left(bodyJs))
        }

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
        } yield exprJs === literal(lit)

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

      case MatchString(arg, parts, binds, _) =>
        // String matching - for now, just return false as placeholder
        // This needs proper implementation with regex or manual matching
        Env.pure(Code.FalseLit)

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
      val exportStmt = Code.Export(exports.head) // Simplified - export each separately
      Code.Statements(statements :+ exportStmt)
    }

    Code.render(allCode)
  }
}
