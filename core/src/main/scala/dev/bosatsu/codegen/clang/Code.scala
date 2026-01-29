package dev.bosatsu.codegen.clang

import cats.Monad
import cats.data.{NonEmptyList, NonEmptyChain}
import java.nio.charset.StandardCharsets
import org.typelevel.paiges.Doc
import scala.language.implicitConversions

import cats.syntax.all._

sealed trait Code

object Code {
  sealed trait Attr derives CanEqual
  object Attr {
    case object Static extends Attr

    private val staticDoc = Doc.text("static")

    def toDoc(a: Attr): Doc =
      a match {
        case Static => staticDoc
      }
  }

  sealed trait TypeIdent {
    def ptr: TypeIdent = TypeIdent.Ptr(this)

    def typedefAs(n: Ident): Statement = Typedef(this, n)

    def cast(expr: Expression): Expression = Cast(this, expr)
  }
  private val asterisk = Doc.char('*')

  object TypeIdent {
    sealed trait ComplexType extends TypeIdent
    case class StructType(name: String) extends ComplexType
    case class UnionType(name: String) extends ComplexType
    case class Named(name: String) extends TypeIdent
    case class Ptr(tpe: TypeIdent) extends TypeIdent
    val Int: TypeIdent = Named("int")
    val UInt32: TypeIdent = Named("uint32_t")
    val Char: TypeIdent = Named("char")
    val BValue: TypeIdent = Named("BValue")
    val AtomicBValue: TypeIdent = Named("_Atomic BValue")
    val Bool: TypeIdent = Named("_Bool")

    private val structDoc = Doc.text("struct ")
    private val unionDoc = Doc.text("union ")

    def toDoc(te: TypeIdent): Doc =
      te match {
        case StructType(n) => structDoc + Doc.text(n)
        case UnionType(n)  => unionDoc + Doc.text(n)
        case Named(n)      => Doc.text(n)
        case Ptr(ptr)      => toDoc(ptr) + asterisk
      }
  }

  sealed trait ValueLike {
    def +:(prefix: Statement): ValueLike =
      ValueLike.prefix(prefix, this)

    // returns something we can evaluate as an arg to if
    def returnsBool: Boolean =
      this match {
        case e: Expression        => e.evalToInt.isDefined
        case WithValue(_, v)      => v.returnsBool
        case IfElseValue(_, t, f) => t.returnsBool && f.returnsBool
      }

    def discardValue: Option[Statement] =
      this match {
        case _: Expression       => None
        case WithValue(stmt, vl) =>
          vl.discardValue match {
            case None      => Some(stmt)
            case Some(rhs) => Some(stmt + rhs)
          }
        case IfElseValue(cond, thenC, elseC) =>
          (thenC.discardValue, elseC.discardValue) match {
            case (Some(ts), Some(es)) => Some(ifThenElse(cond, ts, es))
            case (Some(ts), None)     =>
              Some(IfElse(NonEmptyList.one(cond -> block(ts)), None))
            case (None, Some(es)) =>
              // if (cond) {} else {es} == if (!cond) { es }
              Some(IfElse(NonEmptyList.one(!cond -> block(es)), None))
            case (None, None) => None
          }
      }

    def onExpr[F[_]: Monad](
        fn: Expression => F[ValueLike]
    )(newLocalName: String => F[Code.Ident]): F[ValueLike] =
      this match {
        case expr: Expression    => fn(expr)
        case WithValue(stmt, vl) =>
          vl.onExpr[F](fn)(newLocalName).map(stmt +: _)
        case branch @ IfElseValue(_, _, _) =>
          for {
            resIdent <- newLocalName("branch_res")
            value <- fn(resIdent)
          } yield (
            // Assign branchCond to a temp variable in both branches
            // and then use it so we don't exponentially blow up the code
            // size
            // TODO: not all onExpr uses have this type, we need to take type argument
            (Code.DeclareVar(Nil, Code.TypeIdent.BValue, resIdent, None) +
              (resIdent := branch)) +: value
          )
      }

    def exprToStatement[F[_]: Monad](
        fn: Expression => F[Statement]
    )(newLocalName: String => F[Code.Ident]): F[Statement] =
      this match {
        case expr: Expression    => fn(expr)
        case WithValue(stmt, vl) =>
          vl.exprToStatement[F](fn)(newLocalName).map(stmt + _)
        case branch @ IfElseValue(_, _, _) =>
          for {
            resIdent <- newLocalName("branch_res")
            last <- fn(resIdent)
          } yield
            // Assign branchCond to a temp variable in both branches
            // and then use it so we don't exponentially blow up the code
            // size
            (Code.DeclareVar(Nil, Code.TypeIdent.BValue, resIdent, None) +
              (resIdent := branch)) +
              last
      }
  }

  sealed trait Expression extends Code with ValueLike {
    def :=(rhs: ValueLike): Statement =
      ValueLike.assign(this, rhs)

    def ret: Statement = Return(Some(this))

    def apply(args: Expression*): Apply = Apply(this, args.toList)

    def select(i: Ident): Select = Select(this, i)

    def deref: Expression = PrefixExpr(PrefixUnary.Deref, this)

    def bracket(arg: Expression): Expression = Bracket(this, arg)

    def addr: Expression = PrefixExpr(PrefixUnary.Addr, this)

    def stmt: Statement = Effect(this)

    def castTo(tpe: TypeIdent): Expression = Cast(tpe, this)

    def bin(op: BinOp, rhs: Expression): Expression =
      BinExpr(this, op, rhs)

    def +(that: Expression): Expression = bin(BinOp.Add, that)
    def -(that: Expression): Expression = bin(BinOp.Sub, that)
    def *(that: Expression): Expression = bin(BinOp.Mult, that)
    def /(that: Expression): Expression = bin(BinOp.Div, that)
    def unary_! : Expression = PrefixExpr(PrefixUnary.Not, this)
    def =:=(that: Expression): Expression = bin(BinOp.Eq, that)
    def :<(that: Expression): Expression = bin(BinOp.Lt, that)
    def :>(that: Expression): Expression = bin(BinOp.Gt, that)

    def &&(that: Expression): Expression = bin(BinOp.And, that)

    def postInc: Expression = PostfixExpr(this, PostfixUnary.Inc)
    def postDec: Expression = PostfixExpr(this, PostfixUnary.Dec)

    lazy val evalToInt: Option[IntLiteral] =
      this match {
        case i @ IntLiteral(_) => Some(i)
        case _                 =>
          // TODO we can do a lot more here
          None
      }
  }

  def evalAnd(l: Expression, r: Expression): Expression =
    l.evalToInt match {
      case Some(IntLiteral(lv)) =>
        if (lv == 0) l
        else r
      case None =>
        r.evalToInt match {
          case Some(IntLiteral(rv)) =>
            if (rv == 0) r
            else l
          case None =>
            l && r
        }
    }
  /////////////////////////
  // Here are all the ValueLike
  /////////////////////////

  // this prepares an expression with a number of statements
  case class WithValue(statement: Statement, value: ValueLike) extends ValueLike
  // At least one of thenCond or elseCond should not be an expression
  case class IfElseValue(
      cond: Expression,
      thenCond: ValueLike,
      elseCond: ValueLike
  ) extends ValueLike

  object ValueLike {
    def applyArgs[F[_]: Monad](
        fn: ValueLike,
        args: NonEmptyList[ValueLike]
    )(newLocalName: String => F[Code.Ident]): F[ValueLike] =
      fn.onExpr { fnExpr =>
        def loop(
            rest: List[ValueLike],
            acc: NonEmptyList[Expression]
        ): F[ValueLike] =
          rest match {
            case Nil    => Monad[F].pure[ValueLike](fnExpr(acc.reverse.toList*))
            case h :: t =>
              h.onExpr(hexpr => loop(t, hexpr :: acc))(newLocalName)
          }

        args.head.onExpr { hexpr =>
          loop(args.tail, NonEmptyList.one(hexpr))
        }(newLocalName)
      }(newLocalName)

    def declareArray[F[_]: Monad](
        ident: Ident,
        tpe: TypeIdent,
        values: List[ValueLike]
    )(newLocalName: String => F[Code.Ident]): F[Statement] = {
      def loop(values: List[ValueLike], acc: List[Expression]): F[Statement] =
        values match {
          case Nil =>
            Monad[F].pure(DeclareArray(tpe, ident, Right(acc.reverse)))
          case (e: Expression) :: tail =>
            loop(tail, e :: acc)
          case h :: tail =>
            h.exprToStatement { e =>
              loop(tail, e :: acc)
            }(newLocalName)
        }

      loop(values, Nil)
    }

    def declareVar[F[_]: Monad](tpe: TypeIdent, ident: Ident, value: ValueLike)(
        newLocalName: String => F[Code.Ident]
    ): F[Statement] =
      value.exprToStatement[F] { expr =>
        Monad[F].pure(DeclareVar(Nil, tpe, ident, Some(expr)))
      }(newLocalName)

    def prefix(stmt: Statement, of: ValueLike): ValueLike =
      of match {
        case (_: Expression) | (_: IfElseValue) => WithValue(stmt, of)
        case WithValue(stmt1, v)                => WithValue(stmt + stmt1, v)
      }

    def assign(left: Expression, rhs: ValueLike): Statement =
      rhs match {
        case expr: Expression                => Assignment(left, expr)
        case WithValue(stmt, v)              => stmt + (left := v)
        case IfElseValue(cond, thenC, elseC) =>
          ifThenElse(cond, left := thenC, left := elseC)
      }

    def ifThenElseV[F[_]: Monad](
        cond: Code.ValueLike,
        thenC: Code.ValueLike,
        elseC: Code.ValueLike
    )(newLocalName: String => F[Code.Ident]): F[Code.ValueLike] =
      cond match {
        case expr: Code.Expression =>
          Monad[F].pure {
            expr.evalToInt match {
              case Some(IntLiteral(i)) =>
                if (i == 0) elseC
                else thenC
              case None =>
                (thenC, elseC) match {
                  case (thenX: Expression, elseX: Expression) =>
                    Ternary(expr, thenX, elseX)
                  case _ => IfElseValue(expr, thenC, elseC)
                }
            }
          }
        case Code.WithValue(stmt, v) =>
          ifThenElseV(v, thenC, elseC)(newLocalName).map(stmt +: _)
        case branchCond @ Code.IfElseValue(_, _, _) =>
          for {
            condIdent <- newLocalName("cond")
            res <- ifThenElseV(condIdent, thenC, elseC)(newLocalName)
          } yield {
            // Assign branchCond to a temp variable in both branches
            // and then use it so we don't exponentially blow up the code
            // size
            (Code.DeclareVar(Nil, Code.TypeIdent.Bool, condIdent, None) +
              (condIdent := branchCond)) +:
              res
          }
      }
  }

  def returnValue(vl: ValueLike): Statement =
    vl match {
      case expr: Expression                => Return(Some(expr))
      case WithValue(stmt, v)              => stmt + returnValue(v)
      case IfElseValue(cond, thenC, elseC) =>
        ifThenElse(cond, returnValue(thenC), returnValue(elseC))
    }

  sealed abstract class BinOp(repr: String) {
    val toDoc: Doc = Doc.text(repr)
  }
  object BinOp {
    case object Add extends BinOp("+")
    case object Sub extends BinOp("-")
    case object Mult extends BinOp("*")
    case object Div extends BinOp("/")
    case object Mod extends BinOp("%")

    case object Eq extends BinOp("==")
    case object NotEq extends BinOp("!=")
    case object Lt extends BinOp("<")
    case object LtEq extends BinOp("<=")
    case object Gt extends BinOp(">")
    case object GtEq extends BinOp(">=")

    case object Or extends BinOp("||")
    case object And extends BinOp("&&")

    case object BitOr extends BinOp("|")
    case object BitAnd extends BinOp("&")
    case object BitXor extends BinOp("^")
    case object LeftShift extends BinOp("<<")
    case object RightShift extends BinOp(">>")
  }

  sealed abstract class PrefixUnary(val repr: String) derives CanEqual {
    val toDoc: Doc = Doc.text(repr)
  }
  object PrefixUnary {
    // + - ! ~ ++ -- (type)* & sizeof
    case object Neg extends PrefixUnary("-")
    case object Not extends PrefixUnary("!")
    case object BitNot extends PrefixUnary("~")
    case object Inc extends PrefixUnary("++")
    case object Dec extends PrefixUnary("--")
    case object Addr extends PrefixUnary("&")
    case object Deref extends PrefixUnary("*")
  }

  sealed abstract class PostfixUnary(val repr: String) {
    val toDoc: Doc = Doc.text(repr)
  }
  object PostfixUnary {
    case object Inc extends PostfixUnary("++")
    case object Dec extends PostfixUnary("--")
  }

  case class Ident(name: String) extends Expression
  object Ident {
    implicit def fromString(str: String): Ident = Ident(str)
  }
  case class IntLiteral(value: BigInt) extends Expression
  case class StrLiteral(value: String) extends Expression
  object IntLiteral {
    val One: IntLiteral = IntLiteral(BigInt(1))
    val Zero: IntLiteral = IntLiteral(BigInt(0))

    def apply(i: Int): IntLiteral = IntLiteral(BigInt(i))
    def apply(i: Long): IntLiteral = IntLiteral(BigInt(i))
  }

  def TrueLit: Expression = IntLiteral.One
  def FalseLit: Expression = IntLiteral.Zero

  case class Cast(tpe: TypeIdent, expr: Expression) extends Expression
  case class Apply(fn: Expression, args: List[Expression]) extends Expression
  case class Select(target: Expression, name: Ident) extends Expression
  case class BinExpr(left: Expression, op: BinOp, right: Expression)
      extends Expression
  case class PrefixExpr(op: PrefixUnary, target: Expression) extends Expression
  case class PostfixExpr(target: Expression, op: PostfixUnary)
      extends Expression
  case class Bracket(target: Expression, item: Expression) extends Expression
  case class Ternary(
      cond: Expression,
      whenTrue: Expression,
      whenFalse: Expression
  ) extends Expression

  object Expression {
    implicit def expressionFromInt(i: Int): Expression =
      IntLiteral(i)
  }

  case class Param(tpe: TypeIdent, name: Ident) {
    def toDoc: Doc = TypeIdent.toDoc(tpe) + Doc.space + Doc.text(name.name)
  }

  sealed trait Statement extends Code {
    def +(stmt: Statement): Statement = Statements.combine(this, stmt)

    def ++(stmts: List[Statement]): Statement =
      NonEmptyList.fromList(stmts) match {
        case None      => this
        case Some(nel) => this + Statements(nel)
      }

    def maybeCombine(that: Option[Statement]): Statement =
      that match {
        case Some(t) => Statements.combine(this, t)
        case None    => this

      }
    def :+(vl: ValueLike): ValueLike = (this +: vl)
  }

  case class Assignment(target: Expression, value: Expression) extends Statement
  case class DeclareArray(
      tpe: TypeIdent,
      ident: Ident,
      values: Either[Int, List[Expression]]
  ) extends Statement
  case class DeclareVar(
      attrs: List[Attr],
      tpe: TypeIdent,
      ident: Ident,
      value: Option[Expression]
  ) extends Statement
  case class DeclareFn(
      attrs: List[Attr],
      returnTpe: TypeIdent,
      ident: Ident,
      args: List[Param],
      value: Option[Block]
  ) extends Statement
  case class Typedef(tpe: TypeIdent, name: Ident) extends Statement
  case class DefineComplex(
      tpe: TypeIdent.ComplexType,
      elements: List[(TypeIdent, Ident)]
  ) extends Statement
  case class Return(expr: Option[Expression]) extends Statement
  case class Block(items: NonEmptyList[Statement]) extends Statement {
    def doWhile(cond: Expression): Statement = DoWhile(this, cond)
  }
  // nothing more than a collection of statements
  case class Statements(items: NonEmptyChain[Statement]) extends Statement
  object Statements {
    def apply(nel: NonEmptyList[Statement]): Statements =
      Statements(NonEmptyChain.fromNonEmptyList(nel))

    def apply(first: Statement, rest: Statement*): Statements =
      Statements(NonEmptyChain.of(first, rest*))

    def combine(first: Statement, last: Statement): Statement =
      first match {
        case Statements(items) =>
          last match {
            case Statements(rhs) => Statements(items ++ rhs)
            case notStmts        => Statements(items :+ notStmts)
          }
        case notBlock =>
          last match {
            case Statements(rhs) => Statements(notBlock +: rhs)
            case notStmts => Statements(NonEmptyChain.of(notBlock, notStmts))
          }
      }
  }
  case class IfElse(
      ifs: NonEmptyList[(Expression, Block)],
      elseCond: Option[Block]
  ) extends Statement
  case class DoWhile(block: Block, whileCond: Expression) extends Statement
  case class Effect(expr: Expression) extends Statement
  case class While(cond: Expression, body: Block) extends Statement
  case class Include(quote: Boolean, filename: String) extends Statement
  object Include {
    def quote(filename: String): Include = Include(quote = true, filename)
    def angle(filename: String): Include = Include(quote = false, filename)
  }

  val returnVoid: Statement = Return(None)

  def declareInt(ident: Ident, init: Option[Int]): Statement =
    DeclareVar(Nil, TypeIdent.Int, ident, init.map(IntLiteral(_)))

  def declareBool(ident: Ident, init: Option[Boolean]): Statement =
    DeclareVar(
      Nil,
      TypeIdent.Bool,
      ident,
      init.map(if (_) TrueLit else FalseLit)
    )

  def declareMain(body: Statement): DeclareFn =
    DeclareFn(
      Nil,
      TypeIdent.Int,
      "main",
      Param(TypeIdent.Int, "argc") :: Param(
        TypeIdent.Char.ptr.ptr,
        "argv"
      ) :: Nil,
      Some(block(body))
    )

  def block(item: Statement, rest: Statement*): Block =
    item match {
      case block @ Block(_) if rest.isEmpty => block
      case _ => Block(NonEmptyList(item, rest.toList))
    }

  def ifThenElse(
      cond: Expression,
      thenCond: Statement,
      elseCond: Statement
  ): Statement =
    cond.evalToInt match {
      case Some(IntLiteral(i)) =>
        if (i == 0) elseCond
        else thenCond
      case None =>
        val first = cond -> block(thenCond)
        elseCond match {
          case IfElse(ifs, elseCond) =>
            IfElse(first :: ifs, elseCond)
          case notIfElse =>
            IfElse(NonEmptyList.one(first), Some(block(notIfElse)))
        }
    }
  private val equalsDoc = Doc.text(" = ")
  private val semiDoc = Doc.char(';')
  private val typeDefDoc = Doc.text("typedef ")
  private val leftCurly = Doc.char('{')
  private val rightCurly = Doc.char('}')
  private val leftPar = Doc.char('(')
  private val rightPar = Doc.char(')')
  private val leftBracket = Doc.char('[')
  private val rightBracket = Doc.char(']')
  private val dot = Doc.char('.')
  private val returnSemi = Doc.text("return;")
  private val returnSpace = Doc.text("return ")
  private val ifDoc = Doc.text("if ")
  private val elseIfDoc = Doc.text("else if ")
  private val elseDoc = Doc.text("else ")
  private val commaLine = Doc.char(',') + Doc.line
  private val doDoc = Doc.text("do ")
  private val whileDoc = Doc.text("while")
  private val arrow = Doc.text("->")
  private val questionDoc = Doc.text(" ?") + Doc.line
  private val colonDoc = Doc.text(" :") + Doc.line
  private val quoteDoc = Doc.char('"')
  private def leftAngleDoc = BinOp.Lt.toDoc
  private def rightAngleDoc = BinOp.Gt.toDoc
  private val includeDoc = Doc.text("#include")

  private def par(d: Doc): Doc = leftPar + d + rightPar

  private def curlyBlock[A](items: Iterable[A])(fn: A => Doc): Doc =
    if (items.isEmpty) {
      leftCurly + rightCurly
    } else {
      val inner =
        (Doc.line + Doc.intercalate(Doc.line, items.map(fn))).nested(4)

      leftCurly + inner + Doc.line + rightCurly
    }

  object Tight {
    // These are the highest priority, so safe to not use a parens
    def unapply(e: Expression): Option[Expression] =
      e match {
        case noPar @ (Ident(_) | Apply(_, _) | Select(_, _) | Bracket(_, _) |
            IntLiteral(_)) =>
          Some(noPar)
        case _ => None
      }
  }

  def toDoc(c: Code): Doc =
    c match {
      case Ident(n)        => Doc.text(n)
      case IntLiteral(bi)  => Doc.str(bi)
      case StrLiteral(str) =>
        val result = new java.lang.StringBuilder()
        val bytes = str.getBytes(StandardCharsets.UTF_8)
        bytes.foreach { c =>
          val cint = c.toInt & 0xff
          if (cint == 92) { // this is \
            result.append("\\\\")
          } else if (cint == 34) { // this is "
            result.append("\\\"")
          } else if (25 <= cint && cint <= 126) {
            result.append(cint.toChar)
          } else {
            result.append(
              s"\" \"\\x${java.lang.Integer.toHexString(cint)}\" \""
            )
          }
        }
        quoteDoc + (Doc.text(result.toString()) + quoteDoc)
      case Cast(tpe, expr) =>
        val edoc = expr match {
          case Ident(n) => Doc.text(n)
          case _        => par(toDoc(expr))
        }
        par(TypeIdent.toDoc(tpe)) + edoc
      case Apply(fn, args) =>
        val fnDoc = fn match {
          case Ident(n) => Doc.text(n)
          case notIdent => par(toDoc(notIdent))
        }
        fnDoc + par(
          Doc
            .intercalate(commaLine, args.map(expr => toDoc(expr)))
            .grouped
            .nested(4)
        )
      case PostfixExpr(expr, op) =>
        val left = expr match {
          case Ident(n) => Doc.text(n)
          case notIdent => par(toDoc(notIdent))
        }
        left + op.toDoc
      case PrefixExpr(op, expr) =>
        val right = expr match {
          case Tight(n) => toDoc(n)
          case usePar   => par(toDoc(usePar))
        }
        op.toDoc + right
      case BinExpr(left, op, right) =>
        val leftD = left match {
          case Tight(n) => toDoc(n)
          case usePar   => par(toDoc(usePar))
        }
        val rightD = right match {
          case Tight(n) => toDoc(n)
          case usePar   => par(toDoc(usePar))
        }
        leftD + Doc.space + op.toDoc + Doc.space + rightD
      case Select(target, Ident(nm)) =>
        target match {
          case PrefixExpr(PrefixUnary.Deref, Tight(noPar)) =>
            toDoc(noPar) + arrow + Doc.text(nm)
          case PrefixExpr(PrefixUnary.Deref, notIdent) =>
            par(toDoc(notIdent)) + arrow + Doc.text(nm)
          case Tight(noPar) => toDoc(noPar) + dot + Doc.text(nm)
          case notIdent     => par(toDoc(notIdent)) + dot + Doc.text(nm)
        }
      case Bracket(target, item) =>
        val left = target match {
          case Tight(noPar) => toDoc(noPar)
          case yesPar       => par(toDoc(yesPar))
        }
        left + leftBracket + toDoc(item) + rightBracket
      case Ternary(cond, t, f) =>
        def d(e: Expression): Doc =
          e match {
            case noPar @ (Tight(_) | PrefixExpr(_, _) | BinExpr(_, _, _)) =>
              toDoc(noPar)
            case yesPar => par(toDoc(yesPar))
          }
        (d(cond) + (questionDoc + d(t) + colonDoc + d(f)).nested(4)).grouped
      // Statements
      case Assignment(t, v) => toDoc(t) + (equalsDoc + (toDoc(v) + semiDoc))
      case DeclareArray(tpe, nm, values) =>
        // Foo bar[size] = {v(0), v(1), ...};
        // or
        // Foo bar[size];
        val tpeName = TypeIdent.toDoc(tpe) + Doc.space + toDoc(nm)
        values match {
          case Right(init) =>
            val len = init.size
            val begin = tpeName + leftBracket + Doc.str(
              len
            ) + rightBracket + equalsDoc + leftCurly;
            val items =
              if (init.isEmpty) Doc.empty
              else {
                ((Doc.line + Doc.intercalate(
                  commaLine,
                  init.map(e => toDoc(e))
                )).nested(4) + Doc.line).grouped
              }

            begin + items + rightCurly + semiDoc
          case Left(len) =>
            tpeName + leftBracket + Doc.str(len) + rightBracket + semiDoc
        }
      case DeclareVar(attrs, tpe, ident, v) =>
        val attrDoc =
          if (attrs.isEmpty) Doc.empty
          else {
            Doc.intercalate(
              Doc.space,
              attrs.map(a => Attr.toDoc(a))
            ) + Doc.space
          }

        val prefix = Doc.intercalate(
          Doc.space,
          (attrDoc + TypeIdent.toDoc(tpe)) ::
            toDoc(ident) ::
            Nil
        )

        v match {
          case Some(rhs) => prefix + equalsDoc + toDoc(rhs) + semiDoc
          case None      => prefix + semiDoc
        }
      case DeclareFn(attrs, tpe, ident, args, v) =>
        val attrDoc =
          if (attrs.isEmpty) Doc.empty
          else {
            Doc.intercalate(
              Doc.space,
              attrs.map(a => Attr.toDoc(a))
            ) + Doc.space
          }

        val paramDoc =
          Doc.intercalate(commaLine, args.map(_.toDoc)).nested(4).grouped

        val prefix = Doc.intercalate(
          Doc.space,
          (attrDoc + TypeIdent.toDoc(tpe)) ::
            (toDoc(ident) + par(paramDoc)) ::
            Nil
        )

        v match {
          case Some(rhs) => prefix + Doc.space + toDoc(rhs)
          case None      => prefix + semiDoc
        }
      case Typedef(td, n) =>
        typeDefDoc + TypeIdent.toDoc(td) + Doc.space + toDoc(n) + semiDoc
      case DefineComplex(tpe, els) =>
        val pre = TypeIdent.toDoc(tpe) + Doc.space
        val code = pre + curlyBlock(els) { case (t, n) =>
          TypeIdent.toDoc(t) + Doc.space + toDoc(n) + semiDoc
        }

        code + semiDoc
      case Return(opt) =>
        opt match {
          case None       => returnSemi
          case Some(expr) => returnSpace + toDoc(expr) + semiDoc
        }
      case Block(items)      => curlyBlock(items.toList)(s => toDoc(s))
      case Statements(items) =>
        Doc.intercalate(Doc.line, items.toNonEmptyList.toList.map(toDoc(_)))
      case IfElse(ifs, els) =>
        // "if (ex) {} else if"
        val (fcond, fblock) = ifs.head
        val first = ifDoc + par(toDoc(fcond)) + Doc.space + toDoc(fblock)
        val middle = ifs.tail match {
          case Nil      => Doc.empty
          case nonEmpty =>
            Doc.line + Doc.intercalate(
              Doc.line,
              nonEmpty.map { case (c, b) =>
                elseIfDoc + par(toDoc(c)) + Doc.space + toDoc(b)
              }
            )
        }
        val end = els match {
          case None    => Doc.empty
          case Some(e) => Doc.line + elseDoc + toDoc(e)
        }

        first + middle + end
      case DoWhile(block, cond) =>
        doDoc + toDoc(block) + Doc.space + whileDoc + par(toDoc(cond)) + semiDoc
      case Effect(expr) =>
        toDoc(expr) + semiDoc
      case While(expr, block) =>
        whileDoc + Doc.space + par(toDoc(expr)) + Doc.space + toDoc(block)
      case Include(useQuote, file) =>
        val inc = if (useQuote) {
          quoteDoc + Doc.text(file) + quoteDoc
        } else {
          leftAngleDoc + Doc.text(file) + rightAngleDoc
        }
        includeDoc + Doc.space + inc
    }
}
