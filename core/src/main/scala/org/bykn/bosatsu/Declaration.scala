package org.bykn.bosatsu

import Parser.{ Combinators, Indy, lowerIdent, upperIdent, maybeSpace, spaces, toEOL }
import cats.data.NonEmptyList
import cats.implicits._
import com.stripe.dagon.Memoize
import fastparse.all._
import org.typelevel.paiges.{ Doc, Document }

import Indy.IndyMethods
import org.bykn.fastparse_cats.StringInstances._

/**
 * Represents the syntax version of Expr
 */
sealed abstract class Declaration {
  import Declaration._

  def region: Region

  def toDoc: Doc = {
    this match {
      case Apply(fn, args, dotApply) =>
        val fnDoc = fn match {
          case Var(n) => Doc.text(n)
          case Constructor(c) => Doc.text(c)
          case p@Parens(_) => p.toDoc
          case other => Doc.char('(') + other.toDoc + Doc.char(')')
        }

        val (prefix, body) =
          if (!dotApply) (fnDoc, args.toList)
          else (args.head.toDoc + Doc.char('.') + fnDoc, args.tail)

        body match {
          case Nil => prefix
          case notEmpty =>
            prefix + Doc.char('(') + Doc.intercalate(Doc.text(", "), notEmpty.map(_.toDoc)) + Doc.char(')')
        }
      case Binding(b) =>
        val d0 = Document[Padding[Declaration]]
        val withNewLine = Document.instance[Padding[Declaration]] { pd =>
           Doc.line + d0.document(pd)
        }
        BindingStatement.document(Document[Pattern[Option[String], TypeRef]], withNewLine).document(b)
      case Comment(c) =>
        CommentStatement.document[Padding[Declaration]].document(c)
      case Constructor(name) =>
        Doc.text(name)
      case DefFn(d) =>
        val pairDoc: Document[(OptIndent[Declaration], Padding[Declaration])] =
          Document.instance {
            case (fnBody, letBody) =>
                fnBody.sepDoc +
                Document[OptIndent[Declaration]].document(fnBody) +
                Doc.line +
                Document[Padding[Declaration]].document(letBody)
          }
        DefStatement.document(pairDoc).document(d)
      case IfElse(ifCases, elseCase) =>
        // TODO, we could make this ternary if it is small enough
        def checkBody(cb: (Declaration, OptIndent[Declaration])): Doc = {
          val (check, optbody) = cb
          val sep = optbody.sepDoc
          val rest = sep + Document[OptIndent[Declaration]].document(optbody)
          check.toDoc + Doc.char(':') + rest
        }

        val elseDoc = elseCase.sepDoc + Document[OptIndent[Declaration]].document(elseCase)
        val tail = Doc.text("else:") + elseDoc :: Nil
        val parts = (Doc.text("if ") + checkBody(ifCases.head)) :: (ifCases.tail.map(Doc.text("elif ") + checkBody(_))) ::: tail
        Doc.intercalate(Doc.line, parts)
      case Lambda(args, body) =>
        Doc.char('\\') + Doc.intercalate(Doc.text(", "), args.toList.map(Doc.text _)) + Doc.text(" -> ") + body.toDoc
      case Literal(lit) => Document[Lit].document(lit)
      case Match(kind, typeName, args) =>
        val pid = Document[OptIndent[Declaration]]

        implicit val patDoc: Document[(Pattern[Option[String], TypeRef], OptIndent[Declaration])] =
          Document.instance[(Pattern[Option[String], TypeRef], OptIndent[Declaration])] {
            case (pat, decl) =>
              Document[Pattern[Option[String], TypeRef]].document(pat) + Doc.text(":") + decl.sepDoc + pid.document(decl)
          }
        implicit def linesDoc[T: Document]: Document[NonEmptyList[T]] =
          Document.instance { ts => Doc.intercalate(Doc.line, ts.toList.map(Document[T].document _)) }

        val piPat = Document[OptIndent[NonEmptyList[(Pattern[Option[String], TypeRef], OptIndent[Declaration])]]]
        val kindDoc = kind match {
          case RecursionKind.NonRecursive => Doc.text("match ")
          case RecursionKind.Recursive => Doc.text("recur ")
        }
        // TODO this isn't quite right
        kindDoc + typeName.toDoc + Doc.char(':') + args.sepDoc +
          piPat.document(args)
      case Parens(p) =>
        Doc.char('(') + p.toDoc + Doc.char(')')
      case TupleCons(h :: Nil) =>
        // we need a trailing comma here:
        Doc.char('(') + h.toDoc + Doc.char(',') + Doc.char(')')
      case TupleCons(items) =>
        Doc.char('(') + Doc.intercalate(Doc.text(", "),
          items.map(_.toDoc)) + Doc.char(')')
      case Var(name) => Doc.text(name)

      case ListDecl(list) =>
        ListLang.document[Declaration, Pattern[Option[String], TypeRef]].document(list)
    }
  }

  def toExpr(
    nameToType: String => rankn.Type.Const,
    nameToCons: String => (PackageName, ConstructorName)): Expr[Declaration] = {

    def loop(decl: Declaration): Expr[Declaration] =
      decl match {
        case Apply(fn, args, _) =>
          @annotation.tailrec
          def loop0(fn: Expr[Declaration], args: List[Expr[Declaration]]): Expr[Declaration] =
            args match {
              case Nil => fn
              case h :: tail =>
                loop0(Expr.App(fn, h, decl), tail)
            }
          loop0(loop(fn), args.toList.map(loop(_)))
        case Binding(BindingStatement(pat, value, Padding(_, rest))) =>
          pat match {
            case Pattern.Var(arg) =>
              Expr.Let(arg, loop(value), loop(rest), RecursionKind.NonRecursive, decl)
            case pat =>
              val newPattern = unTuplePattern(pat, nameToType, nameToCons)
              val res = loop(rest)
              val expBranches = NonEmptyList.of((newPattern, res))
              Expr.Match(loop(value), expBranches, decl)
          }
        case Comment(CommentStatement(_, Padding(_, decl))) =>
          loop(decl).map(_ => decl)
        case Constructor(name) =>
          Expr.Var(None, name, decl)
        case DefFn(defstmt@DefStatement(_, _, _, _)) =>
          val (bodyExpr, inExpr) = defstmt.result match {
            case (oaBody, Padding(_, in)) =>
              (loop(oaBody.get), loop(in))
          }
          val lambda = defstmt.toLambdaExpr(bodyExpr, decl)(_.toType(nameToType))
          // we assume all defs are recursive: we put them in scope before the method
          // is called. We rely on DefRecursionCheck to rule out bad recursions
          Expr.Let(defstmt.name, lambda, inExpr, recursive = RecursionKind.Recursive, decl)
        case IfElse(ifCases, elseCase) =>
          def loop0(ifs: NonEmptyList[(Expr[Declaration], Expr[Declaration])], elseC: Expr[Declaration]): Expr[Declaration] =
            ifs match {
              case NonEmptyList((cond, ifTrue), Nil) =>
                Expr.ifExpr(cond, ifTrue, elseC, decl)
              case NonEmptyList(ifTrue, h :: tail) =>
                val elseC1 = loop0(NonEmptyList(h, tail), elseC)
                loop0(NonEmptyList.of(ifTrue), elseC1)
            }
          loop0(ifCases.map { case (d0, d1) =>
            (loop(d0), loop(d1.get))
          }, loop(elseCase.get))
        case Lambda(args, body) =>
          Expr.buildLambda(args.map((_, None)), loop(body), decl)
        case Literal(lit) =>
          Expr.Literal(lit, decl)
        case Parens(p) =>
          loop(p).map(_ => decl)
        case Var(name) =>
          Expr.Var(None, name, decl)
        case Match(_, arg, branches) =>
          /*
           * The recursion kind is only there for DefRecursionCheck, once
           * that passes, the expr only cares if lets are recursive or not
           */
          val expBranches = branches.get.map { case (pat, oidecl) =>
            val decl = oidecl.get
            val newPattern = unTuplePattern(pat, nameToType, nameToCons)
            (newPattern, loop(decl))
          }
          Expr.Match(loop(arg), expBranches, decl)
        case tc@TupleCons(its) =>
          val tup0: Expr[Declaration] = Expr.Var(Some(Predef.packageName), "Unit", tc)
          val tup2: Expr[Declaration] = Expr.Var(Some(Predef.packageName), "Tuple2", tc)
          def tup(args: List[Declaration]): Expr[Declaration] =
            args match {
              case Nil => tup0
              case h :: tail =>
                val tailExp = tup(tail)
                val headExp = loop(h)
                val withH = Expr.App(tup2, headExp, tc)
                Expr.App(withH, tailExp, tc)
            }

          tup(its)
        case l@ListDecl(list) =>
          list match {
            case ListLang.Cons(items) =>
              val revDecs: List[ListLang.SpliceOrItem[Expr[Declaration]]] = items.reverseMap {
                case ListLang.SpliceOrItem.Splice(s) =>
                  ListLang.SpliceOrItem.Splice(loop(s))
                case ListLang.SpliceOrItem.Item(item) =>
                  ListLang.SpliceOrItem.Item(loop(item))
              }

              val pn = Option(Predef.packageName)
              val empty: Expr[Declaration] = Expr.Var(pn, "EmptyList", l)
              def cons(head: Expr[Declaration], tail: Expr[Declaration]): Expr[Declaration] =
                Expr.App(Expr.App(Expr.Var(pn, "NonEmptyList", l), head, l), tail, l)

              def concat(headList: Expr[Declaration], tail: Expr[Declaration]): Expr[Declaration] =
                Expr.App(Expr.App(Expr.Var(pn, "concat", l), headList, l), tail, l)

              revDecs.foldLeft(empty) {
                case (tail, ListLang.SpliceOrItem.Item(i)) =>
                  cons(i, tail)
                case (tail, ListLang.SpliceOrItem.Splice(s)) =>
                  concat(s, tail)
              }
            case ListLang.Comprehension(res, binding, in, filter) =>
              /*
               * [x for y in z] ==
               * z.map_List(\v ->
               *   y = v
               *   x)
               *
               * [x for y in z if w] =
               * z.flat_map_List(\v ->
               *   y = v
               *   if w: [x]
               *   else: []
               * )
               *
               * [*x for y in z] =
               * z.flat_map_List(\v ->
               *   y = v
               *   x
               * )
               *
               * [*x for y in z if w] =
               * z.flat_map_List(\v ->
               *   y = v
               *   if w: x
               *   else: []
               * )
               */
              val pn = Option(Predef.packageName)
              val opName = (res, filter) match {
                case (ListLang.SpliceOrItem.Item(_), None) =>
                  "map_List"
                case (ListLang.SpliceOrItem.Item(_) | ListLang.SpliceOrItem.Splice(_), _) =>
                  "flat_map_List"
              }
              val opExpr: Expr[Declaration] = Expr.Var(pn, opName, l)
              val resExpr: Expr[Declaration] = (res, filter) match {
                case (itOrSp, None) =>
                  loop(itOrSp.value)
                case (itOrSp, Some(cond)) =>
                  val empty: Expr[Declaration] = Expr.Var(pn, "EmptyList", cond)
                  // we need theReturn
                  val r = itOrSp.value
                  val ritem = loop(r)
                  val sing = itOrSp match {
                    case ListLang.SpliceOrItem.Item(_) =>
                      Expr.App(
                        Expr.App(Expr.Var(pn, "NonEmptyList", r), ritem, r),
                        empty,
                        r)
                    case ListLang.SpliceOrItem.Splice(_) => ritem
                  }

                  Expr.If(loop(cond),
                    sing,
                    empty,
                    cond)
              }
              val unusedSymbol0: String = "$a" // TODO we should have better ways to gensym
              val newPattern = unTuplePattern(binding, nameToType, nameToCons)
              val body: Expr[Declaration] =
                Expr.Match(Expr.Var(None, unusedSymbol0, l),
                  NonEmptyList.of((newPattern, resExpr)), l)
              val fnExpr: Expr[Declaration] = Expr.Lambda(unusedSymbol0, body, l)
              Expr.App(Expr.App(opExpr, loop(in), l), fnExpr, l)
          }
      }

    loop(this)
  }
}

object Declaration {
  implicit val document: Document[Declaration] = Document.instance[Declaration](_.toDoc)
  implicit val hasRegion: HasRegion[Declaration] =
    HasRegion.instance[Declaration](_.region)

  /**
   * Tuples are converted into standard types using an HList strategy
   */
  def unTuplePattern(pat: Pattern[Option[String], TypeRef],
    nameToType: String => rankn.Type.Const,
    nameToCons: String => (PackageName, ConstructorName)): Pattern[(PackageName, ConstructorName), rankn.Type] =
      pat.mapStruct[(PackageName, ConstructorName)] {
        case (None, args) =>
          // this is a tuple pattern
          def loop(args: List[Pattern[(PackageName, ConstructorName), TypeRef]]): Pattern[(PackageName, ConstructorName), TypeRef] =
            args match {
              case Nil =>
                // ()
                Pattern.PositionalStruct(
                  (Predef.packageName, ConstructorName("Unit")),
                  Nil)
              case h :: tail =>
                val tailP = loop(tail)
                Pattern.PositionalStruct(
                  (Predef.packageName, ConstructorName("Tuple2")),
                  h :: tailP :: Nil)
            }

          loop(args)
        case (Some(nm), args) =>
          // this is a struct pattern
          Pattern.PositionalStruct(nameToCons(nm), args)
      }
      .mapType(_.toType(nameToType))

  //
  // We use the pattern of an implicit region for two reasons:
  // 1. we don't want the region to play a role in pattern matching or equality, since it is about
  //    error reporting, and if we include it in equality it massively complicates tests.
  // 2. we want to be able to construct these for tests dummy values, so we can set up an implicit
  //    value in tests and construct them.
  // These reasons are a bit abusive, and we may revisit this in the future
  //

  case class Apply(fn: Declaration, args: NonEmptyList[Declaration], useDotApply: Boolean)(implicit val region: Region) extends Declaration
  case class Binding(binding: BindingStatement[Pattern[Option[String], TypeRef], Padding[Declaration]])(implicit val region: Region) extends Declaration
  case class Comment(comment: CommentStatement[Padding[Declaration]])(implicit val region: Region) extends Declaration
  case class Constructor(name: String)(implicit val region: Region) extends Declaration
  case class DefFn(deffn: DefStatement[(OptIndent[Declaration], Padding[Declaration])])(implicit val region: Region) extends Declaration
  case class IfElse(ifCases: NonEmptyList[(Declaration, OptIndent[Declaration])],
    elseCase: OptIndent[Declaration])(implicit val region: Region) extends Declaration
  case class Lambda(args: NonEmptyList[String], body: Declaration)(implicit val region: Region) extends Declaration
  case class Literal(lit: Lit)(implicit val region: Region) extends Declaration
  case class Match(
    kind: RecursionKind,
    arg: Declaration,
    cases: OptIndent[NonEmptyList[(Pattern[Option[String], TypeRef], OptIndent[Declaration])]])(
    implicit val region: Region) extends Declaration
  case class Parens(of: Declaration)(implicit val region: Region) extends Declaration
  case class TupleCons(items: List[Declaration])(implicit val region: Region) extends Declaration
  case class Var(name: String)(implicit val region: Region) extends Declaration

  /**
   * This represents the list construction language
   */
  case class ListDecl(list: ListLang[Declaration, Pattern[Option[String], TypeRef]])(implicit val region: Region) extends Declaration

  val matchKindParser: P[RecursionKind] =
    (P("match").map(_ => RecursionKind.NonRecursive) | P("recur").map(_ => RecursionKind.Recursive))

  /**
   * A pattern can also be a declaration in some cases
   *
   * TODO, patterns don't parse with regions, so we lose track of precise position information
   * if we want to point to an inner portion of it
   */
  def toPattern(d: Declaration): Option[Pattern[Option[String], TypeRef]] =
    d match {
      case Var(v) => Some(Pattern.Var(v))
      case Literal(lit) => Some(Pattern.Literal(lit))
      case ListDecl(ListLang.Cons(elems)) =>
        val optParts: Option[List[Either[Option[String], Pattern[Option[String], TypeRef]]]] =
          elems.traverse {
            case ListLang.SpliceOrItem.Splice(Var(n)) =>
              Some(Left(Some(n)))
            case ListLang.SpliceOrItem.Item(p) =>
              toPattern(p).map(Right(_))
            case _ => None
          }
        optParts.map(Pattern.ListPat(_))
      case Constructor(nm) => Some(Pattern.PositionalStruct(Some(nm), Nil))
      case Apply(Constructor(nm), args, false) =>
        args.traverse(toPattern(_)).map { argPats =>
          Pattern.PositionalStruct(Some(nm), argPats.toList)
        }
      case TupleCons(ps) =>
        ps.traverse(toPattern(_)).map { argPats =>
          Pattern.PositionalStruct(None, argPats.toList)
        }
      case Parens(p) => toPattern(p)
      case _ => None
    }

  private val restP: Indy[Padding[Declaration]] =
    (Indy.parseIndent *> parser).mapF(Padding.parser(_))

  // This is something we check after variables
  private val bindingOp: Indy[(Pattern[Option[String], TypeRef], Region) => Binding] = {
    BindingStatement.bindingParser[Pattern[Option[String], TypeRef], Padding[Declaration]](parser <* Indy.lift(toEOL), restP)
      .region
      .map { case (region, fn) =>
        { (pat: Pattern[Option[String], TypeRef], r: Region) => Binding(fn(pat))(r + region) }
      }
  }

  val constructorP: P[Constructor] =
    upperIdent.region.map { case (r, c) => Constructor(c)(r) }.opaque("Constructor")

  val commentP: Parser.Indy[Comment] =
    CommentStatement.parser(Parser.Indy { indent => Padding.parser(P(indent ~ parser(indent)))})
      .region
      .map { case (r, c) => Comment(c)(r) }

  val defP: Indy[DefFn] = {
    val restParser: Indy[(OptIndent[Declaration], Padding[Declaration])] =
      OptIndent.indy(parser).product(Indy.lift(toEOL) *> restP)

    restParser.mapF { rp =>
      DefStatement.parser(maybeSpace ~ rp)
        .region
        .map { case (r, d) => DefFn(d)(r) }
    }
  }

  def ifElseP(expr: Indy[Declaration]): Indy[IfElse] = {

    def ifelif(str: String): Indy[(Declaration, OptIndent[Declaration])] =
      Indy.block(Indy.lift(P(str ~ spaces ~ maybeSpace)) *> expr, expr)

    /*
     * We don't need to parse the else term to the end,
     * EOL is a kind of a separator we only have in between
     */
    val elseTerm: Indy[OptIndent[Declaration]] =
      Indy.block(Indy.lift(P("else" ~ maybeSpace)), expr).map(_._2)

    val elifs1 = ifelif("elif").rep(1, sepIndy = Indy.toEOLIndent) <* Indy.toEOLIndent

    (ifelif("if") <* Indy.toEOLIndent)
      .product(elifs1.?)
      .product(elseTerm)
      .region
      .map {
        case (region, ((ifcase, optElses), elseBody)) =>
          val elses =
            optElses match {
              case None => Nil
              case Some(s) => s.toList // type inference works better than fold sadly
            }
          IfElse(NonEmptyList(ifcase, elses), elseBody)(region)
      }
  }

  val lambdaP: Indy[Lambda] = {
    val params = Indy.lift(P("\\" ~/ maybeSpace ~ lowerIdent.nonEmptyList))

    Indy.blockLike(params, parser, P(maybeSpace ~ "->"))
      .region
      .map { case (r, (args, body)) => Lambda(args, body.get)(r) }
  }

  def matchP(expr: Indy[Declaration]): Indy[Match] = {
    val withTrailing = expr <* Indy.lift(maybeSpace)
    val branch = Indy.block(Indy.lift(Pattern.parser), withTrailing)

    Indy.block(Indy.lift(P(matchKindParser ~ spaces)).product(withTrailing), branch.nonEmptyList(Indy.toEOLIndent))
      .region
      .map { case (r, ((kind, exp), branches)) =>
        Match(kind, exp, branches)(r)
      }
  }

  val varP: P[Var] =
    lowerIdent.region.map { case (r, v) => Var(v)(r) }

  private val patternBind: Indy[Declaration] =
    Indy.lift(Pattern.parser.region).product(bindingOp)
      .map {
        case ((reg, pat), fn) => fn(pat, reg)
      }

  private def decOrBind(p: P[Declaration], indent: String): P[Declaration] =
    (p ~ bindingOp(indent).?)
      .flatMap {
        case (d, None) => PassWith(d)
        case (d, Some(op)) =>
          toPattern(d) match {
            case None => Fail
            case Some(dpat) =>
              PassWith(op(dpat, d.region))
          }
      }

  private def listP(p: P[Declaration]): P[ListDecl] =
    ListLang.parser(p, Pattern.parser)
      .region
      .map { case (r, l) => ListDecl(l)(r) }

  private[this] val parserCache: String => P[Declaration] =
    Memoize.function[String, P[Declaration]] { (indent, rec) =>

      val recurse = P(rec(indent)) // needs to be inside a P for laziness

      val postOperators: List[P[Declaration => Declaration]] = {
        val params = recurse.nonEmptyList.parens
        // here we are using . syntax foo.bar(1, 2)
        val dotApply =
          P("." ~ varP ~ params.?).region.map { case (r2, (fn, argsOpt)) =>
            val args = argsOpt.fold(List.empty[Declaration])(_.toList)

            { head: Declaration => Apply(fn, NonEmptyList(head, args), true)(head.region + r2) }
          }.opaque(". apply operator")

        // here we directly call a function foo(1, 2)
        val applySuffix = params.region.map { case (r, args) =>

          { fn: Declaration => Apply(fn, args, false)(fn.region + r) }
        }.opaque("apply opereator")

        // here is if/ternary operator
        val ternary =
          P(spaces ~ "if" ~ spaces ~ recurse ~ spaces ~ "else" ~ spaces ~ recurse)
            .region
            .map { case (region, (cond, falseCase)) =>
              { trueCase: Declaration =>
                val ifcase = NonEmptyList.of((cond, OptIndent.same(trueCase)))
                IfElse(ifcase, OptIndent.same(falseCase))(trueCase.region + region)
              }
            }.opaque("ternary operator")

        dotApply :: applySuffix :: ternary :: Nil
      }

      val recIndy = Indy(rec)

      val lits = Lit.parser.region.map { case (r, l) => Literal(l)(r) }

      val tupOrPar =
        recurse
          .tupleOrParens
          .region
          .map {
            case (r, Left(p)) => Parens(p)(r)
            case (r, Right(tup)) => TupleCons(tup)(r)
          }

      /*
       * Note pattern bind needs to be before anything that looks like a pattern that can't handle
       * bind
       */
      val prefix = P(
        // these have keywords which need to be parsed before var (def, match, if)
        defP(indent) |
        lambdaP(indent) |
        matchP(recIndy)(indent) |
        ifElseP(recIndy)(indent) |
        // vars are so common, try to parse them before the generic pattern
        decOrBind(varP | listP(recurse), indent) |
        patternBind(indent) |
        lits | // technically this can be a pattern: 3 = x, so it has to be after patternBind, but it is never total.
        constructorP |
        commentP(indent) |
        tupOrPar)

      val opsList = postOperators.reduce(_ | _).rep().map(_.toList)

      @annotation.tailrec
      def loop[A](a: A, fns: List[A => A]): A =
        fns match {
          case Nil => a
          case h :: tail => loop(h(a), tail)
        }

      P(prefix ~ opsList)
        .map { case (arg, fns) => loop(arg, fns) }
        .opaque(s"Declaration.parser($indent)")
    }

  lazy val parser: Indy[Declaration] =
    Indy.suspend(Indy(parserCache))
}
