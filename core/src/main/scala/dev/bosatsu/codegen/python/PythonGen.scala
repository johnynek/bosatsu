package dev.bosatsu.codegen.python

import cats.Monad
import cats.data.{NonEmptyList, State}
import cats.parse.{Parser => P}
import dev.bosatsu.{PackageName, Identifier, Matchless, Par, Parser}
import dev.bosatsu.codegen.{
  CompilationNamespace,
  CompilationSource,
  Idents
}
import dev.bosatsu.pattern.StrPart
import dev.bosatsu.rankn.Type
import org.typelevel.paiges.Doc
import scala.collection.immutable.SortedMap

import Identifier.Bindable
import Matchless._

import cats.implicits._

object PythonGen {
  import Code.{ValueLike, Statement, Expression}

  type Module = NonEmptyList[Code.Ident]

  sealed abstract class Env[+A]
  object Env {
    import Code._

    def pure[A](a: A): Env[A] = envMonad.pure(a)

    implicit def envMonad: Monad[Env] =
      new Monad[Env] {
        import Impl._

        type EnvStateS[A] = State[EnvState, A]
        val m = Monad[EnvStateS]
        def pure[A](a: A): Env[A] = EnvImpl(m.pure(a))
        override def map[A, B](ea: Env[A])(fn: A => B): Env[B] =
          EnvImpl(m.map(ea.state)(fn))
        def flatMap[A, B](ea: Env[A])(fn: A => Env[B]): Env[B] =
          EnvImpl(m.flatMap(ea.state)(fn.andThen(_.state)))
        def tailRecM[A, B](a: A)(fn: A => Env[Either[A, B]]): Env[B] =
          EnvImpl(m.tailRecM(a)(fn.andThen(_.state)))
      }

    private object Impl {

      case class BindState(
          binding: Bindable,
          count: Int,
          stack: List[Code.Ident]
      ) {
        def currentOption: Option[Code.Ident] = stack.headOption

        def current: Code.Ident =
          stack match {
            case h :: _ => h
            case Nil    =>
              sys.error(
                s"invariant violation: $binding, count = $count has no bindings."
              )
          }

        def next: (BindState, Code.Ident) = {
          val pname =
            Code.Ident(Idents.escape("___b", binding.asString + count.toString))
          (copy(count = count + 1, stack = pname :: stack), pname)
        }

        def push(ident: Code.Ident): BindState =
          copy(stack = ident :: stack)

        def pop: BindState =
          stack match {
            case _ :: tail => copy(stack = tail)
            case Nil       =>
              sys.error(
                s"invariant violation: $binding, count = $count has no bindings to pop"
              )
          }
      }
      object BindState {
        def empty(b: Bindable): BindState = BindState(b, 0, Nil)
      }

      case class EnvState(
          imports: Map[Module, Code.Ident],
          bindings: Map[Bindable, BindState],
          tops: Set[Bindable],
          nextTmp: Long,
          lifted: Vector[Statement]
      ) {

        def bind(b: Bindable): (EnvState, Code.Ident) = {
          val bs = bindings.getOrElse(b, BindState.empty(b))
          val (bs1, pname) = bs.next

          (
            copy(
              bindings = bindings.updated(b, bs1)
            ),
            pname
          )
        }

        def bindTo(b: Bindable, ident: Code.Ident): EnvState = {
          val bs = bindings.getOrElse(b, BindState.empty(b))
          copy(bindings = bindings.updated(b, bs.push(ident)))
        }

        def deref(b: Bindable): Code.Ident =
          // see if we are shadowing, or top level
          bindings.get(b).flatMap(_.currentOption) match {
            case Some(h)      => h
            case _ if tops(b) => escape(b)
            case other        =>
              // $COVERAGE-OFF$
              throw new IllegalStateException(
                s"unexpected deref: $b with bindings: $other, in $this"
              )
            // $COVERAGE-ON$
          }

        def unbind(b: Bindable): EnvState =
          bindings.get(b) match {
            case Some(bs) =>
              copy(bindings = bindings.updated(b, bs.pop))
            case other =>
              // $COVERAGE-OFF$
              throw new IllegalStateException(
                s"invalid scope: $other for $b with $bindings"
              )
            // $COVERAGE-ON$
          }

        def getNextTmp: (EnvState, Long) =
          (copy(nextTmp = nextTmp + 1L), nextTmp)

        def topLevel(b: Bindable): (EnvState, Code.Ident) =
          (copy(tops = tops + b), escape(b))

        def addImport(mod: Module): (EnvState, Code.Ident) =
          imports.get(mod) match {
            case Some(alias) => (this, alias)
            case None        =>
              val impNumber = imports.size
              val alias = Code.Ident(
                Idents.escape("___i", mod.last.name + impNumber.toString)
              )
              (copy(imports = imports.updated(mod, alias)), alias)
          }

        def lift(stmt: Statement): EnvState =
          copy(lifted = lifted :+ stmt)

        def importStatements: List[Code.Import] =
          imports.iterator
            .map { case (path, alias) =>
              val modName = path.map(_.name).toList.mkString(".")
              Code.Import(modName, Some(alias))
            }
            .toList
            .sortBy(_.modname)
      }

      implicit class EnvOps[A](val env: Env[A]) extends AnyVal {
        def state: State[EnvState, A] =
          env.asInstanceOf[EnvImpl[A]].state
      }

      def emptyState: EnvState =
        EnvState(Map.empty, Map.empty, Set.empty, 0L, Vector.empty)

      case class EnvImpl[A](state: State[EnvState, A]) extends Env[A]

      def env[A](fn: EnvState => (EnvState, A)): Env[A] =
        EnvImpl(State(fn))

      def read[A](fn: EnvState => A): Env[A] =
        EnvImpl(State(state => (state, fn(state))))

      def update(fn: EnvState => EnvState): Env[Unit] =
        EnvImpl(State(state => (fn(state), ())))

      def run[A](env: Env[A]): (EnvState, A) =
        env.state.run(emptyState).value
    }

    def render(env: Env[List[Statement]]): Doc = {
      val (state, stmts) = Impl.run(env)

      val imps = state.importStatements
      val lifted = state.lifted.toList
      val allStmts = lifted ::: stmts

      val impDocs = Doc.intercalate(Doc.hardLine, imps.map(Code.toDoc))
      val twoLines = Doc.hardLine + Doc.hardLine
      Doc.intercalate(twoLines, impDocs :: allStmts.map(Code.toDoc))
    }

    // allocate a unique identifier for b
    def bind(b: Bindable): Env[Code.Ident] =
      Impl.env(_.bind(b))

    def bindTo(b: Bindable, ident: Code.Ident): Env[Unit] =
      Impl.update(_.bindTo(b, ident))

    // get the mapping for a name in scope
    def deref(b: Bindable): Env[Code.Ident] =
      Impl.read(_.deref(b))

    // release the current scope for b
    def unbind(b: Bindable): Env[Unit] =
      Impl.update(_.unbind(b))

    def nameForAnon(long: Long): Env[Code.Ident] =
      Env.pure(Code.Ident(s"___a$long"))

    def newAssignableVar: Env[Code.Ident] =
      Impl
        .env(_.getNextTmp)
        .map { long =>
          Code.Ident(s"___t$long")
        }

    def newHoistedDefName: Env[Code.Ident] =
      Impl
        .env(_.getNextTmp)
        .map { long =>
          Code.Ident(s"___h$long")
        }

    def lift(stmt: Statement): Env[Unit] =
      Impl.update(_.lift(stmt))

    def importPackage(ident: NonEmptyList[String]): Env[Code.Ident] =
      importEscape(ident)

    def importEscape(parts: NonEmptyList[String]): Env[Code.Ident] =
      importLiteral(parts.map(escapeModule))

    def importLiteral(parts: NonEmptyList[Code.Ident]): Env[Code.Ident] =
      Impl.env(_.addImport(parts))

    // top level names are imported across files so they have
    // to be consistently transformed
    def topLevelName(n: Bindable): Env[Code.Ident] =
      Impl.env(_.topLevel(n))

    def onLastsM(
        cs: List[ValueLike]
    )(fn: List[Expression] => Env[ValueLike]): Env[ValueLike] = {
      def loop(
          cs: List[ValueLike],
          setup: List[Statement],
          args: List[Expression]
      ): Env[ValueLike] =
        cs match {
          case Nil =>
            val res = fn(args.reverse)
            NonEmptyList.fromList(setup) match {
              case None      => res
              case Some(nel) =>
                val stmts = nel.reverse
                val stmt = Code.block(stmts.head, stmts.tail*)
                res.map(stmt.withValue(_))
            }
          case (e: Expression) :: t            => loop(t, setup, e :: args)
          case (ifelse @ IfElse(_, _)) :: tail =>
            // we allocate a result and assign
            // the result on each value
            Env.newAssignableVar.flatMap { v =>
              loop(tail, (v := ifelse) :: setup, v :: args)
            }
          case WithValue(decl, v) :: tail =>
            loop(v :: tail, decl :: setup, args)
        }

      loop(cs, Nil, Nil)
    }

    def onLasts(cs: List[ValueLike])(
        fn: List[Expression] => ValueLike
    ): Env[ValueLike] =
      onLastsM(cs)(fn.andThen(Env.pure(_)))

    def onLastM(
        c: ValueLike
    )(fn: Expression => Env[ValueLike]): Env[ValueLike] =
      onLastsM(c :: Nil) {
        case x :: Nil => fn(x)
        case other    =>
          // $COVERAGE-OFF$
          throw new IllegalStateException(
            s"expected list to have size 1: $other"
          )
        // $COVERAGE-ON$
      }

    def onLast(c: ValueLike)(fn: Expression => ValueLike): Env[ValueLike] =
      onLastM(c)(fn.andThen(Env.pure(_)))

    def onLast2(c1: ValueLike, c2: ValueLike)(
        fn: (Expression, Expression) => ValueLike
    ): Env[ValueLike] =
      onLasts(c1 :: c2 :: Nil) {
        case x1 :: x2 :: Nil => fn(x1, x2)
        case other           =>
          // $COVERAGE-OFF$
          throw new IllegalStateException(
            s"expected list to have size 2: $other"
          )
        // $COVERAGE-ON$
      }

    def ifElse1(
        cond: ValueLike,
        tCase: ValueLike,
        fCase: ValueLike
    ): Env[ValueLike] =

      cond match {
        case cx: Expression =>
          Env.pure(Code.ValueLike.ifThenElse(cx, tCase, fCase))
        case WithValue(cs, cv) =>
          // Nest into the condition
          ifElse1(cv, tCase, fCase).map(cs.withValue(_))
        case ife @ IfElse(ccond, celse) if ife.returnsBool =>
          // this is basically the distributive property over if/else
          // if (if (c) then x else y) then z else w ==
          // if (c) then (if x then z else w) else (if y then z else w)
          (
            ccond.traverse { case (c, ct) =>
              ifElse1(ct, tCase, fCase)
                .map(r => (c, r))
            },
            ifElse1(celse, tCase, fCase)
          )
            .flatMapN { (conds, elseCase) =>
              ifElse(conds, elseCase)
            }
        case _ =>
          for {
            // allocate a new unshadowable var
            cv <- Env.newAssignableVar
            res <- ifElse1(cv, tCase, fCase)
          } yield (cv := cond).withValue(res)
      }

    def ifElse(
        conds: NonEmptyList[(ValueLike, ValueLike)],
        elseV: ValueLike
    ): Env[ValueLike] = {
      // for all the non-expression conditions, we need to defer evaluating them
      // until they are really needed
      val (c, t) = conds.head
      NonEmptyList.fromList(conds.tail) match {
        case Some(rest) =>
          ifElse(rest, elseV).flatMap { fcase =>
            ifElse1(c, t, fcase)
          }
        case None => ifElse1(c, t, elseV)
      }
    }

    def ifElseS(
        cond: ValueLike,
        thenS: Statement,
        elseS: Statement
    ): Env[Statement] =
      cond match {
        case x: Expression       => Env.pure(Code.ifElseS(x, thenS, elseS))
        case WithValue(stmt, vl) =>
          ifElseS(vl, thenS, elseS).map(stmt +: _)
        case ife @ IfElse(ifs, elseCond) if ife.returnsBool =>
          // every branch has a statically known boolean result
          (
            ifs.traverse { case (cond, t) =>
              ifElseS(t, thenS, elseS)
                .map(s => (cond, s))
            },
            ifElseS(elseCond, thenS, elseS)
          )
            .mapN { (ifs, elseCond) =>
              Code.ifStatement(ifs, Some(elseCond))
            }
        case v =>
          // this is a branch, don't multiply code by writing on each
          // branch, that could give an exponential blowup
          Env.newAssignableVar
            .map { tmp =>
              Code.block(
                tmp := v,
                Code.ifElseS(tmp, thenS, elseS)
              )
            }
      }

    def andCode(c1: ValueLike, c2: ValueLike): Env[ValueLike] =
      (c1, c2) match {
        case (e1: Expression, _) =>
          c2 match {
            case e2: Expression => Env.pure(e1.evalAnd(e2))
            case _              =>
              // and(x, y) == if x: y else: False
              Env.pure(Code.ValueLike.ifThenElse(e1, c2, Code.Const.False))
          }
        case (ife @ IfElse(cs, e), x2)
            if ife.returnsBool || x2.isInstanceOf[Expression] =>
          // push down into the lhs since this won't increase the final branch count
          (
            cs.traverse { case (c, t) => andCode(t, x2).map(t => (c, t)) },
            andCode(e, x2)
          ).mapN(IfElse(_, _))
        case (WithValue(s, c1), c2) =>
          andCode(c1, c2).map(s.withValue(_))
        case _ =>
          // we don't nest pairs of IfElse if the tails can't be evaluated now
          // since that could cause an exponential explosion of code size
          Env.onLastM(c1)(andCode(_, c2))
      }

    def makeDef(
        defName: Code.Ident,
        arg: NonEmptyList[Code.Ident],
        v: ValueLike
    ): Code.Def =
      Code.Def(defName, arg.toList, toReturn(v))

  }
  // we escape by prefixing by three underscores, ___ and n (for name)
  // we use other ___x escapes for different name spaces, e.g. tmps, and anons
  // then we escape _ by __ and any character outside the allowed
  // range by _base 62_
  // ___t: tmp
  // ___a anons
  // ___n: name
  // ___m: modules
  // ___i: import alias
  // ___b: shadowable (internal) names
  def escape(n: Bindable): Code.Ident = {
    val str = n.asString
    if (
      !str.startsWith("___") && Code.python2Name.matcher(str).matches && !Code
        .pyKeywordList(str)
    ) Code.Ident(str)
    else {
      // we need to escape
      Code.Ident(Idents.escape("___n", str))
    }
  }

  def escapeModule(str: String): Code.Ident =
    if (
      !str.startsWith("___") && Code.python2Name.matcher(str).matches && !Code
        .pyKeywordList(str)
    ) Code.Ident(str)
    else {
      // we need to escape
      Code.Ident(Idents.escape("___m", str))
    }

  /** Remap is used to handle remapping external values
    */
  private def stmtOf[K](
      packName: PackageName,
      name: Bindable,
      me: Expr[K],
      ns: CompilationNamespace[K]
  )(
      remap: (PackageName, Bindable) => Env[Option[ValueLike]]
  ): Env[Statement] = {
    val ops = new Impl.Ops(packName, remap, ns)

    // if we have a top level let rec with the same name, handle it more cleanly
    me match {
      // NOTE: MatchlessFromTypedExpr lifts free variables into Lambda.captures,
      // so top-level bindings that produce functions show up as Lambda (or the
      // let-rec pattern above). We don't expect general Let-chains ending in a
      // Lambda here, so we fall through to ops.loop below.
      case fn: Lambda[K] =>
        for {
          nm <- Env.topLevelName(name)
          res <- ops.topFn(nm, fn, None, None)
        } yield res
      case _ =>
        for {
          // name is not in scope yet
          ve <- ops.loop(me, None, None)
          nm <- Env.topLevelName(name)
        } yield nm := ve
    }
  }

  private def addUnitTest(name: Bindable): Env[Statement] =
    // we could inspect the Expr, but for now, we will just put
    // everything in a single test:
    // class BosatsuTests(unittest.TestCase):
    //   def test_all(self):
    //     # iterate through making assertions
    //
    (
      Env.importLiteral(NonEmptyList.one(Code.Ident("unittest"))),
      Env.newAssignableVar,
      Env.topLevelName(name)
    )
      .mapN { (importedName, tmpVar, testName) =>
        import Impl._

        val loopName = Code.Ident("test_loop")
        val argName = Code.Ident("value")
        val selfName = Code.Ident("self")

        val isAssertion: Code.Expression =
          argName.get(0) =:= 0

        // Assertion(bool, msg)
        val testAssertion: Code.Statement =
          Code.Call(
            Code.Apply(
              selfName.dot(Code.Ident("assertTrue")),
              argName.get(1) :: argName.get(2) :: Nil
            )
          )

        // TestSuite(suiteName, tests)
        val testSuite: Code.Statement =
          Code.block(
            tmpVar := argName.get(2), // get the test list
            Code.While(
              isNonEmpty(tmpVar),
              Code.block(
                Code.Call(Code.Apply(loopName, headList(tmpVar) :: Nil)),
                tmpVar := tailList(tmpVar)
              )
            )
          )

        val loopBody: Code.Statement =
          Code.IfStatement(
            NonEmptyList.one((isAssertion, testAssertion)),
            Some(testSuite)
          )

        val recTest =
          Code.Def(loopName, argName :: Nil, loopBody)

        val body =
          Code.block(recTest, Code.Call(Code.Apply(loopName, testName :: Nil)))

        val defBody =
          Code.Def(Code.Ident("test_all"), selfName :: Nil, body)

        Code.ClassDef(
          Code.Ident("BosatsuTests"),
          List(importedName.dot(Code.Ident("TestCase"))),
          defBody
        )
      }

  private def addMainEval(
      name: Bindable,
      mod: Module,
      ci: Code.Ident
  ): Env[Statement] =
    /*
     * this does:
     * if __name__ == "__main__":
     *   from Module import ci
     *   ci(name)
     */
    (Env.importLiteral(mod), Env.topLevelName(name))
      .mapN { (importedName, argName) =>
        Code.mainStatement(
          Code.Call(importedName.dot(ci)(argName))
        )
      }

  private val modParser: P[(Module, Code.Ident)] = {
    val identParser: P[Code.Ident] = Parser.py2Ident.map(Code.Ident(_))
    P.repSep(identParser, min = 2, sep = P.char('.'))
      .map { items =>
        // min = 2 ensures this is safe
        (NonEmptyList.fromListUnsafe(items.init.toList), items.last)
      }
  }

  // parses a nested map with
  //
  // { packageName: { bind: foo.bar.baz } }
  //
  val externalParser: P[List[(PackageName, Bindable, Module, Code.Ident)]] = {

    val inner: P[List[(Bindable, (Module, Code.Ident))]] =
      Parser.dictLikeParser(Identifier.bindableParser, modParser)

    val outer: P[List[(PackageName, List[(Bindable, (Module, Code.Ident))])]] =
      Parser.maybeSpacesAndLines.with1 *> Parser.dictLikeParser(
        PackageName.parser,
        inner
      ) <* Parser.maybeSpacesAndLines

    outer.map { items =>
      items.flatMap { case (p, bs) =>
        bs.map { case (b, (m, i)) => (p, b, m, i) }
      }
    }
  }

  // parses a map of of evaluators
  // { fullyqualifiedType: foo.bar.baz, }
  val evaluatorParser: P[List[(Type, (Module, Code.Ident))]] =
    Parser.maybeSpacesAndLines.with1 *> Parser.dictLikeParser(
      Type.fullyResolvedParser,
      modParser
    ) <* Parser.maybeSpacesAndLines

  def renderSource[S](
      src: S,
      externals: Map[(PackageName, Bindable), (Module, Code.Ident)],
      evaluators: Map[PackageName, (Bindable, Module, Code.Ident)]
  )(implicit
      CS: CompilationSource[S],
      ec: Par.EC
  ): SortedMap[CS.ScopeKey, Map[PackageName, (Module, Doc)]] = {
    val ns = CS.namespace(src)
    renderAll(ns, externals, evaluators)
  }

  // compile a set of packages given a set of external remappings
  def renderAll[K](
      ns: CompilationNamespace[K],
      externals: Map[(PackageName, Bindable), (Module, Code.Ident)],
      evaluators: Map[PackageName, (Bindable, Module, Code.Ident)]
  )(implicit ec: Par.EC): SortedMap[K, Map[PackageName, (Module, Doc)]] = {

    val pm = ns.compiled
    val externalRemap: (PackageName, Bindable) => Env[Option[ValueLike]] = {
      (p, b) =>
        externals.get((p, b)) match {
          case None         => Env.pure(None)
          case Some((m, i)) =>
            Env
              .importLiteral(m)
              .map(alias => Some(Code.DotSelect(alias, i)))
        }
    }

    val all = pm
      .transform { (k, pm) =>
        val testsK =
          if (ns.isRoot(k)) ns.testValues else Map.empty[PackageName, Nothing]
        val evaluatorsK =
          if (ns.isRoot(k)) evaluators else Map.empty[PackageName, Nothing]

        pm.toList
          .traverse { case (p, lets) =>
            Par.start {
              val stmts0: Env[List[Statement]] =
                lets
                  .traverse { case (b, x) =>
                    stmtOf(p, b, x, ns)(externalRemap)
                  }

              val evalStmt: Env[Option[Statement]] =
                evaluatorsK.get(p).traverse { case (b, m, c) =>
                  addMainEval(b, m, c)
                }

              val testStmt: Env[Option[Statement]] =
                testsK.get(p).traverse(addUnitTest)

              val stmts = (stmts0, testStmt, evalStmt)
                .mapN { (s, optT, optM) =>
                  s :++ optT.toList :++ optM.toList
                }

              def modName(p: NonEmptyList[String]): Module =
                p match {
                  case NonEmptyList(h, Nil) =>
                    val Code.Ident(m) = escapeModule(h)

                    NonEmptyList.one(Code.Ident(m + ".py"))
                  case NonEmptyList(h, t1 :: t2) =>
                    escapeModule(h) :: modName(NonEmptyList(t1, t2))
                }

              (p, (modName(ns.identOf(k, p)), Env.render(stmts)))
            }
          }
      }
      .sequence[Par.F, List[(PackageName, (Module, Doc))]]

    Par
      .await(all)
      .transform((_, vs) => vs.toMap)
  }

  // These are values replaced with python operations
  def intrinsicValues: Map[PackageName, Set[Bindable]] =
    Map(
      PackageName.PredefName -> Impl.PredefExternal.results.keySet,
      Impl.NumericExternal.NumericPackage -> Impl.NumericExternal.results.keySet
    )

  private object Impl {

    val emptyList: Expression =
      Code.MakeTuple(Code.fromInt(0) :: Nil)

    def consList(head: Expression, tail: Expression): Expression =
      Code.MakeTuple(List(Code.fromInt(1), head, tail))

    def isNonEmpty(expr: Expression): Expression =
      Code.Op(expr.get(0), Code.Const.Neq, Code.fromInt(0)).simplify

    def headList(lst: Expression): Expression =
      lst.get(1).simplify

    def tailList(lst: Expression): Expression =
      lst.get(2).simplify

    object PredefExternal {
      private val cmpFn: List[ValueLike] => Env[ValueLike] = { input =>
        Env.onLast2(input.head, input.tail.head) { (arg0, arg1) =>
          Code
            .Ternary(
              0,
              arg0 :< arg1,
              Code.Ternary(1, arg0 =:= arg1, 2)
            )
            .simplify
        }
      }

      val results: Map[Bindable, (List[ValueLike] => Env[ValueLike], Int)] =
        Map(
          (
            Identifier.unsafeBindable("add"),
            (
              input => Env.onLast2(input.head, input.tail.head)(_.evalPlus(_)),
              2
            )
          ),
          (
            Identifier.unsafeBindable("sub"),
            (
              { input =>
                Env.onLast2(input.head, input.tail.head)(_.evalMinus(_))
              },
              2
            )
          ),
          (
            Identifier.unsafeBindable("times"),
            (
              { input =>
                Env.onLast2(input.head, input.tail.head)(_.evalTimes(_))
              },
              2
            )
          ),
          (
            Identifier.unsafeBindable("div"),
            (
              { input =>
                Env.onLast2(input.head, input.tail.head) { (a, b) =>
                  Code
                    .Ternary(
                      Code.Op(a, Code.Const.Div, b),
                      b, // 0 is false in python
                      0
                    )
                    .simplify
                }
              },
              2
            )
          ),
          (
            Identifier.unsafeBindable("mod_Int"),
            (
              { input =>
                Env.onLast2(input.head, input.tail.head) { (a, b) =>
                  Code
                    .Ternary(
                      Code.Op(a, Code.Const.Mod, b),
                      b, // 0 is false in python
                      a
                    )
                    .simplify
                }
              },
              2
            )
          ),
          (Identifier.unsafeBindable("cmp_Int"), (cmpFn, 2)),
          (
            Identifier.unsafeBindable("eq_Int"),
            (
              { input =>
                Env.onLast2(input.head, input.tail.head)(
                  _.eval(Code.Const.Eq, _)
                )
              },
              2
            )
          ),
          (
            Identifier.unsafeBindable("shift_left_Int"),
            (
              { input =>
                Env.onLast2(input.head, input.tail.head)(
                  _.eval(Code.Const.BitwiseShiftLeft, _)
                )
              },
              2
            )
          ),
          (
            Identifier.unsafeBindable("shift_right_Int"),
            (
              { input =>
                Env.onLast2(input.head, input.tail.head)(
                  _.eval(Code.Const.BitwiseShiftRight, _)
                )
              },
              2
            )
          ),
          (
            Identifier.unsafeBindable("and_Int"),
            (
              { input =>
                Env.onLast2(input.head, input.tail.head)(
                  _.eval(Code.Const.BitwiseAnd, _)
                )
              },
              2
            )
          ),
          (
            Identifier.unsafeBindable("or_Int"),
            (
              { input =>
                Env.onLast2(input.head, input.tail.head)(
                  _.eval(Code.Const.BitwiseOr, _)
                )
              },
              2
            )
          ),
          (
            Identifier.unsafeBindable("xor_Int"),
            (
              { input =>
                Env.onLast2(input.head, input.tail.head)(
                  _.eval(Code.Const.BitwiseXor, _)
                )
              },
              2
            )
          ),
          (
            Identifier.unsafeBindable("not_Int"),
            (
              {
                // leverage not(x) == -1 - x
                input => Env.onLast(input.head)(Code.fromInt(-1).evalMinus(_))
              },
              2
            )
          ),
          (
            Identifier.unsafeBindable("gcd_Int"),
            (
              { input =>
                (
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar
                ).mapN { (tmpa, tmpb, tmpc) =>
                  Env.onLast2(input.head, input.tail.head) { (a, b) =>
                    Code
                      .block(
                        tmpa := a,
                        tmpb := b,
                        Code.While(
                          tmpb,
                          Code.block(
                            tmpc := tmpb,
                            // we know b != 0 because we are in the while loop
                            /// b = a % b
                            tmpb := Code.Op(tmpa, Code.Const.Mod, tmpb),
                            tmpa := tmpc
                          )
                        )
                      )
                      .withValue(tmpa)
                  }
                }.flatten
              },
              2
            )
          ),
          // external def int_loop(intValue: Int, state: a, fn: (Int, a) -> (Int, a) -> a
          // def int_loop(i, a, fn):
          //   if i <= 0: a
          //   else:
          //     (i1, a1) = fn(i, a)
          //     if i <= i1: a
          //     else int_loop(i1, a, fn)
          //
          // def int_loop(i, a, fn):
          //   cont = (0 < i)
          //   res = a
          //   _i = i
          //   _a = a
          //   while cont:
          //     res = fn(_i, _a)
          //     tmp_i = res[0]
          //     _a = res[1][0]
          //     cont = (0 < tmp_i) and (tmp_i < _i)
          //     _i = tmp_i
          //   return _a
          (
            Identifier.unsafeBindable("int_loop"),
            (
              { input =>
                (
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar
                ).tupled
                  .flatMap { case (cont, res, _i, _a, tmp_i) =>
                    Env.onLasts(input) {
                      case i :: a :: fn :: Nil =>
                        Code
                          .block(
                            cont := (Code.fromInt(0) :< i),
                            res := a,
                            _i := i,
                            _a := a,
                            Code.While(
                              cont,
                              fn(_i, _a).simplify match {
                                case Code.MakeTuple(fst :: snd :: Nil) =>
                                  // inline the tuple allocation and destructuring
                                  Code.block(
                                    tmp_i := fst,
                                    _a := snd,
                                    cont := (Code.fromInt(0) :< tmp_i)
                                      .evalAnd(tmp_i :< _i),
                                    _i := tmp_i
                                  )
                                case notTup =>
                                  Code.block(
                                    res := notTup,
                                    tmp_i := res.get(0),
                                    _a := res.get(1),
                                    cont := (Code.fromInt(0) :< tmp_i)
                                      .evalAnd(tmp_i :< _i),
                                    _i := tmp_i
                                  )
                              }
                            )
                          )
                          .withValue(_a)
                      case other =>
                        // $COVERAGE-OFF$
                        throw new IllegalStateException(
                          s"expected arity 3 got: $other"
                        )
                      // $COVERAGE-ON$
                    }
                  }
              },
              3
            )
          ),
          (
            Identifier.unsafeBindable("concat_String"),
            (
              { input =>
                Env.onLastM(input.head) { listOfStrings =>
                  // convert to python list, then call "".join(seq)
                  Env.newAssignableVar
                    .flatMap { pyList =>
                      bosatsuListToPython(pyList, listOfStrings)
                        .map { loop =>
                          Code
                            .block(
                              pyList := Code.MakeList(Nil),
                              loop
                            )
                            .withValue {
                              Code.PyString("").dot(Code.Ident("join"))(pyList)
                            }
                        }
                    }
                }
              },
              1
            )
          ),
          (
            Identifier.unsafeBindable("int_to_String"),
            (
              { input =>
                Env.onLast(input.head) {
                  case Code.PyInt(i) => Code.PyString(i.toString)
                  case i             =>
                    Code.Apply(Code.DotSelect(i, Code.Ident("__str__")), Nil)
                }
              },
              1
            )
          ),
          (
            Identifier.unsafeBindable("string_to_Int"),
            (
              { input =>
                Env.onLast(input.head) { s =>
                  // int(s) if (s[0] == '-' and s[1:].isdigit()) or s.isdigit() else None
                  val isdigit = Code.Ident("isdigit")
                  val isValid = Code.Op(
                    (s.get(0) =:= Code.PyString("-")).evalAnd(
                      Code
                        .SelectRange(s, Some(Code.Const.One), None)
                        .dot(isdigit)()
                    ),
                    Code.Const.Or,
                    s.dot(isdigit)()
                  )

                  Code.Ternary(
                    Code.MakeTuple(
                      Code.Const.One :: Code.Ident("int")(s) :: Nil
                    ),
                    isValid,
                    Code.MakeTuple(Code.Const.Zero :: Nil)
                  )
                }
              },
              1
            )
          ),
          (
            Identifier.unsafeBindable("char_to_String"),
            // we encode chars as strings so this is just identity
            ({ input => Env.envMonad.pure(input.head) }, 1)
          ),
          (
            Identifier.unsafeBindable("trace"),
            (
              { input =>
                Env.onLast2(input.head, input.tail.head) { (msg, i) =>
                  Code
                    .Call(Code.Apply(Code.Ident("print"), msg :: i :: Nil))
                    .withValue(i)
                }
              },
              2
            )
          ),
          (
            Identifier.unsafeBindable("partition_String"),
            (
              { input =>
                Env.newAssignableVar
                  .flatMap { res =>
                    Env.onLast2(input.head, input.tail.head) { (str, sep) =>
                      // if sep == "": None
                      // else:
                      //   (a, s1, b) = str.partition(sep)
                      //   if s1: (1, (a, b))
                      //   else: (0, )
                      val a = res.get(0)
                      val s1 = res.get(1)
                      val b = res.get(2)
                      val success = Code.MakeTuple(
                        Code.fromInt(1) ::
                          Code.MakeTuple(a :: b :: Nil) ::
                          Nil
                      )
                      val fail = Code.MakeTuple(Code.fromInt(0) :: Nil)
                      val nonEmpty =
                        (res := str.dot(Code.Ident("partition"))(sep))
                          .withValue(Code.Ternary(success, s1, fail))

                      Code.IfElse(NonEmptyList.one((sep, nonEmpty)), fail)
                    }
                  }
              },
              2
            )
          ),
          (
            Identifier.unsafeBindable("rpartition_String"),
            (
              { input =>
                Env.newAssignableVar
                  .flatMap { res =>
                    Env.onLast2(input.head, input.tail.head) { (str, sep) =>
                      // (a, s1, b) = str.partition(sep)
                      // if s1: (1, (a, b))
                      // else: (0, )
                      val a = res.get(0)
                      val s1 = res.get(1)
                      val b = res.get(2)
                      val success = Code.MakeTuple(
                        Code.fromInt(1) ::
                          Code.MakeTuple(a :: b :: Nil) ::
                          Nil
                      )
                      val fail = Code.MakeTuple(Code.fromInt(0) :: Nil)
                      val nonEmpty =
                        (res := str.dot(Code.Ident("rpartition"))(sep))
                          .withValue(Code.Ternary(success, s1, fail))

                      Code.IfElse(NonEmptyList.one((sep, nonEmpty)), fail)
                    }
                  }
              },
              2
            )
          ),
          (Identifier.unsafeBindable("cmp_String"), (cmpFn, 2))
        )

      def bosatsuListToPython(
          pyList: Code.Ident,
          bList: Expression
      ): Env[Statement] =
        Env.newAssignableVar
          .map { tmp =>
            // tmp = bList
            // while tmp != 0:
            //   //(_, h, tail) = tmp
            //   pyList.append(tmp[1])
            //   tmp = tmp[2]
            Code.block(
              tmp := bList,
              Code.While(
                isNonEmpty(tmp),
                Code.block(
                  Code.Call(pyList.dot(Code.Ident("append"))(headList(tmp))),
                  tmp := tailList(tmp)
                )
              )
            )
          }

      def unapply[A](
          expr: Expr[A]
      ): Option[(List[ValueLike] => Env[ValueLike], Int)] =
        expr match {
          case Global(_, PackageName.PredefName, name) => results.get(name)
          case _                                       => None
        }

      def makeLambda(
          arity: Int
      )(fn: List[ValueLike] => Env[ValueLike]): Env[ValueLike] =
        for {
          vars <- (1 to arity).toList.traverse(_ => Env.newAssignableVar)
          body <- fn(vars)
          // TODO: if body isn't an expression, how can just adding a lambda
          // at the end be correct? the arguments will be below points that used it.
          // the onLast has to handle Code.Lambda specially
          res <- Env.onLast(body)(Code.Lambda(vars, _))
        } yield res
    }

    /**
     * Intrinsic functions from Bosatsu/Numeric that are inlined as native Python operations.
     * These use symbolic operators (+., -., *., /.) to distinguish from Int ops.
     * Operations bypass RingOpt algebraic expansion - preserves original formulas.
     */
    object NumericExternal {
      val NumericPackage: PackageName = PackageName.parse("Bosatsu/Numeric").get

      private val cmpDoubleFn: List[ValueLike] => Env[ValueLike] = { input =>
        Env.onLast2(input.head, input.tail.head) { (arg0, arg1) =>
          Code
            .Ternary(
              0,
              arg0 :< arg1,
              Code.Ternary(1, arg0 =:= arg1, 2)
            )
            .simplify
        }
      }

      val results: Map[Bindable, (List[ValueLike] => Env[ValueLike], Int)] =
        Map(
          // Arithmetic - symbolic operators for Double (use Operator for symbolic names)
          (
            Identifier.Operator("+."),
            (
              input => Env.onLast2(input.head, input.tail.head)(_.evalPlus(_)),
              2
            )
          ),
          (
            Identifier.Operator("-."),
            (
              { input =>
                Env.onLast2(input.head, input.tail.head)(_.evalMinus(_))
              },
              2
            )
          ),
          (
            Identifier.Operator("*."),
            (
              { input =>
                Env.onLast2(input.head, input.tail.head)(_.evalTimes(_))
              },
              2
            )
          ),
          (
            Identifier.Operator("/."),
            (
              { input =>
                // Float division - use regular division (no truncation)
                Env.onLast2(input.head, input.tail.head) { (a, b) =>
                  Code.Op(a, Code.Const.Div, b)
                }
              },
              2
            )
          ),
          // Conversion (use Name for regular identifiers)
          (
            Identifier.Name("from_Int"),
            (
              // Python numbers are already floats when needed
              { input => Env.onLast(input.head)(identity) },
              1
            )
          ),
          (
            Identifier.Name("to_Int"),
            (
              // Truncate to integer using int()
              { input =>
                Env.onLast(input.head) { d =>
                  Code.Ident("int")(d)
                }
              },
              1
            )
          ),
          // Comparison
          (Identifier.Name("cmp_Double"), (cmpDoubleFn, 2)),
          (
            Identifier.Name("eq_Double"),
            (
              { input =>
                Env.onLast2(input.head, input.tail.head)(
                  _.eval(Code.Const.Eq, _)
                )
              },
              2
            )
          ),
          // Unary operations
          (
            Identifier.Name("neg_Double"),
            (
              { input =>
                Env.onLast(input.head)(d => Code.Op(Code.fromInt(0), Code.Const.Minus, d))
              },
              1
            )
          ),
          (
            Identifier.Name("abs_Double"),
            (
              { input =>
                Env.onLast(input.head)(d => Code.Ident("abs")(d))
              },
              1
            )
          )
        )

      def unapply[A](
          expr: Expr[A]
      ): Option[(List[ValueLike] => Env[ValueLike], Int)] =
        expr match {
          case Global(_, pack, name) if pack == NumericPackage => results.get(name)
          case _ => None
        }

      def makeLambda(
          arity: Int
      )(fn: List[ValueLike] => Env[ValueLike]): Env[ValueLike] =
        PredefExternal.makeLambda(arity)(fn)
    }

    class Ops[K](
        packName: PackageName,
        remap: (PackageName, Bindable) => Env[Option[ValueLike]],
        ns: CompilationNamespace[K]
    ) {
      private type InlineSlots = Option[Vector[Expression]]

      /*
       * enums with no fields are integers
       * enums and structs are tuples
       * enums first parameter is their index
       * nats are just integers
       */
      def makeCons(ce: ConsExpr, args: List[ValueLike]): Env[ValueLike] = {
        // invariant: args.size == arity
        def applyAll(args: List[ValueLike]): Env[ValueLike] =
          ce match {
            case MakeEnum(variant, _, famArities) =>
              // if all arities are 0, we use
              // integers to represent,
              // otherwise, we use tuples with the first
              // item being the variant
              val useInts = famArities.forall(_ == 0)
              val vExpr = Code.fromInt(variant)
              if (useInts) Env.pure(vExpr)
              else {
                // we make a tuple with the variant in the first position
                Env.onLasts(vExpr :: args)(Code.MakeTuple(_))
              }
            case MakeStruct(arity) =>
              if (arity == 0) Env.pure(Code.Const.Unit)
              else if (arity == 1) Env.pure(args.head)
              else Env.onLasts(args)(Code.MakeTuple(_))
            case ZeroNat =>
              Env.pure(Code.Const.Zero)
            case SuccNat =>
              Env.onLast(args.head)(_.evalPlus(Code.Const.One))
          }

        val sz = args.size
        def makeLam(cnt: Int, args: List[ValueLike]): Env[ValueLike] =
          if (cnt == 0) applyAll(args)
          else if (cnt < 0) {
            // too many args, this shouldn't typecheck
            // $COVERAGE-OFF$
            throw new IllegalStateException(s"invalid arity $sz for $ce")
            // $COVERAGE-ON$
          } else {
            // this is the case where we are using the constructor like a function
            assert(args.isEmpty)
            for {
              vs <- (1 to cnt).toList.traverse(_ => Env.newAssignableVar)
              body <- applyAll(vs)
              // TODO: if body isn't an expression, how can just adding a lambda
              // at the end be correct? the arguments will be below points that used it.
              // the onLast has to handle Code.Lambda specially
              res <- Env.onLast(body)(Code.Lambda(vs, _))
            } yield res
          }

        makeLam(ce.arity - sz, args)
      }

      def boolExpr(
          ix: BoolExpr[K],
          slotName: Option[Code.Ident],
          inlineSlots: InlineSlots
      ): Env[ValueLike] =
        ix match {
          case EqualsLit(expr, lit) =>
            val literal = Code.litToExpr(lit)
            loop(expr, slotName, inlineSlots)
              .flatMap(Env.onLast(_)(ex => ex =:= literal))
          case EqualsNat(nat, zeroOrSucc) =>
            val natF = loop(nat, slotName, inlineSlots)

            if (zeroOrSucc.isZero)
              natF.flatMap(Env.onLast(_)(_ =:= 0))
            else
              natF.flatMap(Env.onLast(_)(_ :> 0))

          case TrueConst     => Env.pure(Code.Const.True)
          case And(ix1, ix2) =>
            (
              boolExpr(ix1, slotName, inlineSlots),
              boolExpr(ix2, slotName, inlineSlots)
            )
              .flatMapN(Env.andCode(_, _))
          case CheckVariant(enumV, idx, _, famArities) =>
            // if all arities are 0, we use
            // integers to represent,
            // otherwise, we use tuples with the first
            // item being the variant
            val useInts = famArities.forall(_ == 0)
            loop(enumV, slotName, inlineSlots).flatMap { tup =>
              Env.onLast(tup) { t =>
                (if (useInts) {
                   // this is represented as an integer
                   t =:= idx
                 } else
                   t.get(0) =:= idx).simplify
              }
            }
          case SetMut(LocalAnonMut(mut), expr) =>
            (Env.nameForAnon(mut), loop(expr, slotName, inlineSlots)).flatMapN {
              (ident, result) =>
                Env.onLast(result) { resx =>
                  (ident := resx).withValue(Code.Const.True)
                }
            }
          case MatchString(str, pat, binds, mustMatch) =>
            (
              loop(str, slotName, inlineSlots),
              binds.traverse { case LocalAnonMut(m) => Env.nameForAnon(m) }
            ).flatMapN { (strVL, binds) =>
              Env.onLastM(strVL)(matchString(_, pat, binds, mustMatch))
            }
          case LetBool(n, v, in) =>
            doLet(n, v, boolExpr(in, slotName, inlineSlots), slotName, inlineSlots)
          case LetMutBool(_, in) =>
            // in python we just ignore this
            boolExpr(in, slotName, inlineSlots)
        }

      def matchString(
          strEx: Expression,
          pat: List[StrPart],
          binds: List[Code.Ident],
          mustMatch: Boolean
      ): Env[ValueLike] = {
        import StrPart.{LitStr, Glob, CharPart}
        val bindArray = binds.toArray
        // return a value like expression that contains the boolean result
        // and assigns all the bindings along the way
        def loop(
            knownPos: Option[Int],
            offsetIdent: Code.Ident,
            pat: List[StrPart],
            next: Int,
            mustMatch: Boolean
        ): Env[ValueLike] =
          pat match {
            case _ if mustMatch && next == bindArray.length =>
              // we have to match and we've captured everything
              Env.pure(Code.Const.True)
            case Nil =>
              // offset == str.length
              val off =
                knownPos.fold(offsetIdent: Expression)(i => (i: Expression))
              if (mustMatch) Env.pure(Code.Const.True)
              else Env.pure(off =:= strEx.len())
            case LitStr(expect) :: tail =>
              // val len = expect.length
              // str.regionMatches(offset, expect, 0, len) && loop(offset + len, tail, next)
              //
              // strEx.startswith(expect, offsetIdent)
              // note: a literal string can never be a total match, so mustMatch is false
              val expectSize = expect.codePointCount(0, expect.length)
              loop(
                knownPos.map(_ + expectSize),
                offsetIdent,
                tail,
                next,
                mustMatch = false
              )
                .flatMap { loopRes =>
                  val off =
                    knownPos.fold(offsetIdent: Expression)(i => (i: Expression))
                  val regionMatches =
                    strEx.dot(Code.Ident("startswith"))(expect, off)
                  val rest =
                    if (tail.nonEmpty && (tail != (StrPart.WildStr :: Nil))) {
                      // we aren't done matching
                      (offsetIdent := offsetIdent + expectSize)
                        .withValue(loopRes)
                    } else {
                      // we are done matching, no need to update offset
                      loopRes
                    }

                  Env.andCode(regionMatches, rest)
                }
            case (c: CharPart) :: Nil =>
              // last character
              val off = knownPos.fold(offsetIdent + 1)(i => (i + 1): Expression)
              val matches =
                if (mustMatch) Code.Const.True else (strEx.len() =:= off)
              if (c.capture) {
                val stmt = bindArray(next) := Code.SelectItem(strEx, -1)
                Env.andCode(matches, stmt.withValue(true))
              } else {
                Env.pure(matches)
              }
            case (c: CharPart) :: tail =>
              val off =
                knownPos.fold(offsetIdent: Expression)(i => (i: Expression))
              val matches =
                if (mustMatch) Code.Const.True
                else (off :< strEx.len())
              val n1 = if (c.capture) (next + 1) else next
              val stmt =
                if (c.capture) {
                  // b = str[offset]
                  Code
                    .block(
                      bindArray(next) := Code.SelectItem(strEx, off),
                      offsetIdent := offsetIdent + 1
                    )
                    .withValue(true)
                } else (offsetIdent := offsetIdent + 1).withValue(true)
              for {
                tailRes <- loop(
                  knownPos.map(_ + 1),
                  offsetIdent,
                  tail,
                  n1,
                  mustMatch
                )
                and2 <- Env.andCode(stmt, tailRes)
                and1 <- Env.andCode(matches, and2)
              } yield and1
            case (h: Glob) :: tail =>
              // after a glob, we no longer know the knownPos
              val off =
                knownPos.fold(offsetIdent: Expression)(i => (i: Expression))
              val knownPos1 = None
              tail match {
                case Nil =>
                  // we capture all the rest
                  Env.pure(
                    if (h.capture) {
                      // b = str[offset:]
                      (bindArray(next) := Code
                        .SelectRange(strEx, Some(off), None))
                        .withValue(true)
                    } else Code.Const.True
                  )
                case LitStr(expect) :: Nil =>
                  // if strEx.endswith(expect):
                  //   h = strEx[off:-(expect.len)]
                  val elen = expect.codePointCount(0, expect.length)
                  val matches =
                    if (mustMatch) Code.Const.True
                    else
                      strEx.dot(Code.Ident("endswith"))(Code.PyString(expect))

                  if (h.capture) {
                    Env.andCode(
                      matches,
                      (binds(next) := Code
                        .SelectRange(strEx, Some(off), Some(-elen: Expression)))
                        .withValue(true)
                    )
                  } else
                    Env.pure(matches)
                case LitStr(expect) :: (g2: Glob) :: Nil =>
                  // we could implement this kind of match: .*expect.*
                  // which is partition:
                  // (left, e, right) = strEx[offset:].partition(expect)
                  // if e:
                  //   h = left
                  //   e = right
                  //   True
                  // else:
                  //   False
                  val base = knownPos match {
                    case Some(0) => strEx
                    case _       => Code.SelectRange(strEx, Some(off), None)
                  }
                  if (h.capture || g2.capture) {
                    var npos = next
                    for {
                      pres <- Env.newAssignableVar
                      // TODO, maybe partition isn't actually ideal here
                      cmd = pres := base.dot(Code.Ident("partition"))(expect)
                      hbind =
                        if (h.capture) {
                          val b = npos
                          npos = npos + 1
                          binds(b) := pres.get(0)
                        } else Code.Pass
                      gbind =
                        if (g2.capture) {
                          val b = npos
                          npos = npos + 1
                          binds(b) := pres.get(2)
                        } else Code.Pass
                      cond = pres.get(1) != Code.PyString("")
                      m <- Env.andCode(
                        cmd.withValue(cond),
                        (hbind +: gbind).withValue(true)
                      )
                    } yield m
                  } else {
                    // this is just expect in strEx[off:]
                    Env.pure(knownPos match {
                      case Some(0) =>
                        Code.Op(Code.PyString(expect), Code.Const.In, strEx)
                      case _ =>
                        strEx.dot(Code.Ident("find"))(
                          expect,
                          off
                        ) :> (-1: Expression)
                    })
                  }
                case LitStr(expect) :: (tail2 @ (th :: _)) =>
                  // well formed patterns can really only
                  // have Nil, Glob :: _, Char :: _ after LitStr
                  // since we should have combined adjacent LitStr
                  //
                  // here we have to make a loop
                  // searching for expect, and then see if we
                  // can match the rest of the pattern
                  val next1 = if (h.capture) next + 1 else next

                  // there is a glob before and after expect,
                  // so, if the tail, .*x can't match s
                  // then it can't match any suffix of s.
                  val shouldSearch = th match {
                    case _: Glob => false
                    case _       => true
                  }
                  /*
                   * this is the scala code for the below
                   * it is in MatchlessToValue but left here
                   * as an aid to read the code below
                   *
                  var start = offset
                  var result = false
                  while (start >= 0) {
                    val candidate = str.indexOf(expect, start)
                    if (candidate >= 0) {
                      // we have to skip the current expect string
                      val candidateOffset = candidate + expect.lenth
                      val check1 = loop(candidateOffset, tail2, next1)
                      if (check1) {
                        // this was a match, write into next if needed
                        if (h.capture) {
                          results(next) = str.substring(offset, candidate)
                        }
                        result = true
                        start = -1
                      }
                      else {
                        // we couldn't match here, try just after candidate
                        start = candidate + 1
                      }
                    }
                    else {
                      // no more candidates
                      start = -1
                    }
                  }
                  result
                   */
                  (
                    Env.newAssignableVar,
                    Env.newAssignableVar,
                    Env.newAssignableVar,
                    Env.newAssignableVar
                  ).flatMapN { (start, result, candidate, candOffset) =>
                    // note, a literal prefix can never be a total match
                    val searchEnv = loop(
                      knownPos1,
                      candOffset,
                      tail2,
                      next1,
                      mustMatch = false
                    )

                    def onSearch(search: ValueLike): Env[Statement] =
                      Env.ifElseS(
                        search, {
                          // we have matched
                          val capture =
                            if (h.capture)
                              (bindArray(next) := Code.SelectRange(
                                strEx,
                                Some(off),
                                Some(candidate)
                              ))
                            else Code.Pass
                          Code.block(
                            capture,
                            result := true,
                            start := -1
                          )
                        },
                        // we couldn't match at start, advance just after the
                        // candidate
                        start := candidate + 1
                      )

                    def findBranch(search: ValueLike): Env[Statement] =
                      onSearch(search)
                        .flatMap { onS =>
                          Env.ifElseS(
                            candidate :> -1,
                            // update candidate and search
                            Code.block(
                              candOffset := candidate + expect.codePointCount(
                                0,
                                expect.length
                              ),
                              onS
                            ),
                            // else no more candidates
                            start := -1
                          )
                        }

                    for {
                      search <- searchEnv
                      find <- findBranch(search)
                    } yield
                      if (shouldSearch) {
                        Code
                          .block(
                            start := off,
                            result := false,
                            Code.While(
                              (start :> -1),
                              Code.block(
                                candidate := strEx
                                  .dot(Code.Ident("find"))(expect, start),
                                find
                              )
                            )
                          )
                          .withValue(result)
                      } else {
                        Code
                          .block(
                            start := off,
                            result := false,
                            candidate := strEx
                              .dot(Code.Ident("find"))(expect, start),
                            find
                          )
                          .withValue(result)
                      }
                  }
                case (c: CharPart) :: Nil =>
                  // last character
                  val matches =
                    if (mustMatch) Code.Const.True else (strEx.len() :> off)
                  val cpart = if (c.capture) {
                    val cnext = if (h.capture) (next + 1) else next
                    val stmt = bindArray(cnext) := Code.SelectItem(strEx, -1)
                    Env.andCode(matches, stmt.withValue(true))
                  } else {
                    Env.pure(matches)
                  }
                  val hpart =
                    if (h.capture) {
                      bindArray(next) := Code.SelectRange(
                        strEx,
                        Some(off),
                        Some(-1: Expression)
                      )
                    } else Code.Pass

                  cpart.map(hpart.withValue(_))
                case (_: CharPart) :: _ =>
                  val next1 = if (h.capture) (next + 1) else next
                  for {
                    matched <- Env.newAssignableVar
                    off1 <- Env.newAssignableVar
                    // the tail match isn't true, because we loop until we find
                    // a case
                    tailMatched <- loop(knownPos1, off1, tail, next1, false)

                    matchStmt = Code
                      .block(
                        matched := false,
                        off1 := off,
                        Code.While(
                          (!matched).evalAnd(off1 :< strEx.len()),
                          matched := tailMatched // the tail match increments the
                        )
                      )
                      .withValue(if (mustMatch) Code.Const.True else matched)

                    fullMatch <-
                      if (!h.capture) Env.pure(matchStmt)
                      else {
                        val capture = Code
                          .block(
                            bindArray(next) := Code
                              .SelectRange(strEx, Some(off), Some(off1))
                          )
                          .withValue(true)
                        Env.andCode(matchStmt, capture)
                      }

                  } yield fullMatch
                // $COVERAGE-OFF$
                case (_: Glob) :: _ =>
                  throw new IllegalArgumentException(
                    s"pattern: $pat should have been prevented: adjacent globs are not permitted (one is always empty)"
                  )
                // $COVERAGE-ON$
              }
          }

        for {
          offsetIdent <- Env.newAssignableVar
          res <- loop(Some(0), offsetIdent, pat, 0, mustMatch)
        } yield (offsetIdent := 0).withValue(res)
      }

      // InlineSlots holds resolved Python expressions, not the original
      // Matchless Exprs. Locals/LocalAnon are immutable and already renamed
      // to unique Python idents (shadowing-safe), and Globals are stable.
      // Inlining these is safe because it captures the correct binding.
      private def inlineableCapture(expr: Expr[K]): Boolean =
        expr match {
          case Local(_) | Global(_, _, _) | LocalAnon(_) => true
          case _                                        => false
        }

      private def inlineCaptures(
          captures: List[Expr[K]],
          slotName: Option[Code.Ident],
          inlineSlots: InlineSlots
      ): Env[InlineSlots] = {
        val allowInline =
          captures.nonEmpty &&
            slotName.isEmpty &&
            inlineSlots.isEmpty &&
            captures.forall(inlineableCapture)

        if (!allowInline) Env.pure(None)
        else {
          captures.traverse(loop(_, slotName, inlineSlots)).map { vs =>
            vs.foldRight(Option(Vector.empty[Expression])) {
              case (e: Expression, acc) => acc.map(e +: _)
              case _                    => None
            }
          }
        }
      }

      // if expr is a Lambda handle it
      def topFn(
          name: Code.Ident,
          expr: Lambda[K],
          slotName: Option[Code.Ident],
          inlineSlots: InlineSlots
      ): Env[Statement] =
        expr match {
          case Lambda(captures, recName, args, body) =>
            // If inlineCaptures succeeds, we don't allocate a slots tuple at all.
            // This is safe to do even if we were passed inlineSlots, because
            // inlineCaptures only returns Some when slotName/inlineSlots are empty.
            // Otherwise, we must build the slots tuple via makeSlots.
            inlineCaptures(captures, slotName, inlineSlots).flatMap {
              case Some(inlined) =>
                recName.traverse_(Env.bindTo(_, name)) *> args
                  .traverse(Env.bind(_))
                  .flatMap { as =>
                    loop(body, None, Some(inlined))
                      .map(Env.makeDef(name, as, _))
                  } <* args.traverse_(Env.unbind(_)) <* recName.traverse_(
                  Env.unbind(_)
                )
              case None =>
                recName.traverse_(Env.bindTo(_, name)) *> (
                  args.traverse(Env.bind(_)),
                  // Reset inlineSlots for the new lambda: since we are creating
                  // a slots tuple here, we could not inline all of its captures.
                  makeSlots(captures, slotName, inlineSlots)(b =>
                    loop(body, b, None)
                  )
                )
                  .mapN { case (as, (slots, body)) =>
                    Code.blockFromList(
                      slots.toList :::
                        Env.makeDef(name, as, body) ::
                        Nil
                    )
                  } <* args.traverse_(Env.unbind(_)) <* recName.traverse_(
                  Env.unbind(_)
                )
            }
        }

      def makeSlots[A](
          captures: List[Expr[K]],
          slotName: Option[Code.Ident],
          inlineSlots: InlineSlots
      )(
          fn: Option[Code.Ident] => Env[A]
      ): Env[(Option[Statement], A)] =
        if (captures.isEmpty) fn(None).map((None, _))
        else {
          for {
            slots <- Env.newAssignableVar
            capVals <- captures.traverse(loop(_, slotName, inlineSlots))
            resVal <- fn(Some(slots))
            tup <- Env.onLasts(capVals)(Code.MakeTuple(_))
          } yield (Some(slots := tup), resVal)
        }

      def doLet(
          name: Either[LocalAnon, Bindable],
          value: Expr[K],
          inF: Env[ValueLike],
          slotName: Option[Code.Ident],
          inlineSlots: InlineSlots
      ): Env[ValueLike] =
        name match {
          case Right(b) =>
            // value b is in scope after ve
            for {
              ve <- loop(value, slotName, inlineSlots)
              bi <- Env.bind(b)
              ine <- inF
              _ <- Env.unbind(b)
            } yield ((bi := ve).withValue(ine))
          case Left(LocalAnon(l)) =>
            // anonymous names never shadow
            (Env.nameForAnon(l), loop(value, slotName, inlineSlots))
              .flatMapN { (bi, vE) =>
                inF.map((bi := vE).withValue(_))
              }
        }
      def loop(
          expr: Expr[K],
          slotName: Option[Code.Ident],
          inlineSlots: InlineSlots
      ): Env[ValueLike] =
        expr match {
          case Lambda(captures, recName, args, res) =>
            val defName = recName match {
              case None    => Env.newAssignableVar
              case Some(n) => Env.bind(n)
            }
            def buildLambdaValue(
                args: NonEmptyList[Code.Ident],
                defName: Code.Ident,
                prefix: Option[Statement],
                body: ValueLike
            ): Env[ValueLike] =
              (prefix, body, recName) match {
                case (None, x: Expression, None) =>
                  Env.pure(Code.Lambda(args.toList, x))
                case _ =>
                  for {
                    _ <- recName.fold(Monad[Env].unit)(Env.unbind(_))
                    defn = Env.makeDef(defName, args, body)
                    block = Code.blockFromList(prefix.toList ::: defn :: Nil)
                  } yield block.withValue(defName)
              }

            // If inlineCaptures succeeds, we don't allocate a slots tuple at all.
            // This is safe to do even if we were passed inlineSlots, because
            // inlineCaptures only returns Some when slotName/inlineSlots are empty.
            // Otherwise, we must build the slots tuple via makeSlots.
            inlineCaptures(captures, slotName, inlineSlots).flatMap {
              case Some(inlined) =>
                (
                  args.traverse(Env.bind(_)),
                  defName,
                  loop(res, None, Some(inlined))
                )
                  .flatMapN { case (args, defName, body) =>
                    buildLambdaValue(args, defName, None, body)
                  } <* args.traverse_(Env.unbind(_))
              case None =>
                (
                  args.traverse(Env.bind(_)),
                  defName,
                  // Reset inlineSlots for the new lambda: since we are creating
                  // a slots tuple here, we could not inline all of its captures.
                  makeSlots(captures, slotName, inlineSlots)(b =>
                    loop(res, b, None)
                  )
                )
                  .flatMapN { case (args, defName, (prefix, body)) =>
                    buildLambdaValue(args, defName, prefix, body)
                  } <* args.traverse_(Env.unbind(_))
            }

          case WhileExpr(cond, effect, res) =>
            (
              boolExpr(cond, slotName, inlineSlots),
              loop(effect, slotName, inlineSlots),
              loop(res, slotName, inlineSlots),
              Env.newAssignableVar
            )
              .mapN { (cond, effect, res, c) =>
                Code
                  .block(
                    c := cond,
                    Code.While(
                      c,
                      Code.block(
                        Code.always(effect),
                        c := cond
                      )
                    )
                  )
                  .withValue(res)
              }
          case PredefExternal((fn, arity)) =>
            // make a lambda
            PredefExternal.makeLambda(arity)(fn)
          case NumericExternal((fn, arity)) =>
            // make a lambda for numeric intrinsic
            NumericExternal.makeLambda(arity)(fn)
          case Global(k, p, n) =>
            remap(p, n)
              .flatMap {
                case Some(v) => Env.pure(v)
                case None    =>
                  if (p == packName) {
                    // This is just a name in the local package
                    Env.topLevelName(n)
                  } else {
                    val ident = ns.globalIdent(k, p)
                    (Env.importPackage(ident), Env.topLevelName(n))
                      .mapN(Code.DotSelect(_, _))
                  }
              }
          case Local(b)         => Env.deref(b)
          case LocalAnon(a)     => Env.nameForAnon(a)
          case LocalAnonMut(m)  => Env.nameForAnon(m)
          case ClosureSlot(idx) =>
            inlineSlots match {
              case Some(inlined) =>
                inlined.lift(idx) match {
                  case Some(e) => Env.pure(e)
                  case None    =>
                    // $COVERAGE-OFF$
                    throw new IllegalStateException(
                      s"saw $expr when inlined slots size ${inlined.size}"
                    )
                  // $COVERAGE-ON$
                }
              case None =>
                slotName match {
                  case Some(ident) => Env.pure(ident.get(idx))
                  case None        =>
                    // $COVERAGE-OFF$
                    // this should be impossible for well formed Matchless AST
                    throw new IllegalStateException(
                      s"saw $expr when there is no defined slot"
                    )
                  // $COVERAGE-ON$
                }
            }
          case App(PredefExternal((fn, _)), args) =>
            args.toList
              .traverse(loop(_, slotName, inlineSlots))
              .flatMap(fn)
          case App(NumericExternal((fn, _)), args) =>
            args.toList
              .traverse(loop(_, slotName))
              .flatMap(fn)
          case App(cons: ConsExpr, args) =>
            args.traverse(loop(_, slotName, inlineSlots)).flatMap { pxs =>
              makeCons(cons, pxs.toList)
            }
          case App(expr, args) =>
            (
              loop(expr, slotName, inlineSlots),
              args.traverse(loop(_, slotName, inlineSlots))
            ).mapN {
              (fn, args) =>
                Env.onLasts(fn :: args.toList) {
                  case fn :: args => Code.Apply(fn, args)
                  case other      =>
                    // $COVERAGE-OFF$
                    throw new IllegalStateException(
                      s"got $other, expected to match $expr"
                    )
                  // $COVERAGE-ON$
                }
            }.flatten
          case Let(localOrBind, fn: Lambda[k], in) =>
            localOrBind match {
              case Right(b) if fn.captures.isEmpty =>
                for {
                  hoistedName <- Env.newHoistedDefName
                  defStmt <- topFn(hoistedName, fn, None, None)
                  _ <- Env.lift(defStmt)
                  _ <- Env.bindTo(b, hoistedName)
                  ine <- loop(in, slotName, inlineSlots)
                  _ <- Env.unbind(b)
                } yield ine
              case Right(b) =>
                val inF = loop(in, slotName, inlineSlots)
                // for fn, bosatsu doesn't allow bind name
                // shadowing, so the bind order of the name
                // doesn't matter
                for {
                  bi <- Env.bind(b)
                  tl <- topFn(bi, fn, slotName, inlineSlots)
                  ine <- inF
                  _ <- Env.unbind(b)
                } yield tl.withValue(ine)
              case Left(LocalAnon(l)) =>
                val inF = loop(in, slotName, inlineSlots)
                // anonymous names never shadow
                Env
                  .nameForAnon(l)
                  .flatMap { bi =>
                    val v = topFn(bi, fn, slotName, inlineSlots)
                    (v, inF).mapN(_.withValue(_))
                  }
            }
          case Let(localOrBind, notFn, in) =>
            // we know that notFn is not Lambda here
            doLet(
              localOrBind,
              notFn,
              loop(in, slotName, inlineSlots),
              slotName,
              inlineSlots
            )
          case LetMut(LocalAnonMut(_), in) =>
            // we could delete this name, but
            // there is no need to
            loop(in, slotName, inlineSlots)
          case Literal(lit)         => Env.pure(Code.litToExpr(lit))
          case ifExpr @ If(_, _, _) =>
            val (ifs, last) = ifExpr.flatten

            val ifsV = ifs.traverse { case (c, t) =>
              (boolExpr(c, slotName, inlineSlots), loop(t, slotName, inlineSlots)).tupled
            }

            (ifsV, loop(last, slotName, inlineSlots)).mapN { (ifs, elseV) =>
              Env.ifElse(ifs, elseV)
            }.flatten

          case Always.SetChain(setmuts, result) =>
            (
              setmuts.traverse { case (LocalAnonMut(mut), v) =>
                Env.nameForAnon(mut).product(loop(v, slotName, inlineSlots))
              },
              loop(result, slotName, inlineSlots)
            ).mapN { (assigns, result) =>
              Code
                .blockFromList(
                  assigns.toList.map { case (mut, v) =>
                    mut := v
                  }
                )
                .withValue(result)
            }
          case Always(cond, expr) =>
            (
              boolExpr(cond, slotName, inlineSlots).map(Code.always),
              loop(expr, slotName, inlineSlots)
            )
              .mapN(_.withValue(_))

          case GetEnumElement(expr, _, idx, _) =>
            // nonempty enums are just structs with the first element being the variant
            // we could assert the v matches when debugging, but typechecking
            // should assure this
            loop(expr, slotName, inlineSlots).flatMap { tup =>
              Env.onLast(tup)(_.get(idx + 1))
            }
          case GetStructElement(expr, idx, sz) =>
            val exprR = loop(expr, slotName, inlineSlots)
            if (sz == 1) {
              // we don't bother to wrap single item structs
              exprR
            } else {
              // structs are just tuples
              exprR.flatMap { tup =>
                Env.onLast(tup)(_.get(idx))
              }
            }
          case PrevNat(expr) =>
            // Nats are just integers
            loop(expr, slotName, inlineSlots).flatMap { nat =>
              Env.onLast(nat)(_.evalMinus(Code.Const.One))
            }
          case cons: ConsExpr => makeCons(cons, Nil)
        }
    }
  }
}
