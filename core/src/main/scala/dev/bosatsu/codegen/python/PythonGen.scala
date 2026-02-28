package dev.bosatsu.codegen.python

import cats.Monad
import cats.data.{NonEmptyList, State}
import cats.parse.{Parser => P}
import dev.bosatsu.{PackageName, Identifier, Matchless, Par, Parser}
import dev.bosatsu.codegen.{CompilationNamespace, CompilationSource, Idents}
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
      val pinned = state.tops.iterator.map(escape).toSet
      val allStmts = Code.optimizeStatements(lifted ::: stmts, pinned)

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
                p.map(escapeModule) :+ Code.Ident("__init__.py")

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
    Impl.PredefExternal.resultsByPackage.view.mapValues(_.keySet).toMap

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
      private val predefPackage: PackageName = PackageName.PredefName
      private val arrayPackage: PackageName =
        PackageName.parts("Bosatsu", "Collection", "Array")
      private val float64Package: PackageName =
        PackageName.parts("Bosatsu", "Num", "Float64")

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

      private def mathModule: Env[Code.Ident] =
        Env.importLiteral(NonEmptyList.one(Code.Ident("math")))

      private def structModule: Env[Code.Ident] =
        Env.importLiteral(NonEmptyList.one(Code.Ident("struct")))

      private def unaryMath(name: String): List[ValueLike] => Env[ValueLike] = {
        input =>
          mathModule.flatMap { math =>
            Env.onLast(input.head) { arg =>
              math.dot(Code.Ident(name))(arg)
            }
          }
      }

      private def binaryMath(
          name: String
      ): List[ValueLike] => Env[ValueLike] = { input =>
        mathModule.flatMap { math =>
          Env.onLast2(input.head, input.tail.head) { (left, right) =>
            math.dot(Code.Ident(name))(left, right)
          }
        }
      }

      private val unsigned64TopBitExpr = Code.PyInt(
        java.math.BigInteger.ONE.shiftLeft(63)
      )
      private val unsigned64ModExpr =
        Code.PyInt(java.math.BigInteger.ONE.shiftLeft(64))
      private val maxFiniteFloatIntExpr = Code.PyInt(
        new java.math.BigInteger(
          "179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368"
        )
      )

      private def signedToUnsignedBits64(
          bits: Code.Expression
      ): Code.Expression =
        Code
          .Ternary(
            bits.evalPlus(unsigned64ModExpr),
            bits :< Code.Const.Zero,
            bits
          )
          .simplify

      private def floatToUnsignedBits64(
          struct: Code.Expression,
          arg: Code.Expression
      ): Code.Expression = {
        val signedBits = struct
          .dot(Code.Ident("unpack"))(
            Code.PyString(">q"),
            struct.dot(Code.Ident("pack"))(Code.PyString(">d"), arg)
          )
          .get(0)
        signedToUnsignedBits64(
          signedBits
        )
      }

      private def unsignedToSignedBits64(
          bits: Code.Expression
      ): Code.Expression = {
        val bits1 = bits.eval(Code.Const.Mod, unsigned64ModExpr)
        Code
          .Ternary(
            bits1.evalMinus(unsigned64ModExpr),
            !(bits1 :< unsigned64TopBitExpr),
            bits1
          )
          .simplify
      }

      private def unsignedBits64ToFloat(
          struct: Code.Expression,
          bits: Code.Expression
      ): Code.Expression = {
        val signedBits = unsignedToSignedBits64(bits)
        struct
          .dot(Code.Ident("unpack"))(
            Code.PyString(">d"),
            struct
              .dot(Code.Ident("pack"))(
                Code.PyString(">q"),
                signedBits
              )
          )
          .get(0)
      }

      private val cmpFloatFn: List[ValueLike] => Env[ValueLike] = { input =>
        mathModule.flatMap { math =>
          Env.onLast2(input.head, input.tail.head) { (arg0, arg1) =>
            val aNaN = math.dot(Code.Ident("isnan"))(arg0)
            val bNaN = math.dot(Code.Ident("isnan"))(arg1)
            val neitherNaN = Code
              .Ternary(
                0,
                arg0 :< arg1,
                Code.Ternary(1, arg0 =:= arg1, 2)
              )
              .simplify
            Code
              .Ternary(
                Code.Ternary(1, bNaN, 0),
                aNaN,
                Code.Ternary(2, bNaN, neitherNaN)
              )
              .simplify
          }
        }
      }

      private val divFloatFn: List[ValueLike] => Env[ValueLike] = { input =>
        mathModule.flatMap { math =>
          Env.onLast2(input.head, input.tail.head) { (arg0, arg1) =>
            val arg1Zero = arg1 =:= Code.PyFloat(0.0)
            val arg0Zero = arg0 =:= Code.PyFloat(0.0)
            val arg0NaN = math.dot(Code.Ident("isnan"))(arg0)
            val nanVal = Code.Ident("float")(Code.PyString("nan"))
            val infBySign = {
              val signMul = Code.Op(
                math.dot(Code.Ident("copysign"))(Code.PyFloat(1.0), arg0),
                Code.Const.Times,
                math.dot(Code.Ident("copysign"))(Code.PyFloat(1.0), arg1)
              )
              Code
                .Ternary(
                  Code.Ident("float")(Code.PyString("-inf")),
                  signMul :< Code.PyFloat(0.0),
                  Code.Ident("float")(Code.PyString("inf"))
                )
                .simplify
            }

            Code
              .Ternary(
                Code.Ternary(
                  nanVal,
                  Code.Op(arg0NaN, Code.Const.Or, arg0Zero),
                  infBySign
                ),
                arg1Zero,
                Code.Op(arg0, Code.Const.Div, arg1)
              )
              .simplify
          }
        }
      }

      private val floatToStringFn: List[ValueLike] => Env[ValueLike] = {
        input =>
          structModule.flatMap { struct =>
            mathModule.flatMap { math =>
              Env.onLast(input.head) { arg =>
                val posInf = Code.PyString("\u221E")
                val negInf = Code.PyString("-\u221E")
                val isInf = math.dot(Code.Ident("isinf"))(arg)
                val isNaN = math.dot(Code.Ident("isnan"))(arg)
                val sign =
                  math.dot(Code.Ident("copysign"))(Code.PyFloat(1.0), arg)
                val infString =
                  Code.Ternary(negInf, sign :< Code.PyFloat(0.0), posInf)
                val bits = floatToUnsignedBits64(struct, arg)
                val nanString =
                  Code.Op(Code.PyString("NaN:0x%016x"), Code.Const.Mod, bits)
                Code
                  .Ternary(
                    infString,
                    isInf,
                    Code.Ternary(nanString, isNaN, Code.Ident("repr")(arg))
                  )
                  .simplify
              }
            }
          }
      }

      private val floatBitsToIntFn: List[ValueLike] => Env[ValueLike] = {
        input =>
          structModule.flatMap { struct =>
            Env.onLast(input.head)(arg => floatToUnsignedBits64(struct, arg))
          }
      }

      private val intBitsToFloatFn: List[ValueLike] => Env[ValueLike] = {
        input =>
          structModule.flatMap { struct =>
            Env.onLast(input.head) { arg =>
              unsignedBits64ToFloat(struct, arg)
            }
          }
      }

      private val floatToIntFn: List[ValueLike] => Env[ValueLike] = { input =>
        mathModule.flatMap { math =>
          Env.onLast(input.head) { arg =>
            val noneValue = Code.MakeTuple(Code.Const.Zero :: Nil)
            def someValue(v: Code.Expression): Code.Expression =
              Code.MakeTuple(Code.Const.One :: v :: Nil)
            val nonFinite = Code
              .Op(
                math.dot(Code.Ident("isinf"))(arg),
                Code.Const.Or,
                math.dot(Code.Ident("isnan"))(arg)
              )
              .simplify
            val floorV = math.dot(Code.Ident("floor"))(arg)
            val frac = Code.Op(arg, Code.Const.Minus, floorV).simplify
            val floorInt = Code.Ident("int")(floorV)
            val floorPlusOne =
              Code.Op(floorInt, Code.Const.Plus, Code.Const.One).simplify
            val floorIsEven = Code
              .Op(
                Code.Op(floorInt, Code.Const.Mod, Code.fromInt(2)).simplify,
                Code.Const.Eq,
                Code.Const.Zero
              )
              .simplify
            val tieRounded =
              Code.Ternary(floorInt, floorIsEven, floorPlusOne).simplify
            val roundedInt = Code
              .Ternary(
                floorInt,
                frac :< Code.PyFloat(0.5),
                Code
                  .Ternary(
                    floorPlusOne,
                    frac :> Code.PyFloat(0.5),
                    tieRounded
                  )
                  .simplify
              )
              .simplify
            Code.Ternary(noneValue, nonFinite, someValue(roundedInt)).simplify
          }
        }
      }

      private val intToFloatFn: List[ValueLike] => Env[ValueLike] = { input =>
        Env.onLast(input.head) { arg =>
          val absArg = Code.Ident("abs")(arg)
          val overflow = absArg :> maxFiniteFloatIntExpr
          val infBySign = Code
            .Ternary(
              Code.Ident("float")(Code.PyString("-inf")),
              arg :< Code.Const.Zero,
              Code.Ident("float")(Code.PyString("inf"))
            )
            .simplify
          Code.Ternary(infBySign, overflow, Code.Ident("float")(arg)).simplify
        }
      }

      private val stringToFloatFn: List[ValueLike] => Env[ValueLike] = {
        input =>
          structModule.flatMap { struct =>
            Env.importLiteral(NonEmptyList.one(Code.Ident("re"))).flatMap { re =>
              (Env.newAssignableVar, Env.newAssignableVar).tupled.flatMap {
                case (cleanedVar, cleanedLowerVar) =>
                  Env.onLast(input.head) { s =>
                    val noneValue = Code.MakeTuple(Code.Const.Zero :: Nil)
                    def someValue(v: Code.Expression): Code.Expression =
                      Code.MakeTuple(Code.Const.One :: v :: Nil)
                    val cleaned: Code.Expression = cleanedVar
                    val cleanedLower: Code.Expression = cleanedLowerVar

                    val posInf = Code.Op(
                      cleanedLower,
                      Code.Const.In,
                      Code.MakeTuple(
                        Code.PyString("\u221E") ::
                          Code.PyString("+\u221E") ::
                          Code.PyString("infinity") ::
                          Code.PyString("+infinity") ::
                          Code.PyString("inf") ::
                          Code.PyString("+inf") :: Nil
                      )
                    )
                    val negInf = Code.Op(
                      cleanedLower,
                      Code.Const.In,
                      Code.MakeTuple(
                        Code.PyString("-\u221E") ::
                          Code.PyString("-infinity") ::
                          Code.PyString("-inf") :: Nil
                      )
                    )
                    val isNaN = Code.Op(
                      cleanedLower,
                      Code.Const.In,
                      Code.MakeTuple(
                        Code.PyString(".nan") :: Code.PyString("nan") :: Nil
                      )
                    )
                    val hasNanPrefix =
                      cleanedLower.dot(Code.Ident("startswith"))(
                        Code.PyString("nan:0x")
                      )
                    val hexPart =
                      Code.SelectRange(cleaned, Some(Code.fromInt(6)), None)
                    val fullHex = re
                      .dot(Code.Ident("match"))(
                        Code.PyString("^[0-9a-fA-F]{16}$"),
                        hexPart
                      )
                      =!= Code.Ident("None")
                    val nanByBits = hasNanPrefix.evalAnd(fullHex)
                    val nanBits = Code.Ident("int")(hexPart, Code.fromInt(16))
                    val nanByBitsValue = unsignedBits64ToFloat(struct, nanBits)
                    val isNumeric = re
                      .dot(Code.Ident("match"))(
                        Code.PyString(
                          "^[+-]?(?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:[eE][+-]?\\d+)?$"
                        ),
                        cleaned
                      )
                      =!= Code.Ident("None")
                    val parsedNumeric = Code.Ident("float")(cleaned)

                    Code
                      .block(
                        cleanedVar := s.dot(Code.Ident("replace"))(
                          Code.PyString("_"),
                          Code.PyString("")
                        ),
                        cleanedLowerVar := cleanedVar.dot(Code.Ident("lower"))()
                      )
                      .withValue(
                        Code.IfElse(
                          NonEmptyList.of(
                            (
                              posInf,
                              someValue(
                                Code.Ident("float")(Code.PyString("inf"))
                              )
                            ),
                            (
                              negInf,
                              someValue(
                                Code.Ident("float")(Code.PyString("-inf"))
                              )
                            ),
                            (
                              isNaN,
                              someValue(
                                Code.Ident("float")(Code.PyString("nan"))
                              )
                            ),
                            (nanByBits, someValue(nanByBitsValue)),
                            (isNumeric, someValue(parsedNumeric))
                          ),
                          noneValue
                        )
                      )
                  }
              }
            }
          }
      }

      private def arrayData(ary: Expression): Expression =
        ary.get(0)
      private def arrayOffset(ary: Expression): Expression =
        ary.get(1)
      private def arrayLen(ary: Expression): Expression =
        ary.get(2)
      private def selectItem(
          target: Expression,
          index: Expression
      ): Expression =
        Code.SelectItem(target, index)
      private def makeArray(
          data: Expression,
          offset: Expression,
          len: Expression
      ): Expression =
        Code.MakeTuple(
          Code.Ident("tuple")(data) :: offset :: len :: Nil
        )
      private val emptyArray: Expression =
        makeArray(Code.MakeList(Nil), Code.Const.Zero, Code.Const.Zero)

      val results: Map[Bindable, (List[ValueLike] => Env[ValueLike], Int)] =
        Map(
          (
            Identifier.Name("add"),
            (
              input => Env.onLast2(input.head, input.tail.head)(_.evalPlus(_)),
              2
            )
          ),
          (
            Identifier.Name("sub"),
            (
              { input =>
                Env.onLast2(input.head, input.tail.head)(_.evalMinus(_))
              },
              2
            )
          ),
          (
            Identifier.Name("mul"),
            (
              { input =>
                Env.onLast2(input.head, input.tail.head)(_.evalTimes(_))
              },
              2
            )
          ),
          (
            Identifier.Name("div"),
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
            Identifier.Name("addf"),
            (
              input => Env.onLast2(input.head, input.tail.head)(_.evalPlus(_)),
              2
            )
          ),
          (
            Identifier.Name("subf"),
            (
              { input =>
                Env.onLast2(input.head, input.tail.head)(_.evalMinus(_))
              },
              2
            )
          ),
          (
            Identifier.Name("timesf"),
            (
              { input =>
                Env.onLast2(input.head, input.tail.head)(_.evalTimes(_))
              },
              2
            )
          ),
          (
            Identifier.Name("divf"),
            (
              divFloatFn,
              2
            )
          ),
          (
            Identifier.Name("mod_Int"),
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
          (Identifier.Name("cmp_Int"), (cmpFn, 2)),
          (
            Identifier.Name("eq_Int"),
            (
              { input =>
                Env.onLast2(input.head, input.tail.head)(
                  _.eval(Code.Const.Eq, _)
                )
              },
              2
            )
          ),
          (Identifier.Name("cmp_Float64"), (cmpFloatFn, 2)),
          (
            Identifier.Name("shift_left_Int"),
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
            Identifier.Name("shift_right_Int"),
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
            Identifier.Name("and_Int"),
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
            Identifier.Name("or_Int"),
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
            Identifier.Name("xor_Int"),
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
            Identifier.Name("not_Int"),
            (
              {
                // leverage not(x) == -1 - x
                input => Env.onLast(input.head)(Code.fromInt(-1).evalMinus(_))
              },
              2
            )
          ),
          (
            Identifier.Name("gcd_Int"),
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
            Identifier.Name("int_loop"),
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
            Identifier.Name("concat_String"),
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
            Identifier.Name("length_String"),
            (
              { input =>
                Env.onLast(input.head)(_.len())
              },
              1
            )
          ),
          (
            Identifier.Name("int_to_String"),
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
            Identifier.Name("string_to_Int"),
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
            Identifier.Name("char_to_String"),
            // we encode chars as strings so this is just identity
            ({ input => Env.envMonad.pure(input.head) }, 1)
          ),
          (
            Identifier.Name("char_to_Int"),
            (
              { input =>
                Env.onLast(input.head)(c => Code.Ident("ord")(c))
              },
              1
            )
          ),
          (
            Identifier.Name("char_List_to_String"),
            (
              { input =>
                Env.onLastM(input.head) { listOfChars =>
                  // convert to python list, then call "".join(seq)
                  Env.newAssignableVar
                    .flatMap { pyList =>
                      bosatsuListToPython(pyList, listOfChars)
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
            Identifier.Name("int_to_Char"),
            (
              { input =>
                Env.onLast(input.head) { cp =>
                  val nonNegative = !(cp :< Code.Const.Zero)
                  val belowUnicodeLimit = cp :< Code.fromInt(0x110000)
                  val inSurrogateRange =
                    (!(cp :< Code.fromInt(0xd800)))
                      .evalAnd(cp :< Code.fromInt(0xe000))
                  val valid =
                    nonNegative
                      .evalAnd(belowUnicodeLimit)
                      .evalAnd(
                        !inSurrogateRange
                      )
                  Code.Ternary(
                    Code.MakeTuple(
                      Code.Const.One :: Code.Ident("chr")(cp) :: Nil
                    ),
                    valid,
                    Code.MakeTuple(Code.Const.Zero :: Nil)
                  )
                }
              },
              1
            )
          ),
          (
            Identifier.Name("trace"),
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
            Identifier.Name("partition_String"),
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
            Identifier.Name("rpartition_String"),
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
          (
            Identifier.Name("uncons_String"),
            (
              { input =>
                Env.onLast(input.head) { str =>
                  val success = Code.MakeTuple(
                    Code.fromInt(1) ::
                      Code.MakeTuple(
                        str.get(0) ::
                          Code.SelectRange(str, Some(Code.Const.One), None) ::
                          Nil
                      ) ::
                      Nil
                  )
                  val fail = Code.MakeTuple(Code.fromInt(0) :: Nil)
                  Code.Ternary(success, str, fail)
                }
              },
              1
            )
          ),
          (
            Identifier.Name("tail_or_empty_String"),
            (
              { input =>
                Env.onLast(input.head) { str =>
                  Code.SelectRange(str, Some(Code.Const.One), None)
                }
              },
              1
            )
          ),
          (Identifier.Name("cmp_String"), (cmpFn, 2))
        )

      val arrayResults
          : Map[Bindable, (List[ValueLike] => Env[ValueLike], Int)] =
        Map(
          (
            Identifier.Name("empty_Array"),
            ((_: List[ValueLike]) => Env.pure(emptyArray), 0)
          ),
          (
            Identifier.Name("tabulate_Array"),
            (
              { input =>
                (Env.newAssignableVar, Env.newAssignableVar).tupled.flatMap {
                  case (data, idx) =>
                    Env.onLasts(input) {
                      case n :: fn :: Nil =>
                        val tabulated = Code
                          .block(
                            data := Code.MakeList(Nil).evalTimes(n),
                            idx := Code.Const.Zero,
                            Code.While(
                              idx :< n,
                              Code.block(
                                selectItem(data, idx) := fn(idx),
                                idx := idx + 1
                              )
                            )
                          )
                          .withValue(
                            makeArray(data, Code.Const.Zero, n)
                          )
                        Code.IfElse(
                          NonEmptyList.one((n :> Code.Const.Zero, tabulated)),
                          emptyArray
                        )
                      case other =>
                        // $COVERAGE-OFF$
                        throw new IllegalStateException(
                          s"expected arity 2 got: $other"
                        )
                      // $COVERAGE-ON$
                    }
                }
              },
              2
            )
          ),
          (
            Identifier.Name("from_List_Array"),
            (
              { input =>
                Env.newAssignableVar.flatMap { pyList =>
                  Env.onLastM(input.head) { bList =>
                    bosatsuListToPython(pyList, bList).map { loop =>
                      Code
                        .block(
                          pyList := Code.MakeList(Nil),
                          loop
                        )
                        .withValue(
                          makeArray(pyList, Code.Const.Zero, pyList.len())
                        )
                    }
                  }
                }
              },
              1
            )
          ),
          (
            Identifier.Name("to_List_Array"),
            (
              { input =>
                (
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar
                ).tupled.flatMap { case (data, offset, size, idx, res) =>
                  Env.onLast(input.head) { ary =>
                    Code
                      .block(
                        data := arrayData(ary),
                        offset := arrayOffset(ary),
                        size := arrayLen(ary),
                        idx := (offset + size).evalMinus(Code.Const.One),
                        res := emptyList,
                        Code.While(
                          !(idx :< offset),
                          Code.block(
                            res := consList(selectItem(data, idx), res),
                            idx := idx.evalMinus(Code.Const.One)
                          )
                        )
                      )
                      .withValue(res)
                  }
                }
              },
              1
            )
          ),
          (
            Identifier.Name("size_Array"),
            (
              { input =>
                Env.onLast(input.head)(arrayLen)
              },
              1
            )
          ),
          (
            Identifier.Name("char_Array_to_String"),
            (
              { input =>
                Env.onLast(input.head) { ary =>
                  val start = arrayOffset(ary)
                  val end = start.evalPlus(arrayLen(ary))
                  val chars = Code.SelectRange(
                    arrayData(ary),
                    Some(start),
                    Some(end)
                  )
                  Code.PyString("").dot(Code.Ident("join"))(chars)
                }
              },
              1
            )
          ),
          (
            Identifier.Name("get_map_Array"),
            (
              { input =>
                Env.onLasts(input) {
                  case ary :: idx :: default :: fn :: Nil =>
                    val valid =
                      (!(idx :< Code.Const.Zero)).evalAnd(idx :< arrayLen(ary))
                    val item =
                      selectItem(arrayData(ary), arrayOffset(ary).evalPlus(idx))
                    Code.IfElse(
                      NonEmptyList.one((valid, fn(item))),
                      default(Code.Const.Unit)
                    )
                  case other =>
                    // $COVERAGE-OFF$
                    throw new IllegalStateException(
                      s"expected arity 4 got: $other"
                    )
                  // $COVERAGE-ON$
                }
              },
              4
            )
          ),
          (
            Identifier.Name("get_or_Array"),
            (
              { input =>
                Env.onLasts(input) {
                  case ary :: idx :: default :: Nil =>
                    val valid =
                      (!(idx :< Code.Const.Zero)).evalAnd(idx :< arrayLen(ary))
                    val item =
                      selectItem(arrayData(ary), arrayOffset(ary).evalPlus(idx))
                    Code.IfElse(
                      NonEmptyList.one((valid, item)),
                      default(Code.Const.Unit)
                    )
                  case other =>
                    // $COVERAGE-OFF$
                    throw new IllegalStateException(
                      s"expected arity 3 got: $other"
                    )
                  // $COVERAGE-ON$
                }
              },
              3
            )
          ),
          (
            Identifier.Name("foldl_Array"),
            (
              { input =>
                (
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar
                ).tupled.flatMap { case (data, offset, size, idx, acc) =>
                  Env.onLasts(input) {
                    case ary :: init :: fn :: Nil =>
                      Code
                        .block(
                          data := arrayData(ary),
                          offset := arrayOffset(ary),
                          size := arrayLen(ary),
                          idx := Code.Const.Zero,
                          acc := init,
                          Code.While(
                            idx :< size,
                            Code.block(
                              acc := fn(
                                acc,
                                selectItem(data, offset.evalPlus(idx))
                              ),
                              idx := idx + 1
                            )
                          )
                        )
                        .withValue(acc)
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
            Identifier.Name("map_Array"),
            (
              { input =>
                (
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar
                ).tupled.flatMap { case (data, offset, size, idx, out) =>
                  Env.onLasts(input) {
                    case ary :: fn :: Nil =>
                      Code
                        .block(
                          data := arrayData(ary),
                          offset := arrayOffset(ary),
                          size := arrayLen(ary),
                          out := Code.MakeList(Nil).evalTimes(size),
                          idx := Code.Const.Zero,
                          Code.While(
                            idx :< size,
                            Code.block(
                              selectItem(out, idx) := fn(
                                selectItem(data, offset.evalPlus(idx))
                              ),
                              idx := idx + 1
                            )
                          )
                        )
                        .withValue(makeArray(out, Code.Const.Zero, size))
                    case other =>
                      // $COVERAGE-OFF$
                      throw new IllegalStateException(
                        s"expected arity 2 got: $other"
                      )
                    // $COVERAGE-ON$
                  }
                }
              },
              2
            )
          ),
          (
            Identifier.Name("set_or_self_Array"),
            (
              { input =>
                (
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar
                ).tupled.flatMap { case (data, offset, size, copied) =>
                  Env.onLasts(input) {
                    case ary :: idx :: value :: Nil =>
                      val valid =
                        (!(idx :< Code.Const.Zero))
                          .evalAnd(idx :< arrayLen(ary))
                      val updated = Code
                        .block(
                          data := arrayData(ary),
                          offset := arrayOffset(ary),
                          size := arrayLen(ary),
                          copied := Code.Ident("list")(
                            Code.SelectRange(
                              data,
                              Some(offset),
                              Some(offset.evalPlus(size))
                            )
                          ),
                          selectItem(copied, idx) := value
                        )
                        .withValue(
                          makeArray(copied, Code.Const.Zero, size)
                        )
                      Code.IfElse(NonEmptyList.one((valid, updated)), ary)
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
            Identifier.Name("sort_Array"),
            (
              { input =>
                (
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar
                ).tupled.flatMap {
                  case (data, offset, size, items, i, j, curr) =>
                    Env.onLasts(input) {
                      case ary :: cmp :: Nil =>
                        val itemAtJ = selectItem(items, j)
                        val itemCmpCurr =
                          cmp(itemAtJ, curr) =:= Code.fromInt(2)
                        Code
                          .block(
                            data := arrayData(ary),
                            offset := arrayOffset(ary),
                            size := arrayLen(ary),
                            items := Code.Ident("list")(
                              Code.SelectRange(
                                data,
                                Some(offset),
                                Some(offset.evalPlus(size))
                              )
                            ),
                            i := Code.Const.One,
                            Code.While(
                              i :< size,
                              Code.block(
                                curr := selectItem(items, i),
                                j := i.evalMinus(Code.Const.One),
                                Code.While(
                                  (!(j :< Code.Const.Zero))
                                    .evalAnd(itemCmpCurr),
                                  Code.block(
                                    selectItem(items, j + 1) := itemAtJ,
                                    j := j.evalMinus(Code.Const.One)
                                  )
                                ),
                                selectItem(items, j + 1) := curr,
                                i := i + 1
                              )
                            )
                          )
                          .withValue(makeArray(items, Code.Const.Zero, size))
                      case other =>
                        // $COVERAGE-OFF$
                        throw new IllegalStateException(
                          s"expected arity 2 got: $other"
                        )
                      // $COVERAGE-ON$
                    }
                }
              },
              2
            )
          ),
          (
            Identifier.Name("concat_all_Array"),
            (
              { input =>
                (
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar
                ).tupled.flatMap {
                  case (
                        parts,
                        current,
                        total,
                        data,
                        write,
                        partIdx,
                        part,
                        partData,
                        partOffset,
                        partLen,
                        idx
                      ) =>
                    Env.onLast(input.head) { arrays =>
                      Code
                        .block(
                          parts := Code.MakeList(Nil),
                          current := arrays,
                          total := Code.Const.Zero,
                          Code.While(
                            isNonEmpty(current),
                            Code.block(
                              part := headList(current),
                              current := tailList(current),
                              Code.Call(
                                parts.dot(Code.Ident("append"))(part)
                              ),
                              total := total.evalPlus(arrayLen(part))
                            )
                          ),
                          data := Code.MakeList(Nil).evalTimes(total),
                          write := Code.Const.Zero,
                          partIdx := Code.Const.Zero,
                          Code.While(
                            partIdx :< parts.len(),
                            Code.block(
                              part := selectItem(parts, partIdx),
                              partData := arrayData(part),
                              partOffset := arrayOffset(part),
                              partLen := arrayLen(part),
                              idx := Code.Const.Zero,
                              Code.While(
                                idx :< partLen,
                                Code.block(
                                  selectItem(data, write.evalPlus(idx)) :=
                                    selectItem(
                                      partData,
                                      partOffset.evalPlus(idx)
                                    ),
                                  idx := idx + 1
                                )
                              ),
                              write := write.evalPlus(partLen),
                              partIdx := partIdx + 1
                            )
                          )
                        )
                        .withValue(makeArray(data, Code.Const.Zero, total))
                    }
                }
              },
              1
            )
          ),
          (
            Identifier.Name("slice_Array"),
            (
              { input =>
                (
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar,
                  Env.newAssignableVar
                ).tupled.flatMap { case (data, offset, size, start1, end1) =>
                  Env.onLasts(input) {
                    case ary :: start :: end :: Nil =>
                      val nonNegStart = !(start1 :< Code.Const.Zero)
                      val nonNegEnd = !(end1 :< Code.Const.Zero)
                      val ordered = !(start1 :> end1)
                      val endInRange = !(end1 :> size)
                      val sliceLen = end1.evalMinus(start1)
                      val valid = nonNegStart
                        .evalAnd(nonNegEnd)
                        .evalAnd(ordered)
                        .evalAnd(endInRange)
                        .evalAnd(sliceLen :> Code.Const.Zero)

                      val sliced = makeArray(
                        data,
                        offset.evalPlus(start1),
                        sliceLen
                      )
                      Code
                        .block(
                          data := arrayData(ary),
                          offset := arrayOffset(ary),
                          size := arrayLen(ary),
                          start1 := Code.Ternary(
                            Code.Const.Zero,
                            start :< Code.Const.Zero,
                            start
                          ),
                          end1 := Code.Ternary(size, end :> size, end)
                        )
                        .withValue(
                          Code.IfElse(
                            NonEmptyList.one((valid, sliced)),
                            emptyArray
                          )
                        )
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
          )
        )

      val floatResults
          : Map[Bindable, (List[ValueLike] => Env[ValueLike], Int)] =
        Map(
          (Identifier.Name("abs"), (unaryMath("fabs"), 1)),
          (Identifier.Name("acos"), (unaryMath("acos"), 1)),
          (Identifier.Name("asin"), (unaryMath("asin"), 1)),
          (Identifier.Name("atan"), (unaryMath("atan"), 1)),
          (Identifier.Name("atan2"), (binaryMath("atan2"), 2)),
          (Identifier.Name("ceil"), (unaryMath("ceil"), 1)),
          (Identifier.Name("cos"), (unaryMath("cos"), 1)),
          (Identifier.Name("cosh"), (unaryMath("cosh"), 1)),
          (Identifier.Name("exp"), (unaryMath("exp"), 1)),
          (Identifier.Name("floor"), (unaryMath("floor"), 1)),
          (Identifier.Name("hypot"), (binaryMath("hypot"), 2)),
          (Identifier.Name("log"), (unaryMath("log"), 1)),
          (Identifier.Name("log10"), (unaryMath("log10"), 1)),
          (Identifier.Name("pow"), (binaryMath("pow"), 2)),
          (Identifier.Name("sin"), (unaryMath("sin"), 1)),
          (Identifier.Name("sinh"), (unaryMath("sinh"), 1)),
          (Identifier.Name("sqrt"), (unaryMath("sqrt"), 1)),
          (Identifier.Name("tan"), (unaryMath("tan"), 1)),
          (Identifier.Name("tanh"), (unaryMath("tanh"), 1)),
          (
            Identifier.Name("copy_sign"),
            (binaryMath("copysign"), 2)
          ),
          (Identifier.Name("is_nan"), (unaryMath("isnan"), 1)),
          (
            Identifier.Name("is_infinite"),
            (unaryMath("isinf"), 1)
          ),
          (
            Identifier.Name("float64_to_String"),
            (floatToStringFn, 1)
          ),
          (
            Identifier.Name("string_to_Float64"),
            (stringToFloatFn, 1)
          ),
          (
            Identifier.Name("int_bits_to_Float64"),
            (intBitsToFloatFn, 1)
          ),
          (
            Identifier.Name("float64_bits_to_Int"),
            (floatBitsToIntFn, 1)
          ),
          (
            Identifier.Name("float64_to_Int"),
            (floatToIntFn, 1)
          ),
          (
            Identifier.Name("int_to_Float64"),
            (intToFloatFn, 1)
          )
        )

      val resultsByPackage: Map[
        PackageName,
        Map[Bindable, (List[ValueLike] => Env[ValueLike], Int)]
      ] =
        Map(
          predefPackage -> results,
          arrayPackage -> arrayResults,
          float64Package -> floatResults
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
          case Global(_, p, name, _) =>
            resultsByPackage.get(p).flatMap(_.get(name))
          case _ => None
        }

      def makeLambda(
          arity: Int
      )(fn: List[ValueLike] => Env[ValueLike]): Env[ValueLike] =
        if (arity == 0) fn(Nil)
        else
          for {
            vars <- (1 to arity).toList.traverse(_ => Env.newAssignableVar)
            body <- fn(vars)
            // TODO: if body isn't an expression, how can just adding a lambda
            // at the end be correct? the arguments will be below points that used it.
            // the onLast has to handle Code.Lambda specially
            res <- Env.onLast(body)(Code.Lambda(vars, _))
          } yield res
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
            case MakeEnum(variant, _, famArities, _) =>
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
            case MakeStruct(arity, _) =>
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
          case EqualsLit(expr, lit: dev.bosatsu.Lit.Float64) =>
            val literal = Code.litToExpr(lit)
            if (java.lang.Double.isNaN(lit.toDouble)) {
              Env.importLiteral(NonEmptyList.one(Code.Ident("math"))).flatMap {
                math =>
                  loop(expr, slotName, inlineSlots)
                    .flatMap(
                      Env.onLast(_)(ex => math.dot(Code.Ident("isnan"))(ex))
                    )
              }
            } else {
              loop(expr, slotName, inlineSlots)
                .flatMap(Env.onLast(_)(ex => ex =:= literal))
            }
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
          case SetMut(LocalAnonMut(mut, _), expr) =>
            (Env.nameForAnon(mut), loop(expr, slotName, inlineSlots)).flatMapN {
              (ident, result) =>
                Env.onLast(result) { resx =>
                  (ident := resx).withValue(Code.Const.True)
                }
            }
          case LetBool(n, v, in) =>
            doLet(
              n,
              v,
              boolExpr(in, slotName, inlineSlots),
              slotName,
              inlineSlots
            )
          case LetMutBool(_, in) =>
            // in python we just ignore this
            boolExpr(in, slotName, inlineSlots)
        }

      // InlineSlots holds resolved Python expressions, not the original
      // Matchless Exprs. Locals/LocalAnon are immutable and already renamed
      // to unique Python idents (shadowing-safe), and Globals are stable.
      // Inlining these is safe because it captures the correct binding.
      private def inlineableCapture(expr: Expr[K]): Boolean =
        expr match {
          case Local(_, _) | Global(_, _, _, _) | LocalAnon(_, _) => true
          case _                                         => false
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
          case Lambda(captures, recName, args, body, _) =>
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
          case Left(LocalAnon(l, _)) =>
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
          case Lambda(captures, recName, args, res, _) =>
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

          case WhileExpr(cond, effect, res, _) =>
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
          case Global(k, p, n, _) =>
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
          case Local(b, _)         => Env.deref(b)
          case LocalAnon(a, _)     => Env.nameForAnon(a)
          case LocalAnonMut(m, _)  => Env.nameForAnon(m)
          case ClosureSlot(idx, _) =>
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
          case App(PredefExternal((fn, _)), args, _) =>
            args.toList
              .traverse(loop(_, slotName, inlineSlots))
              .flatMap(fn)
          case App(cons: ConsExpr, args, _) =>
            args.traverse(loop(_, slotName, inlineSlots)).flatMap { pxs =>
              makeCons(cons, pxs.toList)
            }
          case App(expr, args, _) =>
            (
              loop(expr, slotName, inlineSlots),
              args.traverse(loop(_, slotName, inlineSlots))
            ).mapN { (fn, args) =>
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
          case Let(localOrBind, fn: Lambda[?], in, _) =>
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
              case Left(LocalAnon(l, _)) =>
                val inF = loop(in, slotName, inlineSlots)
                // anonymous names never shadow
                Env
                  .nameForAnon(l)
                  .flatMap { bi =>
                    val v = topFn(bi, fn, slotName, inlineSlots)
                    (v, inF).mapN(_.withValue(_))
                  }
            }
          case Let(localOrBind, notFn, in, _) =>
            // we know that notFn is not Lambda here
            doLet(
              localOrBind,
              notFn,
              loop(in, slotName, inlineSlots),
              slotName,
              inlineSlots
            )
          case LetMut(LocalAnonMut(_, _), in, _) =>
            // we could delete this name, but
            // there is no need to
            loop(in, slotName, inlineSlots)
          case Literal(lit, _)         => Env.pure(Code.litToExpr(lit))
          case ifExpr @ If(_, _, _, _) =>
            val (ifs, last) = ifExpr.flatten

            val ifsV = ifs.traverse { case (c, t) =>
              (
                boolExpr(c, slotName, inlineSlots),
                loop(t, slotName, inlineSlots)
              ).tupled
            }

            (ifsV, loop(last, slotName, inlineSlots)).mapN { (ifs, elseV) =>
              Env.ifElse(ifs, elseV)
            }.flatten

          case Always.SetChain(setmuts, result) =>
            (
              setmuts.traverse { case (LocalAnonMut(mut, _), v) =>
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
          case Always(cond, expr, _) =>
            (
              boolExpr(cond, slotName, inlineSlots).map(Code.always),
              loop(expr, slotName, inlineSlots)
            )
              .mapN(_.withValue(_))

          case GetEnumElement(expr, _, idx, _, _) =>
            // nonempty enums are just structs with the first element being the variant
            // we could assert the v matches when debugging, but typechecking
            // should assure this
            loop(expr, slotName, inlineSlots).flatMap { tup =>
              Env.onLast(tup)(_.get(idx + 1))
            }
          case GetStructElement(expr, idx, sz, _) =>
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
          case PrevNat(expr, _) =>
            // Nats are just integers
            loop(expr, slotName, inlineSlots).flatMap { nat =>
              Env.onLast(nat)(_.evalMinus(Code.Const.One))
            }
          case cons: ConsExpr => makeCons(cons, Nil)
        }
    }
  }
}
