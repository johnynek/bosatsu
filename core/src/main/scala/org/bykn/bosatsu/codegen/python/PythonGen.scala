package org.bykn.bosatsu.codegen.python

import org.typelevel.paiges.Doc
import org.bykn.bosatsu.{PackageName, Identifier, Matchless, RecursionKind, Par, Parser}
import cats.Monad
import cats.data.{NonEmptyList, State}
import scala.concurrent.ExecutionContext

import Identifier.Bindable
import Matchless._

import cats.implicits._

object PythonGen {
  import Code.{ValueLike, Statement, Expression}

  type Module = NonEmptyList[Code.Ident]

  sealed abstract class Env[+A]
  object Env {
    import Code._

    implicit def envMonad: Monad[Env] =
      new Monad[Env] {
        import Impl._

        val m = Monad[State[EnvState, ?]]
        def pure[A](a: A) = EnvImpl(m.pure(a))
        override def map[A, B](ea: Env[A])(fn: A => B): Env[B] =
          EnvImpl(m.map(ea.state)(fn))
        def flatMap[A, B](ea: Env[A])(fn: A => Env[B]): Env[B] =
          EnvImpl(m.flatMap(ea.state)(fn.andThen(_.state)))
        def tailRecM[A, B](a: A)(fn: A => Env[Either[A, B]]): Env[B] =
          EnvImpl(m.tailRecM(a)(fn.andThen(_.state)))
      }

    private object Impl {

      case class EnvState(
        imports: Map[Module, Code.Ident],
        bindings: Map[Bindable, (Int, List[Code.Ident])],
        tops: Set[Bindable],
        nextTmp: Long) {

        private def bindInc(b: Bindable, inc: Int)(fn: Int => Code.Ident): (EnvState, Code.Ident) = {
          val (c, s) = bindings.getOrElse(b, (0, Nil))
          val pname = fn(c)

          (copy(
            bindings = bindings.updated(b, (c + inc, pname :: s))
          ), pname)
        }

        def bind(b: Bindable): (EnvState, Code.Ident) =
          bindInc(b, 1) { c =>
            Code.Ident(escapeRaw("___b", b.sourceCodeRepr + c.toString))
          }

        def bindTop(b: Bindable): EnvState =
          subs(b, escape(b))

        // in loops we need to substitute
        // bindings for mutable variables
        def subs(b: Bindable, c: Code.Ident): EnvState =
          bindInc(b, 0)(_ => c)._1

        def deref(b: Bindable): Code.Ident =
          // see if we are shadowing, or top level
          bindings.get(b) match {
            case Some((_, h :: _)) => h
            case _ if tops(b) => escape(b)
            case other =>
              throw new IllegalStateException(s"unexpected deref: $b with bindings: $other")
          }

        def unbind(b: Bindable): EnvState =
          bindings.get(b) match {
            case Some((cnt, _ :: tail)) =>
              copy(bindings = bindings.updated(b, (cnt, tail)))
            case other =>
              throw new IllegalStateException(s"invalid scope: $other for $b with $bindings")
          }

        def getNextTmp: (EnvState, Long) = (copy(nextTmp = nextTmp + 1L), nextTmp)

        def topLevel(b: Bindable): (EnvState, Code.Ident) =
          (copy(tops = tops + b), escape(b))

        def addImport(mod: Module): (EnvState, Code.Ident) =
          imports.get(mod) match {
            case Some(alias) => (this, alias)
            case None =>
              val impNumber = imports.size
              val alias = Code.Ident(escapeRaw("___i", mod.last.name + impNumber.toString))
              (copy(imports = imports.updated(mod, alias)), alias)
          }

        def importStatements: List[Code.Import] =
          imports
            .iterator
            .map { case (path, alias) =>
              val modName = path.map(_.name).toList.mkString(".")
              Code.Import(modName, Some(alias))
            }
            .toList
            .sortBy(_.modname)
      }

      implicit class EnvOps[A](val env: Env[A]) extends AnyVal {
        def state: State[EnvState, A] =
          env match {
            case EnvImpl(s) => s
          }
      }

      def emptyState: EnvState =
        EnvState(Map.empty, Map.empty, Set.empty, 0L)

      case class EnvImpl[A](state: State[EnvState, A]) extends Env[A]

      def env[A](fn: EnvState => (EnvState, A)): Env[A] =
        EnvImpl(State(fn))

      def read[A](fn: EnvState => A): Env[A] =
        EnvImpl(State { state => (state, fn(state)) })

      def update(fn: EnvState => EnvState): Env[Unit] =
        EnvImpl(State { state => (fn(state), ()) })

      def run[A](env: Env[A]): (EnvState, A) =
        env.state.run(emptyState).value
    }

    def render(env: Env[List[Statement]]): Doc = {
      val (state, stmts) = Impl.run(env)

      val imps = state.importStatements

      val impDocs = Doc.intercalate(Doc.hardLine, imps.map(Code.toDoc))
      val twoLines = Doc.hardLine + Doc.hardLine
      Doc.intercalate(twoLines, impDocs :: stmts.map(Code.toDoc))
    }

    // allocate a unique identifier for b
    def bind(b: Bindable): Env[Code.Ident] =
      Impl.env(_.bind(b))

    // point this name to the top level name
    def bindTop(b: Bindable): Env[Unit] =
      Impl.update(_.bindTop(b))

    def subs(b: Bindable, i: Code.Ident): Env[Unit] =
      Impl.update(_.subs(b, i))

    // get the mapping for a name in scope
    def deref(b: Bindable): Env[Code.Ident] =
      Impl.read(_.deref(b))

    // release the current scope for b
    def unbind(b: Bindable): Env[Unit] =
      Impl.update(_.unbind(b))

    def nameForAnon(long: Long): Env[Code.Ident] =
      Monad[Env].pure(Code.Ident(s"___a$long"))

    def newAssignableVar: Env[Code.Ident] =
      Impl.env(_.getNextTmp)
        .map { long =>
          Code.Ident(s"___t$long")
        }

    def importPackage(pack: PackageName): Env[Code.Ident] =
      importEscape(pack.parts)

    def importEscape(parts: NonEmptyList[String]): Env[Code.Ident] =
      importLiteral(parts.map(escapeModule))

    def importLiteral(parts: NonEmptyList[Code.Ident]): Env[Code.Ident] =
      Impl.env(_.addImport(parts))

    // top level names are imported across files so they have
    // to be consistently transformed
    def topLevelName(n: Bindable): Env[Code.Ident] =
      Impl.env(_.topLevel(n))

    def onLasts(cs: List[ValueLike])(fn: List[Expression] => ValueLike): Env[ValueLike] = {
      def loop(cs: List[ValueLike], setup: List[Statement], args: List[Expression]): Env[ValueLike] =
        cs match {
          case Nil => Monad[Env].pure {
            val res = fn(args.reverse)
            NonEmptyList.fromList(setup) match {
              case None => res
              case Some(nel) =>
                WithValue(Block(nel.reverse), res)
            }
          }
          case (e: Expression) :: t => loop(t, setup, e :: args)
          case (ifelse@IfElse(_, _)) :: tail =>
            // we allocate a result and assign
            // the result on each value
            Env.newAssignableVar.flatMap { v =>
              loop(tail, addAssign(v, ifelse) :: setup, v :: args)
            }
          case WithValue(decl, v) :: tail =>
            loop(v :: tail, decl :: setup, args)
        }

      loop(cs, Nil, Nil)
    }

    def ifElse(conds: NonEmptyList[(ValueLike, ValueLike)], elseV: ValueLike): Env[ValueLike] = {
      // for all the non-expression conditions, we need to defer evaluating them
      // until they are really needed
      conds match {
        case NonEmptyList((cx: Expression, t), Nil) =>
          Monad[Env].pure(IfElse(NonEmptyList((cx, t), Nil), elseV))
        case NonEmptyList((cx: Expression, t), rh :: rt) =>
          val head = (cx, t)
          ifElse(NonEmptyList(rh, rt), elseV).map {
            case IfElse(crest, er) =>
              // preserve IfElse chains
              IfElse(head :: crest, er)
            case nest =>
              IfElse(NonEmptyList(head, Nil), nest)
          }
        case NonEmptyList((cx, t), rest) =>
          for {
            // allocate a new unshadowable var
            cv <- Env.newAssignableVar
            res <- ifElse(NonEmptyList((cv, t), rest), elseV)
          } yield WithValue(addAssign(cv, t), res)
      }
    }

    def onLast(c: ValueLike)(fn: Expression => ValueLike): Env[ValueLike] =
      onLasts(c :: Nil) {
        case x :: Nil => fn(x)
        case other =>
          throw new IllegalStateException(s"expected list to have size 1: $other")
      }

    def andCode(c1: ValueLike, c2: ValueLike): Env[ValueLike] =
      onLasts(c1 :: c2 :: Nil) {
        case e1 :: e2 :: Nil =>
          Op(e1, Const.And, e2)
        case other =>
          throw new IllegalStateException(s"expected list to have size 2: $other")
      }

    def toReturn(v: ValueLike): Env[Statement] =
      v match {
        case x: Expression =>
          Monad[Env].pure(Code.Return(x))
        case WithValue(stmt, v) =>
          toReturn(v).map(stmt :+ _)
        case ie@IfElse(_, _) =>
          Env.newAssignableVar.map { resName =>
            Code.addAssign(resName, ie) :+ Code.Return(resName)
          }
      }

    def makeDef(defName: Code.Ident, arg: Code.Ident, v: ValueLike): Env[Code.Def] =
      toReturn(v).map(Code.Def(defName, arg :: Nil, _))

    def makeCurriedDef(name: Ident, args: NonEmptyList[Ident], body: ValueLike): Env[Statement] =
      args match {
        case NonEmptyList(a, Nil) =>
          //  base case
          makeDef(name, a, body)
        case NonEmptyList(a, h :: t) =>
          for {
            newName <- Env.newAssignableVar
            fn <- makeCurriedDef(newName, NonEmptyList(h, t), body)
          } yield Code.Def(name, a :: Nil, fn :+ Code.Return(newName))
      }


    def replaceTailCallWithAssign(name: Ident, argSize: Int, body: ValueLike)(onArgs: List[Expression] => Statement): Env[ValueLike] = {
      val initBody = body
      def loop(body: ValueLike): Env[ValueLike] =
        body match {
          case a@Apply(_, _) =>
            // we have recurried the args by this point:
            val (fn0, args0) = a.uncurry
            if (fn0 == name) {
              val flatArgs = args0.flatten
              if (flatArgs.length == argSize) {
                val all = onArgs(flatArgs)
                // set all the values and return the empty tuple
                Monad[Env].pure(WithValue(all, MakeTuple(Nil)))
              }
              else {
                throw new IllegalStateException(s"expected a tailcall for $name in $initBody, but found: $a")
              }
            }
            else {
              Monad[Env].pure(a)
            }
          case Parens(p) => loop(p).flatMap(onLast(_)(Parens(_)))
          case IfElse(ifCases, elseCase) =>
            // only the result types are in tail position, we don't need to recurse on conds
            val ifs = ifCases.traverse { case (cond, res) => loop(res).map((cond, _)) }
            (ifs, loop(elseCase)).mapN(IfElse(_, _))
          case WithValue(stmt, v) =>
            loop(v).map(WithValue(stmt, _))
          // the rest cannot have a call in the tail position
          case DotSelect(_, _) | Op(_, _, _) | Lambda(_, _) | MakeTuple(_) | SelectItem(_, _) | Ident(_) | Literal(_) | PyString(_) | PyInt(_) => Monad[Env].pure(body)
        }

      loop(initBody)
    }

    // these are always recursive so we can use def to define them
    def buildLoop(selfName: Ident, fnMutArgs: NonEmptyList[(Ident, Ident)], body: ValueLike): Env[Statement] = {
      /*
       * bodyUpdate = body except App(foo, args) is replaced with
       * reseting the inputs, and setting cont to True and having
       * the value ()
       *
       * def foo(a)(b)(c):
       *   cont = True
       *   res = ()
       *   while cont:
       *     cont = False
       *     res = bodyUpdate
       *   return res
       */
      val fnArgs = fnMutArgs.map(_._1)
      val mutArgs = fnMutArgs.map(_._2)

      def assignMut(cont: Code.Ident)(args: List[Expression]): Statement = {
        // do the replacement
        val vs = mutArgs.toList.zip(args).map { case (v, x) => Assign(v, x) }
        vs.foldLeft(Assign(cont, Const.True): Statement)(_ +: _)
      }

      for {
        cont <- Env.newAssignableVar
        ac = assignMut(cont)(fnArgs.toList)
        res <- Env.newAssignableVar
        ar = Assign(res, MakeTuple(Nil))
        body1 <- replaceTailCallWithAssign(selfName, mutArgs.length, body)(assignMut(cont))
        setRes = addAssign(res, body1)
        loop = While(cont, Assign(cont, Const.False) +: setRes)
        newBody = WithValue(ac +: ar +: loop, res)
        curried <- makeCurriedDef(selfName, fnArgs, newBody)
      } yield curried
    }

  }

  private[this] val base62Items = (('0' to '9') ++ ('A' to 'Z') ++ ('a' to 'z')).toSet

  private def toBase62(c: Char): String =

    if (base62Items(c)) c.toString
    else if (c == '_') "__"
    else {
      def toChar(i0: Int): Char =
        if (i0 < 0) sys.error(s"invalid in: $i0")
        else if (i0 < 10) (i0 + '0'.toInt).toChar
        else if (i0 < 36) (i0 - 10 + 'A'.toInt).toChar
        else if (i0 < 62) (i0 - 36 + 'a'.toInt).toChar
        else sys.error(s"invalid int: $i0")

      def toString(i: Int): String = {
        if (i < 62) toChar(i).toString
        else {
          val i0 = i % 62
          val i1 = i / 62
          toString(i1) + toChar(i0)
        }
      }

      "_" + toString(c.toInt) + "_"
    }

  private def escapeRaw(prefix: String, str: String): String =
    str.map(toBase62).mkString(prefix, "", "")

  private def unBase62(str: String, offset: Int, bldr: java.lang.StringBuilder): Int = {
    var idx = offset
    var num = 0

    while(idx < str.length) {
      val c = str.charAt(idx)
      idx += 1
      if (c == '_') {
        if (idx != offset + 1) {
          // done
          val numC = num.toChar
          bldr.append(numC)
          return (idx - offset)
        }
        else {
          // "__" decodes to "_"
          bldr.append('_')
          return (idx - offset)
        }
      }
      else {
        val base =
          if (c <= '9') '0'.toInt
          else if (c <= 'Z') ('A'.toInt - 10)
          else ('a'.toInt - 36)

        num = num * 62 + c.toInt - base
      }
    }
    return -1
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
    val str = n.sourceCodeRepr
    if (!str.startsWith("___") && Code.python2Name.matcher(str).matches && !Code.pyKeywordList(str)) Code.Ident(str)
    else {
      // we need to escape
      Code.Ident(escapeRaw("___n", str))
    }
  }

  def escapeModule(str: String): Code.Ident = {
    if (!str.startsWith("___") && Code.python2Name.matcher(str).matches && !Code.pyKeywordList(str)) Code.Ident(str)
    else {
      // we need to escape
      Code.Ident(escapeRaw("___m", str))
    }
  }

  def unescape(ident: Code.Ident): Option[Bindable] = {
    val str = ident.name
    val decode =
      if (str.startsWith("___n")) {
        val bldr = new java.lang.StringBuilder()
        var idx = 4
        while (idx < str.length) {
          val c = str.charAt(idx)
          idx += 1
          if (c == '_') {
            val res = unBase62(str, idx, bldr)
            if (res < 1) return None
            else {
              idx += res
            }
          }
          else {
            bldr.append(c)
          }
        }

        bldr.toString()
      }
      else {
        str
      }

    Identifier.optionParse(Identifier.bindableParser, decode)
  }

  // this package should be writen to the path
  // at foo/bar/baz.py
  //
  // relative to some base
  def packageToFile(parts: NonEmptyList[String]): Module =
    parts.map(escapeModule)

  /**
   * Remap is used to handle remapping external values
   */
  def apply(packName: PackageName, name: Bindable, me: Expr)(remap: (PackageName, Bindable) => Env[Option[ValueLike]]): Env[Statement] = {
    val ops = new Impl.Ops(packName, remap)
    for {
      ve <- ops.loop(me)
      nm <- Env.topLevelName(name)
      stmt <- ops.topLet(nm, me, ve)
    } yield stmt
  }

  // parses a nested map with
  //
  // { packageName: { bind: foo.bar.baz } }
  //
  val externalParser: fastparse.all.P[List[(PackageName, Bindable, Module, Code.Ident)]] = {
    import fastparse.all._

    val identParser: P[Code.Ident] = Parser.py2Ident.map(Code.Ident(_))
    val modParser: P[(Module, Code.Ident)] =
      identParser
        .rep(sep = P("."), min = 2)
        .map { items =>
          // min = 1 ensures this is safe
          (NonEmptyList.fromListUnsafe(items.init.toList), items.last)
        }

    val inner: P[List[(Bindable, (Module, Code.Ident))]] =
      Parser.dictLikeParser(Identifier.bindableParser, modParser)

    val outer: P[List[(PackageName, List[(Bindable, (Module, Code.Ident))])]] =
      Parser.dictLikeParser(PackageName.parser, inner) ~ Parser.maybeSpacesAndLines

    outer.map { items =>
      items.flatMap { case (p, bs) =>
        bs.map { case (b, (m, i)) => (p, b, m, i) }
      }
    }
  }

  // compile a set of packages given a set of external remappings
  def renderAll(pm: Map[PackageName, List[(Bindable, Expr)]], externals: Map[(PackageName, Bindable), (Module, Code.Ident)])(implicit ec: ExecutionContext): Map[PackageName, (Module, Doc)] = {
    val externalRemap: (PackageName, Bindable) => Env[Option[ValueLike]] =
      { (p, b) =>
        externals.get((p, b)) match {
          case None => Monad[Env].pure(None)
          case Some((m, i)) =>
            Env.importLiteral(m)
              .map { alias => Some(Code.DotSelect(alias, i)) }
        }
      }

    val all = pm
      .toList
      .traverse { case (p, lets) =>
        Par.start {
          val stmts =
            lets
              .traverse { case (b, x) =>
                apply(p, b, x)(externalRemap)
              }
          val modName = p.parts.map(escapeModule)
          (p, (modName, Env.render(stmts)))
        }
      }

    Par.await(all).toMap
  }

  private object Impl {

    object PredefExternal {
      val results: Map[Bindable, (List[ValueLike] => Env[ValueLike], Int)] =
        Map(
          Identifier.unsafeBindable("add") -> {
            ({
              input => Env.onLasts(input) {
                case arg0 :: arg1 :: Nil => Code.Op(arg0, Code.Const.Plus, arg1)
                case other => throw new IllegalStateException(s"expected arity 2 got: $other")
              }
            }, 2)
          })

      def unapply(expr: Expr): Option[(List[ValueLike] => Env[ValueLike], Int)] =
        expr match {
          case Global(PackageName.PredefName, name) => results.get(name)
          case _ => None
        }
    }

    class Ops(packName: PackageName, remap: (PackageName, Bindable) => Env[Option[ValueLike]]) {
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
            case MakeEnum(variant, arity) =>
              val vExpr = Code.fromInt(variant)
              if (arity == 0) Monad[Env].pure(vExpr)
              else {
                // we make a tuple with the variant in the first position
                Env.onLasts(vExpr :: args)(Code.MakeTuple(_))
              }
            case MakeStruct(arity) =>
                if (arity == 0) Monad[Env].pure(Code.MakeTuple(Nil))
                else if (arity == 1) Monad[Env].pure(args.head)
                else Env.onLasts(args)(Code.MakeTuple(_))
            case ZeroNat =>
              Monad[Env].pure(Code.Const.Zero)
            case SuccNat =>
              Env.onLast(args.head)(Code.Op(_, Code.Const.Plus, Code.Const.One))
          }

        val sz = args.size
        def makeLam(cnt: Int, args: List[ValueLike]): Env[ValueLike] =
          if (cnt == 0) applyAll(args)
          else if (cnt < 0) {
            // too many args, this shouldn't typecheck
            throw new IllegalStateException(s"invalid arity $sz for $ce")
          }
          else {
            // add an arg to the right
            for {
              v <- Env.newAssignableVar
              body <- makeLam(cnt - 1, args :+ v)
              res <- Env.onLast(body)(Code.Lambda(v :: Nil, _))
            } yield res
          }

        makeLam(ce.arity - sz, args)
      }

      def boolExpr(ix: BoolExpr): Env[ValueLike] =
        ix match {
          case EqualsLit(expr, lit) =>
            val literal = Code.litToExpr(lit)
            loop(expr).flatMap(Env.onLast(_) { ex => Code.Op(ex, Code.Const.Eq, literal) })
          case EqualsNat(nat, zeroOrSucc) =>
            val natF = loop(nat)

            if (zeroOrSucc.isZero)
              natF.flatMap(Env.onLast(_) { x =>
                Code.Op(x, Code.Const.Eq, Code.Const.Zero)
              })
            else
              natF.flatMap(Env.onLast(_) { x =>
                Code.Op(x, Code.Const.Gt, Code.Const.Zero)
              })

          case TrueConst => Monad[Env].pure(Code.Const.True)
          case And(ix1, ix2) =>
            (boolExpr(ix1), boolExpr(ix2))
              .mapN(Env.andCode(_, _))
              .flatten
          case CheckVariant(enumV, idx, size) =>
            loop(enumV).flatMap { tup =>
              Env.onLast(tup) { t =>
                val idxExpr = Code.fromInt(idx)
                if (size == 0) {
                  // this is represented as an integer
                  Code.Op(t, Code.Const.Eq, idxExpr)
                }
                else
                  Code.Op(Code.SelectItem(t, 0), Code.Const.Eq, idxExpr)
              }
            }
          case SetMut(LocalAnonMut(mut), expr) =>
            (Env.nameForAnon(mut), loop(expr))
              .mapN { (ident, result) =>
                Env.onLast(result) { resx =>
                  val a = Code.Assign(ident, resx)
                  Code.WithValue(a, Code.Const.True)
                }
              }
              .flatten
          case MatchString(str, pat, binds) => ???
          case SearchList(LocalAnonMut(mutV), init, check, optLeft) => ???
        }

      def topLet(name: Code.Ident, expr: Expr, v: ValueLike): Env[Statement] = {

        /*
         * def anonF():
         *   code
         *
         * name = anonF()
         */
        lazy val worstCase: Env[Statement] =
          v match {
            case ex: Expression =>
              Monad[Env].pure(Code.Assign(name, ex))
            case _ =>
              (Env.newAssignableVar, Env.toReturn(v)).mapN { (defName, body) =>
                val newDef = Code.Def(defName, Nil, body)

                newDef :+ Code.Assign(name, Code.Apply(defName, Nil))
              }
          }

        expr match {
          case l@LoopFn(_, nm, h, t, b) =>
            // note, name is already bound
            val args = NonEmptyList(h, t)
            val boundA = args.traverse(Env.bind)
            val subsA = args.traverse { a =>
              for {
                mut <- Env.newAssignableVar
                _ <- Env.subs(a, mut)
              } yield (a, mut)
            }

            val unbindA = args.traverse_(Env.unbind)

            for {
              as <- boundA
              subs <- subsA
              subs1 = as.zipWith(subs) { case (b, (_, m)) => (b, m) }
              body <- loop(b)
              loopRes <- Env.buildLoop(name, subs1, body)
              // we have bound this name twice, once for the top and once for substitution
              _ <- subs.traverse_ { case (a, _) => Env.unbind(a) }
              _ <- unbindA
            } yield loopRes

          case Lambda(caps, arg, body) =>
            // this isn't recursive, or it would be in a Let
            (Env.bind(arg), loop(body))
              .mapN(Env.makeDef(name, _, _))
              .flatMap { d =>
                d <* Env.unbind(arg)
              }
          case Let(Right((n, RecursionKind.Recursive)), Lambda(_, arg, body), Local(n2)) if n == n2 =>
            if (escape(n) == name) {
              // this is a recursive value in scope for itself
              // but we need to bind the local name to the global name
              for {
                _ <- Env.bindTop(n)
                arg1 <- Env.bind(arg)
                body1 <- loop(body)
                def1 <- Env.makeDef(name, arg1, body1)
                _ <- Env.unbind(arg)
                _ <- Env.unbind(n)
              } yield def1
            }
            else {
              worstCase
            }

          case _ => worstCase
        }
      }

      def loop(expr: Expr): Env[ValueLike] =
        expr match {
          case Lambda(_, arg, res) =>
            // python closures work the same so we don't
            // need to worry about what we capture
            (Env.bind(arg), loop(res)).mapN { (arg, res) =>
              res match {
                case x: Expression =>
                  Monad[Env].pure(Code.Lambda(arg :: Nil, x))
                case v =>
                  for {
                    defName <- Env.newAssignableVar
                    defn <- Env.makeDef(defName, arg, v)
                  } yield Code.WithValue(defn, defName)
              }
            }
            .flatMap(_ <* Env.unbind(arg))
          case LoopFn(_, thisName, argshead, argstail, body) =>
            // note, thisName is already bound because LoopFn
            // is a lambda, not a def
            // closures capture the same in python, we can ignore captures
            val args = NonEmptyList(argshead, argstail)

            val boundA = args.traverse(Env.bind)
            val subsA = args.traverse { a =>
              for {
                mut <- Env.newAssignableVar
                _ <- Env.subs(a, mut)
              } yield (a, mut)
            }

            val unbindA = args.traverse_(Env.unbind)

            for {
              nameI <- Env.deref(thisName)
              as <- boundA
              subs <- subsA
              body <- loop(body)
              subs1 = as.zipWith(subs) { case (b, (_, m)) => (b, m) }
              loopRes <- Env.buildLoop(nameI, subs1, body)
              // we have bound the args twice: once as args, once as interal muts
              _ <- subs.traverse_ { case (a, _) => Env.unbind(a) }
              _ <- unbindA
            } yield Code.WithValue(loopRes, nameI)

          case PredefExternal((fn, arity)) =>
            // make a lambda
            ???
          case Global(p, n) =>
            remap(p, n)
              .flatMap {
                case Some(v) => Monad[Env].pure(v)
                case None =>
                  if (p == packName) {
                    // This is just a name in the local package
                    Env.topLevelName(n)
                  }
                  else {
                    (Env.importPackage(p), Env.topLevelName(n)).mapN(Code.DotSelect(_, _))
                  }
              }
          case Local(b) => Env.deref(b)
          case LocalAnon(a) => Env.nameForAnon(a)
          case LocalAnonMut(m) => Env.nameForAnon(m)
          case App(PredefExternal((fn, arity)), args) if args.length == arity =>
            args
              .toList
              .traverse(loop)
              .flatMap(fn)
          case App(cons: ConsExpr, args) =>
            args.traverse(loop).flatMap { pxs => makeCons(cons, pxs.toList) }
          case App(expr, args) =>
            (loop(expr), args.traverse(loop))
              .mapN { (fn, args) =>
                Env.onLasts(fn :: args.toList) {
                  case fn :: ah :: atail =>
                    // all functions are curried, a future
                    // optimization would improve that
                    atail.foldLeft(Code.Apply(fn, ah :: Nil)) { (left, arg) =>
                      Code.Apply(left, arg :: Nil)
                    }
                  case other => throw new IllegalStateException(s"got $other, expected to match $expr")
                }
              }
              .flatten
          case Let(localOrBind, value, in) =>
            val inF = loop(in)

            localOrBind match {
              case Right((b, rec)) =>
                if (rec.isRecursive) {
                  // value b is in scope first
                  for {
                    bi <- Env.bind(b)
                    ve <- loop(value)
                    tl <- topLet(bi, value, ve)
                    ine <- inF
                    wv = Code.WithValue(tl, ine)
                    _ <- Env.unbind(b)
                  } yield wv
                }
                else {
                  // value b is in scope after ve
                  for {
                    ve <- loop(value)
                    bi <- Env.bind(b)
                    tl <- topLet(bi, value, ve)
                    ine <- inF
                    wv = Code.WithValue(tl, ine)
                    _ <- Env.unbind(b)
                  } yield wv
                }
              case Left(LocalAnon(l)) =>
                // anonymous names never shadow
                (Env.nameForAnon(l), loop(value))
                  .mapN { (bi, vE) =>
                    (topLet(bi, value, vE), inF)
                      .mapN(Code.WithValue(_, _))
                  }
                  .flatten
            }

          case LetMut(LocalAnonMut(_), in) =>
            // we could delete this name, but
            // there is no need to
            loop(in)
          case Literal(lit) => Monad[Env].pure(Code.litToExpr(lit))
          case If(cond, thenExpr, elseExpr) =>
            def combine(expr: Expr): (List[(BoolExpr, Expr)], Expr) =
              expr match {
                case If(c1, t1, e1) =>
                  val (ifs, e2) = combine(e1)
                  (ifs :+ ((c1, t1)), e1)
                case last => (Nil, last)
              }

            val (rest, last) = combine(elseExpr)
            val ifs = NonEmptyList((cond, thenExpr), rest)

            val ifsV = ifs.traverse { case (c, t) =>
              (boolExpr(c), loop(t)).tupled
            }

            (ifsV, loop(elseExpr))
              .mapN { (ifs, elseV) =>
                Env.ifElse(ifs, elseV)
              }
              .flatten

          case Always(cond, expr) =>
            (boolExpr(cond).map(Code.always), loop(expr))
              .mapN {
                case (Code.Pass, v) => v
                case (notPass, v) =>
                  Code.WithValue(notPass, v)
              }

          case GetEnumElement(expr, _, idx, sz) =>
            // nonempty enums are just structs with the first element being the variant
            // we could assert the v matches when debugging, but typechecking
            // should assure this
            loop(expr).flatMap { tup =>
              Env.onLast(tup) { t =>
                Code.SelectItem(t, idx + 1)
              }
            }
          case GetStructElement(expr, idx, sz) =>
            val exprR = loop(expr)
            if (sz == 1) {
              // we don't bother to wrap single item structs
              exprR
            }
            else {
              // structs are just tuples
              exprR.flatMap { tup =>
                Env.onLast(tup) { t =>
                  Code.SelectItem(t, idx)
                }
              }
            }
          case PrevNat(expr) =>
            // Nats are just integers
            loop(expr).flatMap { nat =>
              Env.onLast(nat)(Code.Op(_, Code.Const.Minus, Code.Const.One))
            }
          case cons: ConsExpr => makeCons(cons, Nil)
        }
    }
  }
}
