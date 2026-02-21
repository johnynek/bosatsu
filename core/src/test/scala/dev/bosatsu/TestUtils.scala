package dev.bosatsu

import cats.Order
import cats.data.{Chain, Ior, NonEmptyList, Validated, ValidatedNec}
import java.nio.file.{Files, Paths}
import dev.bosatsu.rankn._
import dev.bosatsu.tool.Output
import munit.Assertions.{assertEquals, fail}
import IorMethods.IorExtension

import cats.syntax.all._

object TestUtils {

  final case class TypeValidationError(path: String, message: String) {
    override def toString: String = s"$path: $message"
  }

  private type TypeValidation[+A] = ValidatedNec[TypeValidationError, A]
  private case class LoopContext(argTypes: List[Type], recurType: Type)

  private val typeValidationPass: TypeValidation[Unit] =
    Validated.validNec(())

  private def typeValidationFail(
      path: String,
      message: String
  ): TypeValidation[Unit] =
    Validated.invalidNec(TypeValidationError(path, message))

  private def combineTypeValidation(
      checks: Iterable[TypeValidation[Unit]]
  ): TypeValidation[Unit] =
    checks.foldLeft(typeValidationPass) { (acc, next) =>
      (acc, next).mapN((_, _) => ())
    }

  private def showType(t: Type): String =
    Type.fullyResolvedDocument.document(t).render(80)

  type SourceConvertedProgram = Program[
    (TypeEnv[Kind.Arg], ParsedTypeEnv[Option[Kind.Arg]]),
    Expr[Declaration],
    List[Statement]
  ]

  def sourceConvertedProgramOf(
      pack: PackageName,
      str: String
  ): SourceConvertedProgram = {
    val stmt = statementsOf(str)
    SourceConverter.toProgram(pack, Nil, stmt) match {
      case Ior.Right(prog)   => prog
      case Ior.Both(_, prog) => prog
      case Ior.Left(err)     => fail(s"source conversion failed: $err")
    }
  }

  def sourceConvertedProgramOf(str: String): SourceConvertedProgram =
    sourceConvertedProgramOf(testPackage, str)

  def parsedTypeEnvOf(
      pack: PackageName,
      str: String
  ): ParsedTypeEnv[Option[Kind.Arg]] =

    sourceConvertedProgramOf(pack, str).types._2

  val predefParsedTypeEnv: ParsedTypeEnv[Option[Kind.Arg]] = {
    val p = Package.predefPackage
    val prog = SourceConverter.toProgram(p.name, Nil, p.program) match {
      case Ior.Right(prog)   => prog
      case Ior.Both(_, prog) => prog
      case Ior.Left(err)     => sys.error(err.toString)
    }
    prog.types._2
  }

  def typeEnvOf(pack: PackageName, str: String): TypeEnv[Option[Kind.Arg]] =
    TypeEnv.fromParsed(parsedTypeEnvOf(pack, str))

  def statementsOf(str: String): List[Statement] =
    Parser.unsafeParse(Statement.parser, str)

  /** Make sure no illegal final types escaped into a TypedExpr
    */
  def assertValid[A](te: TypedExpr[A]): Unit = {
    def checkType[T <: Type](t: T, bound: Set[Type.Var.Bound]): T =
      t match {
        case t @ Type.TyVar(Type.Var.Skolem(_, _, _, _)) =>
          sys.error(s"illegal skolem ($t) escape in ${te.repr}")
        case Type.TyVar(Type.Var.Bound(_)) => t
        case t @ Type.TyMeta(_)            =>
          sys.error(s"illegal meta ($t) escape in ${te.repr}")
        case Type.TyApply(left, right) =>
          checkType(left, bound): Unit
          checkType(right, bound): Unit
          t
        case Type.ForAll(vars, in) =>
          checkType(in, bound ++ vars.toList.map(_._1)): Unit
          t
        case Type.Exists(vars, in) =>
          checkType(in, bound ++ vars.toList.map(_._1)): Unit
          t
        case Type.TyConst(_) => t
      }
    te.traverseType[cats.Id](checkType(_, Set.empty)): Unit

    def checkExpr(expr: TypedExpr[A]): Unit =
      expr match {
        case TypedExpr.Generic(_, in) =>
          checkExpr(in)
        case TypedExpr.Annotation(term, tpe, qev) =>
          qev.foreach { ev =>
            Require(
              ev.targetAtSolve.sameAs(tpe),
              s"quantifier evidence invariant violated: targetAtSolve ${ev.targetAtSolve} " +
                s"is not sameAs annotation type $tpe in ${te.repr}"
            )
          }
          checkExpr(term)
        case TypedExpr.AnnotatedLambda(_, body, _) =>
          checkExpr(body)
        case TypedExpr.Local(_, _, _) | TypedExpr.Global(_, _, _, _) |
            TypedExpr.Literal(_, _, _) =>
          ()
        case TypedExpr.App(fn, args, _, _) =>
          checkExpr(fn)
          args.iterator.foreach(checkExpr)
        case TypedExpr.Let(_, expr0, in, _, _) =>
          checkExpr(expr0)
          checkExpr(in)
        case TypedExpr.Loop(args, body, _) =>
          args.iterator.foreach { case (_, init) =>
            checkExpr(init)
          }
          checkExpr(body)
        case TypedExpr.Recur(args, _, _) =>
          args.iterator.foreach(checkExpr)
        case TypedExpr.Match(arg, branches, _) =>
          checkExpr(arg)
          branches.iterator.foreach { branch =>
            branch.guard.foreach(checkExpr)
            checkExpr(branch.expr)
          }
      }

    checkExpr(te)

    val tp = te.getType
    lazy val teStr = Type.fullyResolvedDocument.document(tp).render(80)
    Require(
      Type.freeTyVars(tp :: Nil).isEmpty,
      s"illegal inferred type: $teStr in: ${te.repr}"
    )

    Require(
      Type.metaTvs(tp :: Nil).isEmpty,
      s"illegal inferred type: $teStr in: ${te.repr}"
    )
  }

  private def assertTypeValidationResult(
      stage: String,
      res: TypeValidation[Unit]
  ): Unit =
    res match {
      case Validated.Valid(_) =>
        ()
      case Validated.Invalid(errs) =>
        val msg = errs.toNonEmptyList.toList.map(_.toString).mkString("\n")
        fail(s"type connectivity validation failed at $stage:\n$msg")
    }

  private def validateTypedExprInvariants[A](
      te: TypedExpr[A],
      path: String
  ): TypeValidation[Unit] =
    Either
      .catchNonFatal(assertValid(te))
      .leftMap(err =>
        TypeValidationError(path, Option(err.getMessage).getOrElse(err.toString))
      )
      .toValidatedNec
      .void

  private def globalValuesFromTypeEnv(
      env: TypeEnv[Kind.Arg]
  ): Map[(PackageName, Identifier), Type] =
    env.referencedPackages.iterator
      .flatMap { p =>
        env.localValuesOf(p).iterator.map { case (n, t) =>
          ((p, n: Identifier), t)
        }
      }
      .toMap

  private def letGlobalValues[A](
      pack: PackageName,
      lets: List[(Identifier.Bindable, RecursionKind, TypedExpr[A])]
  ): Map[(PackageName, Identifier), Type] =
    lets.iterator.map { case (n, _, te) =>
      ((pack, n: Identifier), te.getType)
    }.toMap

  private def validateTypeConnections[A](
      te: TypedExpr[A],
      locals: Map[Identifier.Bindable, Type],
      globals: Map[(PackageName, Identifier), Type],
      env: TypeEnv[Kind.Arg],
      loopStack: List[LoopContext],
      path: String
  ): TypeValidation[Unit] =
    te match {
      case TypedExpr.Generic(quant, in) =>
        val expected =
          Type.quantify(quant.forallList, quant.existList, in.getType)
        val quantCheck =
          if (te.getType.sameAs(expected)) typeValidationPass
          else
            typeValidationFail(
              path,
              s"generic quantification type mismatch: expected ${showType(expected)}, got ${showType(te.getType)}"
            )
        combineTypeValidation(
          quantCheck :: validateTypeConnections(
            in,
            locals,
            globals,
            env,
            loopStack,
            s"$path/generic"
          ) :: Nil
        )

      case TypedExpr.Annotation(term, tpe, qev) =>
        val evidenceCheck = qev match {
          case Some(ev) if !ev.targetAtSolve.sameAs(tpe) =>
            typeValidationFail(
              path,
              s"annotation evidence target mismatch: evidence=${showType(ev.targetAtSolve)}, annotation=${showType(tpe)}"
            )
          case _ =>
            typeValidationPass
        }
        combineTypeValidation(
          evidenceCheck :: validateTypeConnections(
            term,
            locals,
            globals,
            env,
            loopStack,
            s"$path/annotation"
          ) :: Nil
        )

      case TypedExpr.AnnotatedLambda(args, body, _) =>
        val argsMap = args.iterator.toMap
        validateTypeConnections(
          body,
          locals ++ argsMap,
          globals,
          env,
          loopStack,
          s"$path/lambda"
        )

      case TypedExpr.Local(name, tpe, _) =>
        locals.get(name) match {
          case Some(_) =>
            typeValidationPass
          case None =>
            typeValidationFail(
              path,
              s"unbound local ${name.sourceCodeRepr} with type ${showType(tpe)}"
            )
        }

      case TypedExpr.Global(pack, name, tpe, _) =>
        globals.get((pack, name)).orElse(env.getValue(pack, name)) match {
          case Some(_) =>
            typeValidationPass
          case None =>
            typeValidationFail(
              path,
              s"unknown global ${pack.asString}::${name.sourceCodeRepr} with type ${showType(tpe)}"
            )
        }

      case TypedExpr.App(fn, args, _, _) =>
        val childChecks = combineTypeValidation(
          validateTypeConnections(
            fn,
            locals,
            globals,
            env,
            loopStack,
            s"$path/app/fn"
          ) :: args.toList.zipWithIndex.map { case (arg, idx) =>
            validateTypeConnections(
              arg,
              locals,
              globals,
              env,
              loopStack,
              s"$path/app/arg[$idx]"
            )
          }
        )

        val appTypeChecks: TypeValidation[Unit] = {
          val (_, _, rho) = Type.splitQuantifiers(fn.getType)
          rho match {
            case Type.Fun(_, _) =>
              typeValidationPass
            case nonFn =>
              typeValidationFail(
                path,
                s"application head is not a function type: ${showType(nonFn)}"
              )
          }
        }

        combineTypeValidation(childChecks :: appTypeChecks :: Nil)

      case TypedExpr.Let(arg, expr, in, rec, _) =>
        val exprLocals =
          if (rec.isRecursive) locals.updated(arg, expr.getType)
          else locals
        val inLocals = locals.updated(arg, expr.getType)

        combineTypeValidation(
          validateTypeConnections(
            expr,
            exprLocals,
            globals,
            env,
            loopStack,
            s"$path/let/$arg/expr"
          ) :: validateTypeConnections(
            in,
            inLocals,
            globals,
            env,
            loopStack,
            s"$path/let/$arg/in"
          ) :: Nil
        )

      case TypedExpr.Loop(args, body, _) =>
        val initChecks = args.toList.zipWithIndex.map { case ((_, init), idx) =>
          validateTypeConnections(
            init,
            locals,
            globals,
            env,
            loopStack,
            s"$path/loop/init[$idx]"
          )
        }
        val argTypes = args.toList.map { case (n, init) => (n, init.getType) }
        val duplicateBinds = {
          val seen = scala.collection.mutable.HashSet.empty[Identifier.Bindable]
          argTypes.collect { case (n, _) if !seen.add(n) => n }
        }
        val duplicateCheck =
          if (duplicateBinds.isEmpty) typeValidationPass
          else
            typeValidationFail(
              path,
              s"loop has duplicate binders: ${duplicateBinds.map(_.sourceCodeRepr).mkString(", ")}"
            )
        val bodyChecks = validateTypeConnections(
          body,
          locals ++ argTypes,
          globals,
          env,
          LoopContext(argTypes.map(_._2), te.getType) :: loopStack,
          s"$path/loop/body"
        )

        combineTypeValidation(duplicateCheck :: bodyChecks :: initChecks)

      case TypedExpr.Recur(args, tpe, _) =>
        val argChecks = args.toList.zipWithIndex.map { case (arg, idx) =>
          validateTypeConnections(
            arg,
            locals,
            globals,
            env,
            loopStack,
            s"$path/recur/arg[$idx]"
          )
        }
        val recurChecks =
          loopStack match {
            case Nil =>
              typeValidationFail(path, "recur found outside loop")
            case LoopContext(loopArgTypes, loopType) :: _ =>
              val arityCheck =
                if (loopArgTypes.length == args.length) typeValidationPass
                else
                  typeValidationFail(
                    path,
                    s"recur arity mismatch: loop expects ${loopArgTypes.length} args, got ${args.length}"
                  )
              val argTypeChecks =
                loopArgTypes
                  .zip(args.toList)
                  .zipWithIndex
                  .map { case ((expected, got), idx) =>
                    if (got.getType.sameAs(expected)) typeValidationPass
                    else
                      typeValidationFail(
                        s"$path/recur/arg[$idx]",
                        s"recur arg type mismatch: expected ${showType(expected)}, got ${showType(got.getType)}"
                      )
                  }
              val recurTypeCheck =
                if (tpe.sameAs(loopType)) typeValidationPass
                else
                  typeValidationFail(
                    path,
                    s"recur result type mismatch: expected ${showType(loopType)}, got ${showType(tpe)}"
                  )
              combineTypeValidation(arityCheck :: recurTypeCheck :: argTypeChecks)
          }

        combineTypeValidation(recurChecks :: argChecks)

      case TypedExpr.Literal(lit, tpe, _) =>
        val expected = Type.getTypeOf(lit)
        if (tpe.sameAs(expected)) typeValidationPass
        else
          typeValidationFail(
            path,
            s"literal type mismatch: expected ${showType(expected)}, got ${showType(tpe)}"
          )

      case TypedExpr.Match(arg, branches, _) =>
        val argCheck = validateTypeConnections(
          arg,
          locals,
          globals,
          env,
          loopStack,
          s"$path/match/arg"
        )

        val branchChecks = branches.toList.zipWithIndex.map { case (branch, idx) =>
          val branchPath = s"$path/match/branch[$idx]"
          val branchLocals: Map[Identifier.Bindable, Type] =
            branch.pattern.names.iterator.map(_ -> Type.UnitType).toMap

          val branchResultTypeCheck =
            if (branch.expr.getType.sameAs(te.getType)) typeValidationPass
            else
              typeValidationFail(
                branchPath,
                s"branch result type mismatch: expected ${showType(te.getType)}, got ${showType(branch.expr.getType)}"
              )

          val scope = locals ++ branchLocals
          val guardCheck =
            branch.guard match {
              case None =>
                typeValidationPass
              case Some(guard) =>
                val guardTypeCheck =
                  if (guard.getType.sameAs(Type.BoolType)) typeValidationPass
                  else
                    typeValidationFail(
                      s"$branchPath/guard",
                      s"guard must be Bool, got ${showType(guard.getType)}"
                    )
                combineTypeValidation(
                  guardTypeCheck :: validateTypeConnections(
                    guard,
                    scope,
                    globals,
                    env,
                    loopStack,
                    s"$branchPath/guard"
                  ) :: Nil
                )
            }

          val branchExprCheck = validateTypeConnections(
            branch.expr,
            scope,
            globals,
            env,
            loopStack,
            s"$branchPath/body"
          )
          val branchScopeChecks =
            combineTypeValidation(guardCheck :: branchExprCheck :: Nil)

          combineTypeValidation(branchResultTypeCheck :: branchScopeChecks :: Nil)
        }

        combineTypeValidation(argCheck :: branchChecks)
    }

  private def validateLetList[A](
      pack: PackageName,
      lets: List[(Identifier.Bindable, RecursionKind, TypedExpr[A])],
      fullTypeEnv: TypeEnv[Kind.Arg],
      globalValues: Map[(PackageName, Identifier), Type],
      stage: String
  ): TypeValidation[Unit] = {
    val localNames = lets.iterator.map { case (n, _, te) => (n, te.getType) }.toMap

    combineTypeValidation(lets.map { case (name, _, te) =>
      val path = s"$stage/${pack.asString}/${name.sourceCodeRepr}"
      combineTypeValidation(
        validateTypedExprInvariants(te, path) :: validateTypeConnections(
          te,
          localNames,
          globalValues,
          fullTypeEnv,
          Nil,
          path
        ) :: Nil
      )
    })
  }

  private def allTypeEnvOfPackageMap[A](
      pm: PackageMap.Typed[A]
  ): TypeEnv[Kind.Arg] =
    pm.toMap.valuesIterator.foldLeft(TypeEnv.empty: TypeEnv[Kind.Arg]) {
      (acc, pack) =>
        acc ++ pack.types
    }

  private def lowerPackageMap[A](
      pm: PackageMap.Typed[A]
  ): PackageMap.Typed[A] =
    PackageMap.fromIterable(
      pm.toMap.valuesIterator.map { pack =>
        val (prog, impMap) = pack.program
        val loweredLets = TypedExprLoopRecurLowering.lowerAll(prog.lets)
        pack.copy(program = (prog.copy(lets = loweredLets), impMap))
      }.toList
    )

  private def normalizePackageMap[A: cats.Eq](
      pm: PackageMap.Typed[A]
  ): PackageMap.Typed[A] = {
    val fullTypeEnv = allTypeEnvOfPackageMap(pm)
    PackageMap.fromIterable(
      pm.toMap.valuesIterator.map { pack =>
        val (prog, impMap) = pack.program
        val normalizedLets =
          TypedExprNormalization.normalizeAll(pack.name, prog.lets, fullTypeEnv)
        pack.copy(program = (prog.copy(lets = normalizedLets), impMap))
      }.toList
    )
  }

  def validateTypes[A](
      te: TypedExpr[A],
      names: Map[TypedExpr.Name[A], Type],
      env: TypeEnv[Kind.Arg]
  ): ValidatedNec[TypeValidationError, Unit] = {
    val localNames =
      names.iterator.collect { case (TypedExpr.Local(n, _, _), t) =>
        (n, t)
      }.toMap
    val globalNames =
      names.iterator.collect { case (TypedExpr.Global(p, n, _, _), t) =>
        ((p, n), t)
      }.toMap

    combineTypeValidation(
      validateTypedExprInvariants(te, "typed-expr") :: validateTypeConnections(
        te,
        localNames,
        globalNames,
        env,
        Nil,
        "typed-expr"
      ) :: Nil
    )
  }

  def validatePackageMap[A](
      pm: PackageMap.Typed[A],
      stage: String = "package-map"
  ): ValidatedNec[TypeValidationError, Unit] = {
    val fullTypeEnv = allTypeEnvOfPackageMap(pm)
    val globalValues =
      globalValuesFromTypeEnv(fullTypeEnv) ++ pm.toMap.valuesIterator.flatMap {
        pack =>
          letGlobalValues(pack.name, pack.lets)
      }.toMap

    combineTypeValidation(pm.toMap.valuesIterator.map { pack =>
      validateLetList(pack.name, pack.lets, fullTypeEnv, globalValues, stage)
    }.toList)
  }

  def assertPackageMapTypeConnections[A](
      pm: PackageMap.Typed[A],
      stage: String
  ): Unit =
    assertTypeValidationResult(stage, validatePackageMap(pm, stage))

  def assertRewritePipelineTypeConnections[A: cats.Eq](
      unoptimized: PackageMap.Typed[A]
  ): Unit = {
    assertPackageMapTypeConnections(unoptimized, "unoptimized")
    val lowered = lowerPackageMap(unoptimized)
    assertPackageMapTypeConnections(lowered, "loop/recur lowered")
    val normalized = normalizePackageMap(lowered)
    assertPackageMapTypeConnections(normalized, "typed expr normalization")
  }

  private def normalizeWithRewriteValidation(
      pack: PackageName,
      fullTypeEnv: TypeEnv[Kind.Arg],
      unoptimized: Program[
        TypeEnv[Kind.Arg],
        TypedExpr[Declaration],
        List[Statement]
      ]
  ): Program[TypeEnv[Kind.Arg], TypedExpr[Declaration], List[Statement]] = {
    val unoptGlobals =
      globalValuesFromTypeEnv(fullTypeEnv) ++ letGlobalValues(pack, unoptimized.lets)
    assertTypeValidationResult(
      "unoptimized",
      validateLetList(pack, unoptimized.lets, fullTypeEnv, unoptGlobals, "unoptimized")
    )

    val loweredLets = TypedExprLoopRecurLowering.lowerAll(unoptimized.lets)
    val loweredGlobals =
      globalValuesFromTypeEnv(fullTypeEnv) ++ letGlobalValues(pack, loweredLets)
    assertTypeValidationResult(
      "loop/recur lowered",
      validateLetList(
        pack,
        loweredLets,
        fullTypeEnv,
        loweredGlobals,
        "loop/recur lowered"
      )
    )

    val normalizedLets =
      TypedExprNormalization.normalizeAll(pack, loweredLets, fullTypeEnv)
    val normalizedGlobals =
      globalValuesFromTypeEnv(fullTypeEnv) ++ letGlobalValues(pack, normalizedLets)
    assertTypeValidationResult(
      "typed expr normalization",
      validateLetList(
        pack,
        normalizedLets,
        fullTypeEnv,
        normalizedGlobals,
        "typed expr normalization"
      )
    )

    unoptimized.copy(lets = normalizedLets)
  }

  val testPackage: PackageName = PackageName.parts("Test")

  def checkLast[A](
      statement: String
  )(fn: TypedExpr[Declaration] => A): A = {
    val stmts = Parser.unsafeParse(Statement.parser, statement)
    Package.inferBodyUnopt(testPackage, Nil, stmts).strictToValidated match {
      case Validated.Invalid(errs) =>
        val lm = LocationMap(statement)
        val packMap = Map((testPackage, (lm, statement)))
        val msg = errs.toList
          .map { err =>
            err.message(packMap, LocationMap.Colorize.None)
          }
          .mkString("", "\n==========\n", "\n")
        sys.error("inference failure: " + msg)
      case Validated.Valid((fullTypeEnv, program)) =>
        val normalized =
          normalizeWithRewriteValidation(testPackage, fullTypeEnv, program)
        normalized.lets.foreach { case (_, _, te) => assertValid(te) }
        fn(normalized.lets.last._3)
    }
  }

  def checkPackageMap[A](
      statement: String
  )(
      fn: PackageMap.Typed[Declaration] => A
  ): A = {
    val stmts = Parser.unsafeParse(Statement.parser, statement)
    Package.inferBodyUnopt(testPackage, Nil, stmts).strictToValidated match {
      case Validated.Invalid(errs) =>
        val lm = LocationMap(statement)
        val packMap = Map((testPackage, (lm, statement)))
        val msg = errs.toList
          .map { err =>
            err.message(packMap, LocationMap.Colorize.None)
          }
          .mkString("", "\n==========\n", "\n")
        sys.error("inference failure: " + msg)
      case Validated.Valid((fullTypeEnv, program)) =>
        val normalized =
          normalizeWithRewriteValidation(testPackage, fullTypeEnv, program)
        normalized.lets.foreach { case (_, _, te) => assertValid(te) }
        val pack: Package.Typed[Declaration] =
          Package(testPackage, Nil, Nil, (normalized, ImportMap.empty))
        val pm: PackageMap.Typed[Declaration] =
          PackageMap.empty + pack + PackageMap.predefCompiled
        assertPackageMapTypeConnections(pm, "optimized")
        fn(pm)
    }
  }

  def checkMatchless[A](
      statement: String
  )(
      fn: Map[
        PackageName,
        List[(Identifier.Bindable, Matchless.Expr[Unit])]
      ] => A
  ): A =
    checkPackageMap(statement) { pm =>
      Par.withEC {
        given Order[Unit] = Order.fromOrdering
        val comp = MatchlessFromTypedExpr.compile((), pm)
        fn(comp)
      }
    }

  def compileFile(path: String, rest: String*)(implicit
      ec: Par.EC
  ): PackageMap.Typed[Any] = {
    def toS(s: String): String =
      new String(Files.readAllBytes(Paths.get(s)), "UTF-8")

    val packNEL =
      NonEmptyList(path, rest.toList)
        .map { s =>
          val str = toS(s)
          val pack = Parser.unsafeParse(Package.parser(None), str)
          (("", LocationMap(str)), pack)
        }

    val res =
      PackageMap.typeCheckParsed(packNEL, Nil, "", CompileOptions.Default)
    res.left match {
      case Some(err) => sys.error(err.toString)
      case None      => ()
    }

    val pm = res.right.get
    assertPackageMapTypeConnections(pm, "optimized")
    pm
  }

  def makeInputArgs(files: List[(Chain[String], Any)]): List[String] =
    ("--package_root" :: "" :: Nil) ::: files.flatMap { case (idx, _) =>
      "--input" :: idx.iterator.mkString("/") :: Nil
    }

  private type ErrorOr[A] = Either[Throwable, A]
  private val module = MemoryMain[ErrorOr]

  def evalTest(packages: List[String], mainPackS: String, expected: Value) = {
    val files = packages.zipWithIndex.map(_.swap).map { case (idx, content) =>
      Chain.one(s"Package$idx") -> content
    }

    module.runWith(files)(
      "tool" :: "eval" :: "--main" :: mainPackS :: makeInputArgs(files)
    ) match {
      case Right(Output.EvaluationResult(got, _, gotDoc)) =>
        val gv = got.value
        assertEquals(
          gv,
          expected,
          s"${gotDoc.value.render(80)}\n\n$gv != $expected"
        )
      case Right(other) =>
        fail(s"got an unexpected success: $other")
      case Left(err) =>
        module.mainExceptionToString(err) match {
          case Some(msg) => fail(msg)
          case None      => fail(s"got an exception: $err")
        }
    }
  }

  def evalTestJson(
      packages: List[String],
      mainPackS: String,
      expected: Json
  ) = {
    val files = packages.zipWithIndex.map(_.swap).map { case (idx, content) =>
      Chain.one(s"Package$idx") -> content
    }

    module.runWith(files)(
      "tool" :: "json" :: "write" :: "--main" :: mainPackS :: "--output" :: "-1" :: makeInputArgs(
        files
      )
    ) match {
      case Right(Output.JsonOutput(got, _)) =>
        assertEquals(got, expected, s"$got != $expected")
      case Right(other) =>
        fail(s"got an unexpected success: $other")
      case Left(err) =>
        fail(s"got an exception: $err")
    }
  }

  def runBosatsuTest(
      packages: List[String],
      mainPackS: String,
      assertionCount: Int
  ) = {
    val files = packages.zipWithIndex.map(_.swap).map { case (idx, content) =>
      Chain.one(s"Package$idx") -> content
    }

    module.runWith(files)(
      "tool" :: "test" :: "--test_package" :: mainPackS :: makeInputArgs(
        files
      )
    ) match {
      case Right(Output.TestOutput(results, _)) =>
        results.collect { case (_, Some(t)) => t.value } match {
          case t :: Nil =>
            assertEquals(
              t.assertions,
              assertionCount,
              s"${t.assertions} != $assertionCount"
            )
            val Test.Report(_, failcount, message) =
              Test.report(t, LocationMap.Colorize.None)
            assertEquals(t.failures.map(_.assertions).getOrElse(0), failcount)
            if (failcount > 0) fail(message.render(80))
            else ()
          case other =>
            fail(s"expected exactly one test result, got: $other")
        }
      case Right(other) =>
        fail(s"got an unexpected success: $other")
      case Left(err) =>
        module.mainExceptionToString(err) match {
          case Some(err) =>
            fail(err)
          case None =>
            err.printStackTrace
            fail(err.toString)
        }
    }
  }

  def testInferred(
      packages: List[String],
      mainPackS: String,
      inferredHandler: (PackageMap.Inferred, PackageName) => Unit
  )(implicit ec: Par.EC) = {
    val mainPack = PackageName.parse(mainPackS).get

    val parsed = packages.zipWithIndex.traverse { case (pack, i) =>
      Parser.parse(Package.parser(None), pack).map { case (lm, parsed) =>
        ((i.toString, lm), parsed)
      }
    }

    val parsedPaths = parsed match {
      case Validated.Valid(vs)     => vs
      case Validated.Invalid(errs) =>
        errs.toList.foreach { p =>
          System.err.println(
            p.showContext(LocationMap.Colorize.None).render(80)
          )
        }
        sys.error("failed to parse") // errs.toString)
    }

    val fullParsed =
      PackageMap
        .withPredefA(("predef", LocationMap("")), parsedPaths)
        .map { case ((path, _), p) => (path, p) }

    PackageMap
      .resolveThenInfer(fullParsed, Nil, CompileOptions.NoOptimize)
      .strictToValidated match {
      case Validated.Valid(unoptimizedPackMap) =>
        assertRewritePipelineTypeConnections(unoptimizedPackMap)
      case Validated.Invalid(errs) =>
        val tes = errs.toList
          .collect { case te: PackageError.TypeErrorIn =>
            te.tpeErr.toString
          }
          .mkString("\n")
        fail(tes + "\n" + errs.toString)
    }

    PackageMap
      .resolveThenInfer(fullParsed, Nil, CompileOptions.Default)
      .strictToValidated match {
      case Validated.Valid(packMap) =>
        assertPackageMapTypeConnections(packMap, "optimized")
        inferredHandler(packMap, mainPack)

      case Validated.Invalid(errs) =>
        val tes = errs.toList
          .collect { case te: PackageError.TypeErrorIn =>
            te.tpeErr.toString
          }
          .mkString("\n")
        fail(tes + "\n" + errs.toString)
    }
  }

  def evalFail(
      packages: List[String]
  )(errFn: PartialFunction[PackageError, Unit])(implicit ec: Par.EC) = {

    val parsed = packages.zipWithIndex.traverse { case (pack, i) =>
      Parser.parse(Package.parser(None), pack).map { case (lm, parsed) =>
        ((i.toString, lm), parsed)
      }
    }

    val parsedPaths = parsed match {
      case Validated.Valid(vs)     => vs
      case Validated.Invalid(errs) =>
        sys.error(s"parse fail: ${errs}")
    }

    // use parallelism to typecheck
    val withPre =
      PackageMap.withPredefA(("predef", LocationMap("")), parsedPaths)

    val withPrePaths = withPre.map { case ((path, _), p) => (path, p) }
    PackageMap
      .resolveThenInfer(withPrePaths, Nil, CompileOptions.Default)
      .left match {
      case None =>
        fail("expected to fail type checking")

      case Some(errs) if errs.collect(errFn).nonEmpty =>
        // make sure we can print the messages:
        val sm = PackageMap.buildSourceMap(withPre)
        errs.toList.foreach(_.message(sm, LocationMap.Colorize.None))
        assert(true)
      case Some(errs) =>
        fail(s"failed, but no type errors: $errs")
    }
  }

}
