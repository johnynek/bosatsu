package org.bykn.bosatsu

import alleycats.std.map._ // TODO use SortedMap everywhere
import com.stripe.dagon.Memoize
import cats.data.{NonEmptyList, Validated, ValidatedNel, ReaderT}
import cats.{Foldable, Order}
import cats.implicits._
import fastparse.all._
import org.typelevel.paiges.{Doc, Document}
import Parser.{lowerIdent, upperIdent, spaces, maybeSpace, Combinators}

import rankn.Infer

/**
 * Represents a package over its life-cycle: from parsed to resolved to inferred
 */
case class Package[A, B, C, D](
  name: PackageName,
  imports: List[Import[A, B]],
  exports: List[ExportedName[C]],
  program: D) {

  // TODO, this isn't great
  private lazy val importMap: ImportMap[A, B] =
    ImportMap.fromImports(imports)._2

  def localImport(n: String): Option[(A, ImportedName[B])] = importMap(n)

  def withImport(i: Import[A, B]): Package[A, B, C, D] =
    copy(imports = i :: imports)

  def mapProgram[D1](fn: D => D1): Package[A, B, C, D1] =
    Package(name, imports, exports, fn(program))
}

object Package {
  type FixPackage[B, C, D] = Fix[Lambda[a => Package[a, B, C, D]]]
  type PackageF[A, B] = Package[FixPackage[A, A, B], A, A, B]
  type Parsed = Package[PackageName, Unit, Unit, Statement]
  type Inferred = FixPackage[Referant, Referant, Program[Expr[(Declaration, Scheme)], Statement]]

  /**
   * build a Parsed Package from a Statement. This is useful for testing or
   * library usages.
   */
  def fromStatement(pn: PackageName, st: Statement): Package.Parsed =
    Package(pn, Nil, Nil, st)

  /** add a Fix wrapper
   *  it is combersome to write the correct type here
   */
  def asInferred(p: PackageF[Referant, Program[Expr[(Declaration, Scheme)], Statement]]): Inferred =
    Fix[Lambda[a =>
      Package[a, Referant, Referant, Program[Expr[(Declaration, Scheme)], Statement]]]](p)

  implicit val document: Document[Package[PackageName, Unit, Unit, Statement]] =
    Document.instance[Package.Parsed] { case Package(name, imports, exports, program) =>
      val p = Doc.text("package ") + Document[PackageName].document(name) + Doc.line
      val i = imports match {
        case Nil => Doc.empty
        case nonEmptyImports =>
          Doc.intercalate(Doc.line, nonEmptyImports.map(Document[Import[PackageName, Unit]].document _)) + Doc.line
      }
      val e = exports match {
        case Nil => Doc.empty
        case nonEmptyExports =>
          Doc.text("export ") + Doc.text("[ ") +
          Doc.intercalate(Doc.text(", "), nonEmptyExports.map(Document[ExportedName[Unit]].document _)) + Doc.text(" ]") + Doc.line
      }
      val b = Document[Statement].document(program)
      // add an extra line between each group
      Doc.intercalate(Doc.line, List(p, i, e, b))
    }

  val parser: P[Package[PackageName, Unit, Unit, Statement]] = {
    // TODO: support comments before the Statement
    val pname = Padding.parser(P("package" ~ spaces ~ PackageName.parser)).map(_.padded)
    val im = Padding.parser(Import.parser).map(_.padded).rep().map(_.toList)
    val ex = Padding.parser(P("export" ~ maybeSpace ~ ExportedName.parser.nonEmptyListSyntax)).map(_.padded)
    val body = Padding.parser(Statement.parser).map(_.padded)
    (pname ~ im ~ Parser.nonEmptyListToList(ex) ~ body).map { case (p, i, e, b) =>
      Package(p, i, e, b)
    }
  }
}

case class PackageMap[A, B, C, D](toMap: Map[PackageName, Package[A, B, C, D]]) {
  def +(pack: Package[A, B, C, D]): PackageMap[A, B, C, D] =
    PackageMap(toMap + (pack.name -> pack))

  def ++(packs: Iterable[Package[A, B, C, D]]): PackageMap[A, B, C, D] =
    packs.foldLeft(this)(_ + _)
}

object PackageMap {
  def empty[A, B, C, D]: PackageMap[A, B, C, D] =
    PackageMap(Map.empty)

  /**
   * Either return a unique mapping from B to A, or return a pair of duplicates
   * and the unque subset
   */
  def uniqueByKey[A, B: Order](as: List[A])(fn: A => B): Either[(Map[B, (A, NonEmptyList[A])], Map[B, A]), Map[B, A]] =
    as match {
      case Nil => Right(Map.empty)
      case h :: tail =>
        val nea = NonEmptyList(h, tail)
        val m: Map[B, NonEmptyList[A]] = nea.groupBy(fn)
        def check(as: NonEmptyList[A]): Either[(A, NonEmptyList[A]), A] =
          as match {
            case NonEmptyList(a, Nil) =>
              Right(a)
            case NonEmptyList(a0, a1 :: tail) =>
              Left((a0, NonEmptyList(a1, tail)))
          }

        val checked = m.map { case (b, as) => (b, check(as)) }
        val good = checked.collect { case (b, Right(a)) => (b, a) }.toMap
        val bad = checked.collect { case (b, Left(a)) => (b, a) }.toMap

        if (bad.isEmpty) Right(good)
        else Left((bad, good))
    }

  import Package.{ FixPackage, PackageF }

  type MapF3[A, B, C] = PackageMap[FixPackage[A, B, C], A, B, C]
  type MapF2[A, B] = MapF3[A, A, B]
  type ParsedImp = PackageMap[PackageName, Unit, Unit, Program[Expr[Declaration], Statement]]
  type Resolved = MapF2[Unit, Program[Expr[Declaration], Statement]]
  type Inferred = MapF2[Referant, Program[Expr[(Declaration, Scheme)], Statement]]

  /**
   * This builds a DAG of actual packages where names have been replaced by the fully resolved
   * packages
   */
  def resolvePackages[A, B, C](map: PackageMap[PackageName, A, B, C]): ValidatedNel[PackageError, MapF3[A, B, C]] = {
    def getPackage(i: Import[PackageName, A], from: Package[PackageName, A, B, C]): ValidatedNel[PackageError, Import[Package[PackageName, A, B, C], A]] =
      map.toMap.get(i.pack) match {
        case None => Validated.invalidNel(PackageError.UnknownImportPackage(i.pack, from))
        case Some(pack) => Validated.valid(Import(pack, i.items))
      }

    def step(p: Package[PackageName, A, B, C]): ReaderT[Either[NonEmptyList[PackageError], ?], List[PackageName], Package[FixPackage[A, B, C], A, B, C]] = {
      val edeps = ReaderT.ask[Either[NonEmptyList[PackageError], ?], List[PackageName]]
        .flatMapF {
          case nonE@(h :: tail) if nonE.contains(p.name) =>
            Left(NonEmptyList.of(PackageError.CircularDependency(p, NonEmptyList(h, tail))))
          case _ =>
            val deps = p.imports.traverse(getPackage(_, p)) // the packages p depends on
            deps.toEither
        }

      edeps
        .flatMap { deps =>
          deps.traverse { i =>
            step(i.pack)
              .local[List[PackageName]](p.name :: _) // add this package into the path of all the deps
              .map { p => Import(Fix[Lambda[a => Package[a, A, B, C]]](p), i.items) }
          }
          .map { imports =>
            Package(p.name, imports, p.exports, p.program)
          }
        }
    }

    type M = Map[PackageName, Package[FixPackage[A, B, C], A, B, C]]
    val r: ReaderT[Either[NonEmptyList[PackageError], ?], List[PackageName], M] =
      map.toMap.traverse(step)
    val m: Either[NonEmptyList[PackageError], M] = r.run(Nil)
    m.map(PackageMap(_)).toValidated
  }

  /**
   * Convenience method to create a PackageMap then resolve it
   */
  def resolveAll[A](ps: List[(A, Package.Parsed)]): (Map[PackageName, ((A, Package.Parsed), NonEmptyList[(A, Package.Parsed)])], ValidatedNel[PackageError, Resolved]) = {

    type AP = (A, Package.Parsed)
    val (nonUnique, unique): (Map[PackageName, (AP, NonEmptyList[AP])], Map[PackageName, AP]) =
      uniqueByKey(ps)(_._2.name) match {
        case Right(unique) =>
          (Map.empty, unique)
        case Left(res) => res
      }

    def toProg(p: Package.Parsed):
      (Option[PackageError],
        Package[PackageName, Unit, Unit, Program[Expr[Declaration], Statement]]) = {

      val (errs0, imap) = ImportMap.fromImports(p.imports)
      val errs = errs0 match {
        case Nil => None
        case h :: tail =>
          Some(PackageError.DuplicatedImport(NonEmptyList(h, tail)))
      }
      (errs, p.mapProgram { s => Program.fromStatement(p.name, imap, s) })
    }

    // we know all the package names are unique here
    def foldMap(m: Map[PackageName, (A, Package.Parsed)]): (List[PackageError], PackageMap.ParsedImp) = {
      val initPm = PackageMap
        .empty[PackageName, Unit, Unit, Program[Expr[Declaration], Statement]]

      m.iterator.foldLeft((List.empty[PackageError], initPm)) {
        case ((errs, pm), (_, (_, pack))) =>
          val (lerrs, pp) = toProg(pack)
          (lerrs.toList ::: errs, pm + pp)
      }
    }

    val (errs, pmap) = foldMap(unique)
    val res = resolvePackages(pmap)
    // combine the import errors now:
    val check =
      errs match {
        case Nil =>
          Validated.valid(())
        case h :: tail =>
          Validated.invalid(NonEmptyList(h, tail))
      }
    // keep all the errors
    (nonUnique, check *> res)
  }

  /**
   * Infer all the types in a resolved PackageMap
   */
  def inferAll(ps: Resolved): ValidatedNel[PackageError, Inferred] = {

      type PackIn = PackageF[Unit, Program[Expr[Declaration], Statement]]
      type PackOut = PackageF[Referant, Program[Expr[(Declaration, Scheme)], Statement]]

      val infer: PackIn => ValidatedNel[PackageError, PackOut] =
        Memoize.function[PackIn, ValidatedNel[PackageError, PackOut]] {
          case (p@Package(nm, imports, exports, prog), recurse) =>

            def getImport(packF: PackOut,
              exMap: Map[String, NonEmptyList[ExportedName[Referant]]],
              i: ImportedName[Unit]): ValidatedNel[PackageError, NonEmptyList[ImportedName[Referant]]] = {

              def err =
                Validated.invalidNel(PackageError.UnknownImportName(p, packF, i, exMap.iterator.flatMap(_._2.toList).toList))

              i match {
                case ImportedName.OriginalName(nm, _) =>
                  exMap.get(nm) match {
                    case Some(refs) => Validated.valid(refs.map { ref => ImportedName.OriginalName(nm, ref.tag) })
                    case None => err
                  }
                case ImportedName.Renamed(orig, nm, _) =>
                  exMap.get(orig) match {
                    case Some(refs) => Validated.valid(refs.map { ref => ImportedName.Renamed(orig, nm, ref.tag) })
                    case None => err
                  }
              }
            }

            def stepImport(i: Import[FixPackage[Unit, Unit, Program[Expr[Declaration], Statement]], Unit]):
              ValidatedNel[PackageError, Import[FixPackage[Referant, Referant, Program[Expr[(Declaration, Scheme)], Statement]], Referant]] = {
              val Import(fixpack, items) = i
              recurse(fixpack.unfix).andThen { packF =>
                val exMap = ExportedName.buildExportMap(packF.exports)
                items.traverse(getImport(packF, exMap, _))
                  .map { imps =>
                    Import(Fix[Lambda[a => Package[a, Referant, Referant, Program[Expr[(Declaration, Scheme)], Statement]]]](packF), imps.flatten)
                  }
              }
            }

            def inferExports(imps: List[Import[FixPackage[Referant, Referant, Program[Expr[(Declaration, Scheme)], Statement]], Referant]]):
              ValidatedNel[PackageError, (List[ExportedName[Referant]], TypeEnv, List[(String, Expr[(Declaration, Scheme)])])] = {
              implicit val subD: Substitutable[Declaration] = Substitutable.opaqueSubstitutable[Declaration]
              implicit val subS: Substitutable[String] = Substitutable.opaqueSubstitutable[String]
              implicit val subExpr: Substitutable[Expr[(Declaration, Scheme)]] =
                Substitutable.fromMapFold[Expr, (Declaration, Scheme)]

              val foldNest = Foldable[List].compose(Foldable[NonEmptyList])
              val Program(te, lets, _) = prog

              // Add all the imports to the type environment
              val updatedTE = foldNest.foldLeft(imps.map(_.items), te)(_.addRef(_))

              val ilets = Inference.inferLets(lets)

              Inference.runInfer(updatedTE, ilets) match {
                case Left(typeerr) => Validated.invalidNel(PackageError.TypeErrorIn(typeerr, p)) // TODO give better errors
                case Right(lets) =>
                  val letMap = lets.toMap
                  def expName(n: ExportedName[Unit]): ValidatedNel[PackageError, NonEmptyList[ExportedName[Referant]]] = {
                    def err = Validated.invalidNel(PackageError.UnknownExport(n, p))
                    n match {
                      case ExportedName.Binding(n, _) =>
                        letMap.get(n) match {
                          case Some(exprDeclScheme) =>
                            Validated.valid(NonEmptyList(ExportedName.Binding(n, Referant.Value(exprDeclScheme.tag._2.normalized)), Nil))
                          case None =>
                            // It could be an external or imported value in the TypeEnv
                            updatedTE.values.get(n) match {
                              case Some(scheme) =>
                                Validated.valid(NonEmptyList(ExportedName.Binding(n, Referant.Value(scheme.normalized)), Nil))
                              case None => err
                            }
                        }
                      case ExportedName.TypeName(n, _) =>
                        // export the opaque type
                        updatedTE.definedTypes.get((nm, TypeName(n))) match {
                          case Some(dt) =>
                            Validated.valid(NonEmptyList(ExportedName.TypeName(n, Referant.DefinedT(dt.toOpaque)), Nil))
                          case None => err
                        }
                      case ExportedName.Constructor(n, _) =>
                        // export the type and all constructors
                        updatedTE.definedTypes.get((nm, TypeName(n))) match {
                          case Some(dt) =>
                            val cons = dt.constructors.map { case (n, _) =>
                              ExportedName.Constructor(n.asString, Referant.Constructor(n, dt))
                            }
                            val t = ExportedName.TypeName(n, Referant.DefinedT(dt))
                            Validated.valid(NonEmptyList(t, cons))
                          case None => err
                        }
                    }
                  }
                  exports.traverse(expName)
                    .map(_.flatMap(_.toList))
                    .map((_, updatedTE, lets))
              }
            }

            imports
              .traverse(stepImport(_))
              .andThen { imps =>
                inferExports(imps).map((imps, _))
              }
              .map { case (imps, (exps, types, lets)) =>
                val Program(_, _, statement) = prog
                Package(nm, imps, exps, Program(types, lets, statement))
              }
        }

      ps.toMap.traverse(infer).map(PackageMap(_))
    }

  def resolveThenInfer[A](
    ps: List[(A, Package.Parsed)]): (Map[PackageName, ((A, Package.Parsed), NonEmptyList[(A, Package.Parsed)])], ValidatedNel[PackageError, Inferred]) = {
      val (bad, good) = resolveAll(ps)
      (bad, good.andThen(inferAll(_)))
    }
}

sealed abstract class PackageError {
  def message(sourceMap: Map[PackageName, (LocationMap, String)]): String
}

object PackageError {
  case class UnknownExport(ex: ExportedName[Unit],
    in: Package.PackageF[Unit, Program[Expr[Declaration], Statement]]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)]) = {
      val (lm, sourceName) = sourceMap(in.name)
      val header =
        s"in $sourceName unknown export ${ex.name}"
      val candidates = in.program.lets
        .map { case (n, expr) => (EditDistance.string(n, ex.name), n, HasRegion.region(expr)) }
        .sorted
        .take(3)
        .map { case (_, n, r) =>
          val pos = lm.toLineCol(r.start).map { case (l, c) => s" at line: ${l + 1}, column: ${c + 1}" }.getOrElse("")
          s"$n$pos"
        }
      val candstr = candidates.mkString("\n\t", "\n\t", "\n")
      val suggestion = s"perhaps you meant:$candstr"
      header + "\n" + suggestion
    }
  }

  case class UnknownImportPackage[A, B, C](pack: PackageName, from: Package[PackageName, A, B, C]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)]) = {
      val (_, sourceName) = sourceMap(pack)
      s"in $sourceName package ${pack.asString} imports unknown package ${from.name.asString}"
    }
  }

  case class DuplicatedImport(duplicates: NonEmptyList[(PackageName, ImportedName[Unit])]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)]) =
      duplicates.sortBy(_._2.localName)
        .toList
        .iterator
        .map { case (pack, imp) =>
          val (_, sourceName) = sourceMap(pack)
          s"duplicate import in $sourceName package ${pack.asString} imports ${imp.originalName} as ${imp.localName}"
        }
        .mkString("\n")
  }

  // We could check if we forgot to export the name in the package and give that error
  case class UnknownImportName(in: Package.PackageF[Unit, Program[Expr[Declaration], Statement]],
    importing: Package.PackageF[Referant, Program[Expr[(Declaration, Scheme)], Statement]],
    iname: ImportedName[Unit],
    exports: List[ExportedName[Referant]]) extends PackageError {
      def message(sourceMap: Map[PackageName, (LocationMap, String)]) = {
        val ls = importing
          .program
          .lets

        val (_, sourceName) = sourceMap(in.name)
        ls
          .toMap
          .get(iname.originalName) match {
            case Some(_) =>
              s"in $sourceName package: ${importing.name} has ${iname.originalName} but it is not exported. Add to exports"
            case None =>
              val dist = Memoize.function[String, Int] { (s, _) => EditDistance(iname.originalName.toIterable, s.toIterable) }
              val nearest = ls.map { case (n, _) => (dist(n), n) }.sorted.take(3).map { case (_, n) => n }.mkString(", ")
              s"in $sourceName package: ${importing.name} does not have name ${iname.originalName}. Nearest: $nearest"
          }
      }
    }
  case class CircularDependency[A, B, C](from: Package[PackageName, A, B, C], path: NonEmptyList[PackageName]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)]) = {
      val packs = from.name :: (path.toList)
      val msg = packs.map { p =>
        val (_, src) = sourceMap(p)
        s"${p.asString} in $src"
      }
      val tab = "\n\t"
      s"circular package dependency:\n${msg.mkString(tab)}"
    }
  }
  case class TypeErrorIn(tpeErr: TypeError, pack: Package.PackageF[Unit, Program[Expr[Declaration], Statement]]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)]) = {
      val (lm, sourceName) = sourceMap(pack.name)
      tpeErr.message(sourceName + s" in package ${pack.name.asString}", lm)
    }
  }
}
