package org.bykn.bosatsu

import cats.Eval
import cats.data.{Validated, ValidatedNel}
import cats.implicits._
import com.monovore.decline._
import java.nio.file.Path
import org.typelevel.paiges.Doc

object Foo {
  def times(i: java.lang.Integer): java.lang.Integer =
    java.lang.Integer.valueOf(i.intValue + 42)
}

object Std {
  //fold = ffi scala org.bykn.bosatsu.Std.fold List[a] -> b -> (b -> a -> b) -> b
  @annotation.tailrec
  final def fold(list: Any, bv: Any, fn: Any): Any = {
    list match {
      case (0, _) =>
        // Empty
        bv
      case (1, head :: tail :: Nil) =>
        val fnT = fn.asInstanceOf[Fn[Any, Fn[Any, Any]]]
        fold(tail, fnT(bv)(head), fn)
      case _ => sys.error(s"unexpected: $list")
    }
  }

  def unitValue: AnyRef = (0, Nil)

  def print(i: Any): Any =
    Eval.always { println(i.toString); unitValue }

  def flatMap(act: Any, fn: Any): Any =
    act.asInstanceOf[Eval[Any]].flatMap { v =>
      fn.asInstanceOf[Fn[Any, Eval[Any]]](v)
    }
  def mapAction(act: Any, fn: Any): Any =
    act.asInstanceOf[Eval[Any]].map { v =>
      fn.asInstanceOf[Fn[Any, Any]](v)
    }

  def toAction(a: Any): Any = Eval.now(a)
  def runAction(a: Any): Any = a.asInstanceOf[Eval[Any]].value
}

object Main extends CommandApp(
  name = "bosatsu",
  header = "a total language",
  main = {
    implicit val argPack: Argument[PackageName] =
      new Argument[PackageName] {
        def defaultMetavar: String = "packageName"
        def read(string: String): ValidatedNel[String, PackageName] =
          PackageName.parse(string) match {
            case Some(pn) => Validated.valid(pn)
            case None => Validated.invalidNel(s"could not parse $string as a package name. Must be capitalized strings separated by /")
          }
      }
    val ins = Opts.options[Path]("input", help = "input files")
    val outputPath = Opts.option[Path]("output", help = "output path").orNone
    val extern = Opts.options[Path]("external", help = "input files").orNone
    val compileRoot = Opts.option[Path]("compile_root", help = "root directory to write java output").orNone
    val jsonOutput = Opts.flag("json", help = "evaluate to a json value").orFalse
    val mainP = Opts.option[PackageName]("main", help = "main package to evaluate").orNone
    (ins, extern, mainP, compileRoot, jsonOutput, outputPath).mapN { (paths, optExt, mainPack, croot, toJson, outPath) =>


      val extV = optExt match {
        case None => Validated.valid(Externals.empty)
        case Some(epaths) =>
          epaths.traverse(Parser.parseFile(Externals.parser, _))
            .map(_.toList.map(_._2).reduce(_ ++ _))
      }

      val parsedPathsV = paths.traverse { path =>
        Parser.parseFile(Package.parser, path).map { case (lm, parsed) =>
          ((path.toString, lm), parsed)
        }
      }

      val (parsedPaths, extern) = parsedPathsV.product(extV) match {
        case Validated.Valid(res) => res
        case Validated.Invalid(errs) =>
          errs.toList.foreach {
            case Parser.Error.PartialParse(_, pos, map, Some(path)) =>
              // we should never be partial here
              val (r, c) = map.toLineCol(pos).get
              val ctx = map.showContext(pos).get
              System.err.println(s"failed to parse $path at line ${r + 1}, column ${c + 1}")
              System.err.println(ctx)
              System.exit(1)
            case Parser.Error.ParseFailure(pos, map, Some(path)) =>
              // we should never be partial here
              val (r, c) = map.toLineCol(pos).get
              val ctx = map.showContext(pos).get
              System.err.println(s"failed to parse $path at line ${r + 1}, column ${c + 1}")
              System.err.println(ctx)
              System.exit(1)
            case Parser.Error.FileError(path, err) =>
              System.err.println(s"failed to parse $path")
              System.err.println(err.getMessage)
              System.err.println(err.getClass)
              System.exit(1)
            case other =>
              System.err.println(s"unexpected error $other")
              System.exit(1)
          }
          sys.error("unreachable")
      }

      PackageMap.resolveThenInfer(Predef.withPredefA(("predef", LocationMap("")), parsedPaths.toList)) match {
        case (duplicatePackages, Validated.Valid(_)) if duplicatePackages.nonEmpty =>
          // we have duplications, but those that are duplicated are okay
          duplicatePackages.foreach { case (pname, (((src, _), _), nelist)) =>
            val dupsrcs = (src :: nelist.map { case ((s, _), _) => s }.toList).sorted.mkString(", ")
            System.err.println(s"package ${pname.asString} duplicated in $dupsrcs")
          }
          System.exit(1)
        case (_, Validated.Valid(packMap)) =>
          croot match {
            case None =>
              mainPack match {
                case None =>
                  // Just type check
                  println(s"typechecked ${packMap.toMap.size} packages")

                case Some(main) =>
                  // Something to evaluate
                  val ev = Evaluation(packMap, Predef.jvmExternals ++ extern)
                  ev.evaluateLast(main) match {
                    case None => sys.error("found no main expression")
                    case Some((eval, scheme)) =>
                      val res = eval.value
                      (outPath, toJson) match {
                        case (None, true) =>
                          println(ev.toJson(res, scheme).get.toDoc.render(80))
                        case (Some(p), true) =>
                          CodeGen.writeDoc(p, ev.toJson(res, scheme).get.toDoc).get
                        case (None, false) =>
                          println(s"$res: ${scheme.result}")
                        case (Some(p), false) =>
                          CodeGen.writeDoc(p, Doc.text(s"$res: ${scheme.result}")).get
                      }
                  }
              }
            case Some(rootPath) =>
              CodeGen.write(rootPath,  packMap, Predef.jvmExternals ++ extern).get
          }

        case (duplicatePackages, Validated.Invalid(errs)) =>
          val sourceMap = parsedPaths.map { case ((src, lm), pack) => (pack.name, (lm, src)) }.toList.toMap
          sys.error("failed: " + errs.map(_.message(sourceMap)).toList.mkString("\n"))
          // we have duplications, but those that are duplicated are okay
          duplicatePackages.foreach { case (pname, (((src, _), _), nelist)) =>
            val dupsrcs = (src :: nelist.map { case ((s, _), _) => s }.toList).sorted.mkString(", ")
            System.err.println(s"package ${pname.asString} duplicated in $dupsrcs")
          }
          System.exit(1)
      }
    }
  }
)
