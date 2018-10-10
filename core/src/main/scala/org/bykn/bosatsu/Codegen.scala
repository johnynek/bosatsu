package org.bykn.bosatsu

import org.typelevel.paiges.Doc
import cats.{Id, Monoid, Traverse, Monad, Foldable}
import cats.data.{RWST, NonEmptyList}
import cats.implicits._
import java.nio.file.Path
import scala.util.Try
import java.io.PrintWriter

import alleycats.std.map._ // TODO use SortedMap everywhere

import org.bykn.bosatsu.rankn.Type

trait CodeGen {
  import TypedExpr._
  import CodeGen._

  def toFieldName(s: String, i: Long): String =
    s + "$" + (i.toString)

  def toExportedName(s: String): String =
    s

  def toConstructorName(s: String): String =
    "cons$" + s

  def toPackage(p: PackageName): String =
    p.parts.map(_.toLowerCase).toList.mkString(".")

  def tell(d: Doc): Output[Unit] =
    RWST.tell[Id, Scope, Doc, Unique](d)

  def genPackage(p: Package.Inferred, ext: Externals): Output[Unit] = {
    val unfix = p.unfix

    def packageName: Doc =
      Doc.text("package ") +
        Doc.text(toPackage(unfix.name)) +
        Doc.char(';') + Doc.line

    def enclose(d: Doc): Doc =
      Doc.char('{') + (Doc.line + d).nested(2) + Doc.line + Doc.char('}')

    val isExported: Set[String] =
      unfix.exports.map(_.name).toSet

    /**
     * add the imports and return a list of lines to set up inside
     * Values
     */
    val imps: Output[List[NonEmptyList[Option[(Package.Inferred, String, String)]]]] =
      Traverse[List].traverse(unfix.imports) { case Import(pack, items) =>
        Traverse[NonEmptyList].traverse(items) { imp =>
          def go(fn: String => Option[String]): Output[Option[(Package.Inferred, String, String)]] =
            if (imp.isRenamed || isExported(imp.localName)) {
              // we make a new field for this.
              val opt = for {
                o <- fn(imp.originalName)
                l <- fn(imp.localName)
              } yield (pack, o, l)
              Monad[Output].pure(opt)
            }
            else {
              // we can just do a static import
              (fn(imp.originalName) match {
                case None =>
                  Monad[Output].pure(())
                case Some(n) =>
                  tell(Doc.text("import static ") + Doc.text(toPackage(pack.unfix.name)) +
                    Doc.text(".Values.") + Doc.text(n) + Doc.char(';') + Doc.line)
              }).map(_ => None)
            }

            Traverse[NonEmptyList].traverse(imp.tag) {
              case Referant.Value(_) => go { s => Some(toExportedName(s)) }
              case Referant.DefinedT(_) => go { _ => None }
              case Referant.Constructor(_, _, _, _) => go { s => Some(toConstructorName(s)) }
            }
        }
        .map(_.flatten)
      }

    def flatten[A](fn: List[NonEmptyList[Option[A]]]): List[A] =
      fn.flatMap(_.toList.flatMap(_.toList))

    val flatImps: Output[List[(Package.Inferred, String, String)]] = imps.map(flatten(_))

    def makeImportFields(ls: List[(Package.Inferred, String, String)]): Doc =
      Foldable[List].foldMap(ls) { case (p, orig, local) =>
        val priv = if (isExported(local)) "public " else "private "
        Doc.text(priv) + Doc.text("final static Object ") + Doc.text(toExportedName(local)) + Doc.text(" = ") +
          Doc.text(toPackage(p.unfix.name)) + Doc.text(".Values.") + Doc.text(toExportedName(orig)) + Doc.text(";") + Doc.line
      }

    val internalImports = List("Fn", "EnumValue")
      .map { i => Doc.text(s"import org.bykn.bosatsu.$i;") + Doc.line }
      .reduce(_ + _)

    val impsDoc: Output[Doc] = (tell(internalImports) >> flatImps).map(makeImportFields)

    /*
     * build the constructors
     */
    val definedTypes = unfix.program.types.definedTypes.values.toList.sortBy(_.name.asString)

    def mkConstructor(i: Int, cn: ConstructorName, args: List[(ParamName, Type)]): Output[Doc] = {
      val argC = args.size

      def go(offset: Long, arg: Int): Doc =
        if (arg == argC) {
          // actually create the instance:
          val ids = (0 until argC)
          val objs = ids.map { i => "x$" + s"${offset + i}" }.mkString(", ")
          Doc.text(s"new EnumValue($i, new Object[]{$objs})")
        }
        else {
          val argName = "x$" + s"${offset + arg}"
          val next = go(offset, arg + 1)
          Doc.text(s"new Fn<Object, Object>() { public Object apply(final Object $argName) { return ") + next + Doc.text("; } }")
        }

      getIds(argC).map { init =>
        val fn = go(init.id, 0)
        val cname = toConstructorName(cn.asString)
        // TODO check exports to see if this should be private
        Doc.text(s"public final static Object $cname = ") + fn + Doc.text(";") + Doc.line
      }
    }

    val constructors: Output[Unit] =
      definedTypes.flatMap(_.constructors.zipWithIndex).traverse_ { case ((cn, args, ct), idx) =>
        mkConstructor(idx, cn, ???).flatMap(tell)
      }
    /**
     * Build the externals
     */
    val externals = NameKind.externals(p)
    val extDoc: Output[Unit] =
      externals.traverse_ { case NameKind.ExternalDef(p, n, scheme) =>
        outputExternal(n, ext.toMap((p, n)), ???/*scheme*/)
      }

    val body = Traverse[List].traverse(unfix.program.lets) { case (f, e) =>
      val priv = if (isExported(f)) "public " else "private "
      val left = Doc.text(s"${priv}final static Object ") + Doc.text(toExportedName(f)) + Doc.text(" = ")
      for {
        eDoc <- apply(e, true, p)
        _ <- tell(left + eDoc + Doc.char(';') + Doc.line)
      } yield ()
    }.map(_ => ())

    impsDoc.map { valueImports =>
      val fullBody = tell(valueImports) >> extDoc >> constructors >> body
      fullBody.mapWritten { d: Doc =>
        Doc.text("public class Values ") + enclose(d)
      }
    }
    .flatMap { o => o }
    .mapWritten { d: Doc =>
      packageName + Doc.line + d
    }
  }

  def getIds(cnt: Int): Output[Unique] =
    for {
      id0 <- RWST.get[Id, Scope, Doc, Unique]
      _ = require(cnt >= 0, s"$cnt < 0")
      id1 = Unique(id0.id + cnt)
      _ <- RWST.set[Id, Scope, Doc, Unique](id1)
    } yield id0

  def nameIn[T](n: String)(out: Unique => Output[T]): Output[T] =
    for {
      id0 <- RWST.get[Id, Scope, Doc, Unique]
      id1 = id0.next
      _ <- RWST.set[Id, Scope, Doc, Unique](id1)
      t <- out(id0).local { s: Scope => Scope(s.toMap.updated(n, id0)) }
    } yield t

  def outputExternal(n: String, apicall: FfiCall, tpe: Type): Output[Unit] = {
    def getArity(t: Type): Int = {
      def loop(t: Type, top: Boolean): Int = {
        t match {
          case Type.Fun(a, b) if top =>
            loop(a, false) + loop(b, top)
          case _ => 1 // inner functions are just functions Objects
        }
      }
      loop(t, true)
    }

    val arity = getArity(tpe)
    assert((n, apicall, arity) != null)
          // case Some(NameKind.ExternalDef(p, n, scheme)) =>
          //   outputExternal(n, ext.toMap((p, n)), scheme)
    ???
  }

  /**
   * To codegen an expression, we first do any
   * setup side effect writing, in Output, then
   * return a Doc holding an expression that can
   * be a right-hand-side of an equation in java.
   */
  def apply[T](e: TypedExpr[T], topLevel: Boolean, pack: Package.Inferred): Output[Doc] =
    e match {
      case Generic(_, e, _) => apply(e, topLevel, pack)
      case Annotation(expr, _, _) =>
        // TODO we might want to use the type info
        apply(expr, topLevel, pack)
      case Var(n, _, _) =>
        NameKind(pack, n) match {
          case Some(NameKind.Constructor(_, _, _, _)) =>
            Monad[Output].pure(Doc.text(toConstructorName(n)))
          case _ =>
            for {
              scope <- RWST.ask[Id, Scope, Doc, Unique]
              // TODO: see Evaluation for the other cases here: imports, constructors and externals
              v = scope.toMap.get(n).fold(toExportedName(n)) { u => toFieldName(n, u.id) }
            } yield Doc.text(v)
        }
      case App(fn, arg, _,  _) =>
        for {
          fnDoc <- apply(fn, topLevel, pack)
          aDoc <- apply(arg, topLevel, pack)
        } yield Doc.text("((Fn<Object, Object>)") + fnDoc + Doc.char(')') + Doc.text(".apply(") + aDoc + Doc.char(')')

      case AnnotatedLambda(arg, _, exp, _) =>
        nameIn(arg) { ua =>
          nameIn("anon") { uanon =>
            val attr = if (topLevel) "private final static" else "final"
            for {
              _ <- tell(Doc.text(s"$attr Object anon${uanon.id} = "))
              _ <- tell((Doc.text("new Fn() {") + Doc.line +
                Doc.text(s"@Override public Object apply(final Object ${toFieldName(arg, ua.id)}) {") + Doc.line).nested(2))
              eDoc <- apply(exp, false, pack)
              _ <- tell(Doc.text("return ") + eDoc + Doc.char(';') + Doc.line + Doc.text("}") + Doc.line + Doc.text("};") + Doc.line)
            } yield Doc.text(s"anon${uanon.id}")
          }
        }

      case Let(nm, nmv, in, _) =>
        nameIn(nm) { ua =>
          nameIn("anon") { uanon =>
            for {
              nmDoc <- apply(nmv, topLevel, pack)
              _ <- tell(Doc.text(s"final Object ${toFieldName(nm, ua.id)} = ") + nmDoc + Doc.char(';') + Doc.line)
              inDoc <- apply(in, topLevel, pack)
            } yield inDoc
          }
        }
      case Literal(Lit.Integer(i), _, _) =>
        Monad[Output].pure(Doc.text("java.lang.Integer.valueOf(") + quote(i.toString) + Doc.char(')'))
      case Literal(Lit.Str(s), _,  _) =>
        Monad[Output].pure(quote(s))
      case If(cond, ift, iff, _) =>
        Monad[Output].pure(Doc.text("null"))
      case Match(_, _, _) =>
        Monad[Output].pure(Doc.text("null"))
    }

  def quote(str: String): Doc =
    Doc.char('"') + Doc.text(str) + Doc.char('"')
}

object CodeGen {
  case class Scope(toMap: Map[String, Unique])

  implicit val docMonoid: Monoid[Doc] =
    new Monoid[Doc] {
      def empty: Doc = Doc.empty
      def combine(a: Doc, b: Doc): Doc = a + b
    }

  type Output[A] = RWST[Id, Scope, Doc, Unique, A]

  def run[T](o: Output[T]): (Doc, T) =
    o.run(Scope(Map.empty), Unique(0L)) match {
      case (doc, _, t) => (doc, t)
    }

  @annotation.tailrec
  final def toPath(root: Path, pn: PackageName): Path =
    pn.parts match {
      case NonEmptyList(h, Nil) => root.resolve(h).resolve("Values.java")
      case NonEmptyList(h0, h1 :: tail) =>
        toPath(root.resolve(h0), PackageName(NonEmptyList(h1, tail)))
    }

  def writeDoc(p: Path, d: Doc): Try[Unit] =
    Try {
      Option(p.getParent).foreach(_.toFile.mkdirs)
      val pw = new PrintWriter(p.toFile, "UTF-8")
      val res = Try {
        d.renderStream(80).foreach(pw.print(_))
      }
      pw.close
      res
    }
    .flatten

  def write(root: Path, packages: PackageMap.Inferred, ext: Externals): Try[Unit] = {
    val cg = new CodeGen { }
    packages.toMap.traverse_ { pack =>
      val (d, _) = run(cg.genPackage(Package.asInferred(pack), ext))
      val path = toPath(root, pack.name)
      writeDoc(path, d)
    }
  }
}
