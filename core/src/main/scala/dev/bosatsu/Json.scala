package dev.bosatsu

import java.math.{BigInteger, BigDecimal}
import org.typelevel.paiges.Doc
import cats.parse.{Parser0 => P0, Parser => P, Numbers}
import cats.{Eq, Show}

import cats.syntax.all._

/** A simple JSON ast for output
  */
sealed abstract class Json derives CanEqual {
  def toDoc: Doc

  def render: String
}

object Json {
  import Doc.text

  final case class JString(str: String) extends Json {
    override def render = "\"%s\"".format(JsonStringUtil.escape('"', str))
    def toDoc = text(render)
  }
  final case class JNumberStr(asString: String) extends Json {
    override def render = asString
    def toDoc = text(asString)

    def toBigInteger: Option[BigInteger] =
      try Some(new BigDecimal(asString).toBigIntegerExact)
      catch {
        case (_: ArithmeticException) => None
      }
  }

  object JBigInteger {
    // optimize the common case
    private def allDigits(str: String): Boolean = {
      var idx = 0
      while (idx < str.length) {
        val c = str(idx)
        if (c < '0' || '9' < c) return false
        idx = idx + 1
      }
      true
    }
    def unapply(j: Json): Option[BigInteger] =
      j match {
        case num @ JNumberStr(str) =>
          if (allDigits(str)) Some(new BigInteger(str))
          else num.toBigInteger
        case _ => None
      }
  }

  object JBool {
    case object True extends Json {
      override val render = "true"
      val toDoc = text(render)
    }
    case object False extends Json {
      override val render = "false"
      val toDoc = text(render)
    }

    def apply(bool: Boolean): Json =
      if (bool) True else False

    private val someTrue = Some(true)
    private val someFalse = Some(false)

    def unapply(j: Json): Option[Boolean] =
      j match {
        case True  => someTrue
        case False => someFalse
        case _     => None
      }

  }

  case object JNull extends Json {
    override val render = "null"
    val toDoc = text(render)
  }
  private val emptyArray = Doc.text("[]")
  final case class JArray(toVector: Vector[Json]) extends Json {
    def toDoc =
      if (toVector.isEmpty) emptyArray
      else {
        val parts = Doc.intercalate(
          Doc.comma,
          toVector.map(j => (Doc.line + j.toDoc).grouped)
        )
        "[" +: ((parts :+ " ]").nested(2))
      }

    def render = toDoc.render(80)
  }
  private val emptyDict = Doc.text("{}")
  // we use a List here to preserve the order in which items
  // were given to us
  final case class JObject(items: List[(String, Json)]) extends Json {
    val toMap: Map[String, Json] = items.toMap
    val keys: List[String] = items.map(_._1).distinct

    def getOrNull(key: String): Json =
      toMap.get(key) match {
        case Some(value) => value
        case None        => JNull
      }

    def toDoc =
      if (items.isEmpty) emptyDict
      else {
        val kvs = keys.map { k =>
          val j = toMap(k)
          JString(k).toDoc + Doc.char(':') + ((Doc.lineOrSpace + j.toDoc)
            .nested(2))
        }
        val parts = Doc.intercalate(Doc.comma + Doc.line, kvs).grouped
        parts.bracketBy(text("{"), text("}"))
      }

    /** Return a JObject with each key at most once, but in the order of this
      */
    def normalize: JObject = JObject(keys.map(k => (k, toMap(k))))

    def render = toDoc.render(80)
  }

  /** this checks for semantic equivalence:
    *   1. we use BigDecimal to compare JNumberStr 2. we normalize objects
    */
  implicit val eqJson: Eq[Json] =
    new Eq[Json] {
      def eqv(a: Json, b: Json) =
        (a, b) match {
          case (JNull, JNull)                   => true
          case (JBool.True, JBool.True)         => true
          case (JBool.False, JBool.False)       => true
          case (JString(sa), JString(sb))       => sa == sb
          case (JNumberStr(sa), JNumberStr(sb)) =>
            new BigDecimal(sa).compareTo(new BigDecimal(sb)) == 0
          case (JArray(itemsa), JArray(itemsb)) =>
            (itemsa.size == itemsb.size) &&
            itemsa.iterator
              .zip(itemsb.iterator)
              .forall { case (a, b) => eqv(a, b) }
          case (oa @ JObject(_), ob @ JObject(_)) =>
            val na = oa.normalize
            val nb = ob.normalize
            (na.toMap.keySet == nb.toMap.keySet) &&
            na.keys.forall { k =>
              eqv(na.toMap(k), nb.toMap(k))
            }
          case (_, _) => false
        }
    }

  private val whitespace: P[Unit] = P.charIn(" \t\r\n").void
  private val whitespaces0: P0[Unit] = whitespace.rep0.void

  /** This doesn't have to be super fast (but is fairly fast) since we use it in
    * places where speed won't matter: feeding it into a program that will
    * convert it to bosatsu structured data
    */
  val parser: P[Json] = {
    val recurse = P.defer(parser)

    // cats-parse uses a radix tree for these so it only needs to check 1 character
    // to see if it misses
    val pconst = P.fromStringMap(
      Map(
        "null" -> JNull,
        "true" -> JBool.True,
        "false" -> JBool.False
        /* you can imagine going nuts, but we should justify this with benchmarks
      "0" -> JNumberStr("0"),
      "1" -> JNumberStr("1"),
      "\"\"" -> JString(""),
      "[]" -> JArray(Vector.empty),
      "{}" -> JObject(Nil)
         */
      )
    )

    val justStr = JsonStringUtil.escapedString('"')
    val str = justStr.map(JString(_))
    val num = Parser.JsonNumber.parser.map(JNumberStr(_))

    val listSep: P[Unit] =
      (whitespaces0.with1.soft ~ P.char(',') ~ whitespaces0).void

    def rep[A](pa: P[A]): P0[List[A]] =
      (whitespaces0 *> P.repSep0(pa, min = 0, sep = listSep) <* whitespaces0)

    val list = (P.char('[') *> rep(recurse) <* P.char(']'))
      .map(vs => JArray(vs.toVector))

    val kv: P[(String, Json)] =
      justStr ~ ((whitespaces0.with1 ~ P.char(':') ~ whitespaces0) *> recurse)

    val obj = (P.char('{') *> rep(kv) <* P.char('}'))
      .map(vs => JObject(vs))

    P.oneOf(pconst :: str :: num :: list :: obj :: Nil)
  }

  // any whitespace followed by json followed by whitespace followed by end
  val parserFile: P[Json] =
    whitespaces0.with1 *> (parser ~ whitespaces0 ~ P.end).map(_._1._1)

  implicit val showJson: Show[Json] =
    new Show[Json] {
      def show(j: Json) = j.render
    }

  sealed abstract class Path derives CanEqual {
    def index(idx: Int): Path = Path.Index(this, idx)
    def key(key: String): Path = Path.Key(this, key)
  }
  object Path {
    case object Root extends Path
    case class Index(of: Path, idx: Int) extends Path
    case class Key(of: Path, key: String) extends Path

    implicit val showPath: Show[Path] =
      new Show[Path] {
        def show(p: Path): String = {
          @annotation.tailrec
          def loop(p: Path, rhs: List[String]): String =
            p match {
              case Root           => rhs.mkString("/", "", "")
              case Index(of, idx) =>
                loop(of, s"[$idx]" :: rhs)
              case Key(of, key) =>
                loop(of, s".\"${StringUtil.escape('"', key)}\"" :: rhs)
            }

          loop(p, Nil)
        }
      }

    val parser: P[Path] = {
      val pIdx = P.char('[') *> Numbers.bigInt.flatMap { bi =>
        if (bi.isValidInt) P.pure(bi.toInt)
        else P.failWith(s"$bi cannot fit in Int")
      } <* P.char(']')

      val pKey = P.char('.') *> StringUtil.escapedString('"')

      val part: P[Either[String, Int]] = pIdx.eitherOr(pKey)

      P.char('/') *> part.rep0.map { list =>
        list.foldLeft(Root: Path) {
          case (p, Right(idx)) => Index(p, idx)
          case (p, Left(key))  => Key(p, key)
        }
      }
    }
  }

  trait Reader[+A] { self =>
    def describe: String
    def read(path: Path, j: Json): Either[(String, Json, Path), A]

    final def mapEither[B](
        desc: String
    )(fn: A => Either[String, B]): Reader[B] =
      new Reader[B] {
        def describe: String = desc
        def read(path: Path, j: Json): Either[(String, Json, Path), B] =
          self.read(path, j).flatMap { a =>
            fn(a) match {
              case r @ Right(_) => r.leftCast
              case Left(value)  => Left((value, j, path))
            }
          }
      }
  }

  object Reader {
    def apply[A](implicit r: Reader[A]): Reader[A] = r

    case class FromObj(path: Path, j: JObject) {
      def field[A: Reader](key: String): Either[(String, Json, Path), A] = {
        val jv = j.getOrNull(key)
        val p1 = path.key(key)
        Reader[A].read(p1, jv)
      }

      def optional[A: Reader](
          key: String
      ): Either[(String, Json, Path), Option[A]] =
        j.getOrNull(key) match {
          case JNull   => Right(None)
          case notNull =>
            val p1 = path.key(key)
            Reader[A].read(p1, notNull).map(Some(_))
        }
    }

    trait Obj[A] extends Reader[A] {
      def readObj(from: FromObj): Either[(String, Json, Path), A]

      final def read(path: Path, j: Json): Either[(String, Json, Path), A] =
        j match {
          case jobj: JObject => readObj(FromObj(path, jobj))
          case _             => Left((s"expected obj with $describe", j, path))
        }
    }

    implicit val stringReader: Reader[String] =
      new Reader[String] {
        val describe = "String"
        def read(path: Path, j: Json): Either[(String, Json, Path), String] =
          j match {
            case JString(str) => Right(str)
            case _            => Left((s"expected to find $describe", j, path))
          }
      }

    implicit def listReader[A: Reader]: Reader[List[A]] =
      new Reader[List[A]] {
        val describe = s"List[${Reader[A].describe}]"
        def read(path: Path, j: Json): Either[(String, Json, Path), List[A]] =
          j match {
            case JArray(items) =>
              items
                .traverseWithIndexM { (a, idx) =>
                  Reader[A].read(path.index(idx), a)
                }
                .map(_.toList)
            case _ => Left((s"expected to find $describe", j, path))
          }
      }

    implicit def fromParser[A](desc: String, p0: P0[A]): Reader[A] =
      new Reader[A] {
        def describe: String = desc
        def read(path: Path, j: Json): Either[(String, Json, Path), A] =
          j match {
            case JString(str) =>
              p0.parseAll(str) match {
                case Right(value) => Right(value)
                case Left(value)  =>
                  Left((show"string parser error: $value", j, path))
              }
            case _ => Left((s"expected to find $describe", j, path))
          }
      }
  }

  trait Writer[A] { self =>
    def write(value: A): Json
    final def contramap[B](fn: B => A): Writer[B] =
      new Writer[B] {
        def write(value: B): Json = self.write(fn(value))
      }
  }

  object Writer {
    def apply[A](implicit w: Writer[A]): Writer[A] = w

    def write[A](a: A)(implicit w: Writer[A]): Json = w.write(a)

    def from[A](fn: A => Json): Writer[A] =
      new Writer[A] {
        def write(value: A) = fn(value)
      }

    implicit val stringWriter: Writer[String] =
      from(JString(_))

    implicit def listWriter[A: Writer]: Writer[List[A]] =
      from(list => JArray(list.map(write(_)).toVector))
  }
}
