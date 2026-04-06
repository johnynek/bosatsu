package dev.bosatsu

import java.math.{BigInteger, BigDecimal}
import java.util.Locale
import org.typelevel.paiges.Doc
import cats.parse.{Parser0 => P0, Parser => P, Numbers}
import cats.{Eq, Show}
import scala.compiletime.{constValue, erasedValue, summonInline}
import scala.deriving.Mirror

import cats.syntax.all._

/** A simple JSON ast for output
  */
sealed abstract class Json derives CanEqual {
  def toDoc: Doc

  def render: String
}

object Json {
  import Doc.text

  private inline def summonLabels[T <: Tuple]: List[String] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts) =>
        constValue[t].asInstanceOf[String] :: summonLabels[ts]
    }

  // YAML core-schema parsers can coerce these plain scalars into non-strings.
  // Keep them quoted so YAML and JSON output stay semantically aligned.
  private val yamlAmbiguousScalars: Set[String] =
    Set("true", "false", "null", "yes", "no", "on", "off")

  private def isYamlPlainStartChar(c: Char): Boolean =
    c.isLetter || c == '_' || c == '/'

  private def isYamlPlainChar(c: Char): Boolean =
    c.isLetterOrDigit || c == '_' || c == '-' || c == '.' || c == '/'

  private def needsYamlUnicodeEscape(c: Char): Boolean = {
    val codePoint = c.toInt
    (codePoint < 0x20) ||
    (codePoint == 0x7f) ||
    (0x80 <= codePoint && codePoint < 0xa0) ||
    Character.isSurrogate(c)
  }

  private def appendHex4(sb: java.lang.StringBuilder, c: Char): Unit = {
    val hex = c.toInt.toHexString
    var idx = hex.length
    while (idx < 4) {
      sb.append('0')
      idx += 1
    }
    sb.append(hex): Unit
  }

  private def yamlEscapeQuoted(str: String): String = {
    val sb = new java.lang.StringBuilder
    var idx = 0
    while (idx < str.length) {
      val c = str.charAt(idx)
      c match {
        case '"'  => sb.append("\\\"")
        case '\\' => sb.append("\\\\")
        case '\b' => sb.append("\\b")
        case '\f' => sb.append("\\f")
        case '\n' => sb.append("\\n")
        case '\r' => sb.append("\\r")
        case '\t' => sb.append("\\t")
        case _ =>
          if (needsYamlUnicodeEscape(c)) {
            sb.append("\\u")
            appendHex4(sb, c)
          } else {
            sb.append(c)
          }
      }
      idx += 1
    }
    sb.toString
  }

  private def yamlQuotedStringDoc(str: String): Doc =
    Doc.char('"') + Doc.text(yamlEscapeQuoted(str)) + Doc.char('"')

  // conservative plain-scalar rules keep values parse-stable across YAML implementations
  private def safePlainYamlString(str: String): Boolean =
    str.nonEmpty &&
      !str.exists(_.isWhitespace) &&
      parserFile.parseAll(str).isLeft &&
      !yamlAmbiguousScalars(str.toLowerCase(Locale.ROOT)) &&
      isYamlPlainStartChar(str.head) &&
      str.forall(isYamlPlainChar)

  private def yamlStringDoc(str: String): Doc =
    if (safePlainYamlString(str)) text(str)
    else yamlQuotedStringDoc(str)

  private def hasNestedYamlContent(j: Json): Boolean =
    j match {
      case JArray(items) => items.nonEmpty
      case JObject(items) =>
        items.nonEmpty
      case _ => false
    }

  private def yamlItemDoc(j: Json): Doc = {
    val valueDoc = toYamlDoc(j)
    if (hasNestedYamlContent(j)) Doc.char('-') + (Doc.line + valueDoc).nested(2)
    else Doc.text("- ") + valueDoc
  }

  private def yamlFieldDoc(key: String, value: Json): Doc = {
    val keyDoc = yamlStringDoc(key)
    val valueDoc = toYamlDoc(value)
    if (hasNestedYamlContent(value))
      keyDoc + Doc.char(':') + (Doc.line + valueDoc).nested(2)
    else keyDoc + Doc.text(": ") + valueDoc
  }

  def toYamlDoc(j: Json): Doc =
    j match {
      case JString(str)     => yamlStringDoc(str)
      case JNumberStr(str)  => text(str)
      case JBool.True       => text("true")
      case JBool.False      => text("false")
      case JNull            => text("null")
      case JArray(toVector) =>
        if (toVector.isEmpty) emptyArray
        else Doc.intercalate(Doc.line, toVector.map(yamlItemDoc))
      case jobj @ JObject(items) =>
        if (items.isEmpty) emptyDict
        else Doc.intercalate(Doc.line, jobj.keys.map(k => yamlFieldDoc(k, jobj.toMap(k))))
    }

  def renderYaml(j: Json): String =
    toYamlDoc(j).render(80)

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

  private sealed trait RenderFrame
  private final case class ArrayFrame(
      items: Vector[Json],
      nextIndex: Int,
      indent: Int
  ) extends RenderFrame
  private final case class ObjectFrame(
      keys: Vector[String],
      values: Map[String, Json],
      nextIndex: Int,
      indent: Int
  ) extends RenderFrame

  private def quotedDoc(value: String): Doc =
    Doc.char('"') + Doc.text(JsonStringUtil.escape('"', value)) + Doc.char('"')

  private def concatBalanced(parts: Vector[Doc]): Doc =
    if (parts.isEmpty) Doc.empty
    else {
      var level = parts
      while (level.length > 1) {
        val next = Vector.newBuilder[Doc]
        var idx = 0
        while (idx < level.length) {
          if (idx + 1 < level.length) next += (level(idx) + level(idx + 1))
          else next += level(idx)
          idx += 2
        }
        level = next.result()
      }
      level(0)
    }

  private def toDocStackSafe(root: Json): Doc = {
    val out = scala.collection.mutable.ArrayBuffer.empty[Doc]
    val indentDocs = scala.collection.mutable.HashMap.empty[Int, Doc]

    def indentDoc(indent: Int): Doc =
      indentDocs.getOrElseUpdate(indent, Doc.text(" " * indent))

    var stack: List[RenderFrame] = Nil
    var current: Option[(Json, Int)] = Some((root, 0))

    while (current.nonEmpty || stack.nonEmpty) {
      current match {
        case Some((json, indent)) =>
          current = None
          json match {
            case JString(str) =>
              out += quotedDoc(str)
            case JNumberStr(asString) =>
              out += text(asString)
            case JBool.True =>
              out += text("true")
            case JBool.False =>
              out += text("false")
            case JNull =>
              out += text("null")
            case JArray(items) =>
              if (items.isEmpty) out += emptyArray
              else {
                out += Doc.char('[')
                out += Doc.hardLine
                out += indentDoc(indent + 2)
                stack = ArrayFrame(items, nextIndex = 1, indent = indent) :: stack
                current = Some((items(0), indent + 2))
              }
            case obj @ JObject(items) =>
              if (items.isEmpty) out += emptyDict
              else {
                val keys = obj.keys.toVector
                val values = obj.toMap
                out += Doc.char('{')
                out += Doc.hardLine
                out += indentDoc(indent + 2)
                out += quotedDoc(keys(0))
                out += Doc.text(": ")
                stack =
                  ObjectFrame(keys, values, nextIndex = 1, indent = indent) :: stack
                current = Some((values(keys(0)), indent + 2))
              }
          }

        case None =>
          stack match {
            case ArrayFrame(items, nextIndex, indent) :: tail =>
              if (nextIndex < items.length) {
                out += Doc.comma
                out += Doc.hardLine
                out += indentDoc(indent + 2)
                stack = ArrayFrame(items, nextIndex + 1, indent) :: tail
                current = Some((items(nextIndex), indent + 2))
              } else {
                out += Doc.hardLine
                out += indentDoc(indent)
                out += Doc.char(']')
                stack = tail
              }

            case ObjectFrame(keys, values, nextIndex, indent) :: tail =>
              if (nextIndex < keys.length) {
                out += Doc.comma
                out += Doc.hardLine
                out += indentDoc(indent + 2)
                val key = keys(nextIndex)
                out += quotedDoc(key)
                out += Doc.text(": ")
                stack = ObjectFrame(keys, values, nextIndex + 1, indent) :: tail
                current = Some((values(key), indent + 2))
              } else {
                out += Doc.hardLine
                out += indentDoc(indent)
                out += Doc.char('}')
                stack = tail
              }

            case Nil =>
              ()
          }
      }
    }

    concatBalanced(out.toVector)
  }

  private val emptyArray = Doc.text("[]")
  final case class JArray(toVector: Vector[Json]) extends Json {
    def toDoc = toDocStackSafe(this)
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

    def toDoc = toDocStackSafe(this)

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

    trait ProductFieldReader[A] {
      def read(from: FromObj, key: String): Either[(String, Json, Path), A]
    }

    trait AnyProductFieldReader {
      def read(from: FromObj, key: String): Either[(String, Json, Path), Any]
    }

    final class LiftedProductFieldReader[A](
        mk: () => ProductFieldReader[A]
    ) extends AnyProductFieldReader {
      private lazy val reader = mk()

      def read(from: FromObj, key: String): Either[(String, Json, Path), Any] =
        reader.read(from, key)
    }

    final class DerivedProductReader[A](
        val describe: String,
        fieldLabels: List[String],
        fieldReaders: List[AnyProductFieldReader],
        fromProduct: Product => A
    ) extends Obj[A] {
      private val size = fieldReaders.size

      def readObj(from: FromObj): Either[(String, Json, Path), A] = {
        val values = new Array[Any](size)

        @annotation.tailrec
        def loop(
            labels: List[String],
            readers: List[AnyProductFieldReader],
            idx: Int
        ): Either[(String, Json, Path), A] =
          readers match {
            case Nil =>
              Right(fromProduct(Tuple.fromArray(values)))
            case reader :: nextReaders =>
              reader.read(from, labels.head) match {
                case Right(value) =>
                  values(idx) = value
                  loop(labels.tail, nextReaders, idx + 1)
                case Left(err) =>
                  Left(err)
              }
          }

        loop(fieldLabels, fieldReaders, 0)
      }
    }

    private inline def summonProductFieldReaders[T <: Tuple]
        : List[AnyProductFieldReader] =
      inline erasedValue[T] match {
        case _: EmptyTuple => Nil
        case _: (t *: ts) =>
          new LiftedProductFieldReader[t](() => summonInline[ProductFieldReader[t]]) ::
            summonProductFieldReaders[ts]
      }

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

    given [A](using r: Reader[A]): ProductFieldReader[A] with {
      def read(from: FromObj, key: String): Either[(String, Json, Path), A] =
        from.field[A](key)
    }

    given [A](using r: Reader[A]): ProductFieldReader[Option[A]] with {
      def read(
          from: FromObj,
          key: String
      ): Either[(String, Json, Path), Option[A]] =
        from.optional[A](key)
    }

    given optionJsonFieldReader: ProductFieldReader[Option[Json]] with {
      def read(
          from: FromObj,
          key: String
      ): Either[(String, Json, Path), Option[Json]] =
        from.optional[Json](key).map {
          case Some(obj: JObject) if obj.keys.isEmpty => None
          case other                                  => other
        }
    }

    given optionListFieldReader[A: Reader]: ProductFieldReader[Option[List[A]]] with {
      def read(
          from: FromObj,
          key: String
      ): Either[(String, Json, Path), Option[List[A]]] =
        from.optional[List[A]](key).map(_.filter(_.nonEmpty))
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

    implicit val jsonReader: Reader[Json] =
      new Reader[Json] {
        val describe = "Json"
        def read(path: Path, j: Json): Either[(String, Json, Path), Json] =
          Right(j)
      }

    implicit val intReader: Reader[Int] =
      new Reader[Int] {
        val describe = "Int"
        private val intMin = java.math.BigInteger.valueOf(Int.MinValue.toLong)
        private val intMax = java.math.BigInteger.valueOf(Int.MaxValue.toLong)
        def read(path: Path, j: Json): Either[(String, Json, Path), Int] =
          j match {
            case JBigInteger(bi)
                if bi.compareTo(intMin) >= 0 && bi.compareTo(intMax) <= 0 =>
              Right(bi.intValue)
            case JBigInteger(bi) =>
              Left((s"$bi cannot fit in Int", j, path))
            case _ =>
              Left((s"expected to find $describe", j, path))
          }
      }

    implicit val boolReader: Reader[Boolean] =
      new Reader[Boolean] {
        val describe = "Boolean"
        def read(
            path: Path,
            j: Json
        ): Either[(String, Json, Path), Boolean] =
          j match {
            case JBool(value) => Right(value)
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

    implicit def stringMapReader[A: Reader]: Reader[Map[String, A]] =
      new Reader[Map[String, A]] {
        val describe = s"Map[String, ${Reader[A].describe}]"
        def read(
            path: Path,
            j: Json
        ): Either[(String, Json, Path), Map[String, A]] =
          j match {
            case jobj: JObject =>
              jobj.keys
                .traverse { key =>
                  Reader[A].read(path.key(key), jobj.toMap(key)).map(key -> _)
                }
                .map(_.toMap)
            case _ => Left((s"expected to find $describe", j, path))
          }
      }

    implicit def nullableReader[A: Reader]: Reader[Nullable[A]] =
      new Reader[Nullable[A]] {
        val describe = s"Nullable[${Reader[A].describe}]"
        def read(path: Path, j: Json): Either[(String, Json, Path), Nullable[A]] =
          j match {
            case JNull => Right(Nullable.empty)
            case other => Reader[A].read(path, other).map(Nullable(_))
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

    inline given derived[A](using mirror: Mirror.ProductOf[A]): Reader[A] =
      new DerivedProductReader[A](
        constValue[mirror.MirroredLabel].asInstanceOf[String],
        summonLabels[mirror.MirroredElemLabels],
        summonProductFieldReaders[mirror.MirroredElemTypes],
        mirror.fromProduct
      )
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

    trait ProductFieldWriter[-A] {
      def fields(name: String, value: A): List[(String, Json)]
    }

    trait AnyProductFieldWriter {
      def fields(name: String, value: Any): List[(String, Json)]
    }

    final class LiftedProductFieldWriter[A](
        mk: () => ProductFieldWriter[A]
    ) extends AnyProductFieldWriter {
      private lazy val writer = mk()

      def fields(name: String, value: Any): List[(String, Json)] =
        writer.fields(name, value.asInstanceOf[A])
    }

    final class DerivedProductWriter[A <: Product](
        fieldLabels: List[String],
        fieldWriters: List[AnyProductFieldWriter]
    ) extends Writer[A] {
      def write(a: A): Json = {
        val builder = List.newBuilder[(String, Json)]
        val product = a

        var idx = 0
        var labels = fieldLabels
        var writers = fieldWriters
        while (writers.nonEmpty) {
          builder ++= writers.head.fields(labels.head, product.productElement(idx))
          idx += 1
          labels = labels.tail
          writers = writers.tail
        }

        JObject(builder.result())
      }
    }

    private inline def summonProductFieldWriters[T <: Tuple]
        : List[AnyProductFieldWriter] =
      inline erasedValue[T] match {
        case _: EmptyTuple => Nil
        case _: (t *: ts) =>
          new LiftedProductFieldWriter[t](() => summonInline[ProductFieldWriter[t]]) ::
            summonProductFieldWriters[ts]
      }

    def write[A](a: A)(implicit w: Writer[A]): Json = w.write(a)

    def from[A](fn: A => Json): Writer[A] =
      new Writer[A] {
        def write(value: A) = fn(value)
      }

    given [A](using w: Writer[A]): ProductFieldWriter[A] with {
      def fields(name: String, value: A): List[(String, Json)] =
        (name -> write(value)) :: Nil
    }

    given [A](using w: Writer[A]): ProductFieldWriter[Option[A]] with {
      def fields(name: String, value: Option[A]): List[(String, Json)] =
        value match {
          case Some(item) => (name -> write(item)) :: Nil
          case None       => Nil
        }
    }

    given optionJsonFieldWriter: ProductFieldWriter[Option[Json]] with {
      def fields(name: String, value: Option[Json]): List[(String, Json)] =
        value match {
          case None                                   => Nil
          case Some(obj: JObject) if obj.keys.isEmpty => Nil
          case Some(item)                             => (name -> write(item)) :: Nil
        }
    }

    given optionListFieldWriter[A: Writer]: ProductFieldWriter[Option[List[A]]] with {
      def fields(name: String, value: Option[List[A]]): List[(String, Json)] =
        value.filter(_.nonEmpty) match {
          case Some(items) => (name -> write(items)) :: Nil
          case None        => Nil
        }
    }

    implicit val stringWriter: Writer[String] =
      from(JString(_))

    implicit val jsonWriter: Writer[Json] =
      from(identity)

    implicit val intWriter: Writer[Int] =
      from(i => JNumberStr(i.toString))

    implicit val boolWriter: Writer[Boolean] =
      from(JBool(_))

    implicit def listWriter[A: Writer]: Writer[List[A]] =
      from(list => JArray(list.map(write(_)).toVector))

    implicit def stringMapWriter[A: Writer]: Writer[Map[String, A]] =
      from { map =>
        JObject(map.toList.sortBy(_._1).map { case (k, v) => k -> write(v) })
      }

    implicit def nullableWriter[A: Writer]: Writer[Nullable[A]] =
      from { value =>
        value.fold[Json](JNull)(write(_))
      }

    inline given derived[A <: Product](using mirror: Mirror.ProductOf[A]): Writer[A] =
      new DerivedProductWriter[A](
        summonLabels[mirror.MirroredElemLabels],
        summonProductFieldWriters[mirror.MirroredElemTypes]
      )
  }
}
