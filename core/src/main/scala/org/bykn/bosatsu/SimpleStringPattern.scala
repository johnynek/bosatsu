package org.bykn.bosatsu

object SimpleStringPattern {
  sealed trait Pattern {
    def unapply(str: String): Option[Map[String, String]] =
      matches(this, str)
/*
    // if this must start with string s,
    // return the rest of the pattern
    def take(s: String): Option[Pattern] =
      if (s.isEmpty) Some(this)
      else {
        this match {
          case Lit(s0) =>
            if (s0.startsWith(s)) Some(Lit(s0.drop(s.length)))
            else None
          case Var(_) | Wildcard => None
          case Cat(Lit(s0), tail) =>
            if (s0.isEmpty) tail.take(s)
            else {
              // we can only match as much of s as s0 is long
              val subs = s.take(s0.length)
              if (s0.startsWith(subs)) {
                // s0 does match the start of s
                if (subs.length == s0.length) {
                  // we matched all of s0, the rest of s has to match the tail
                  tail.take(s.drop(subs.length))
                }
                else {
                  // this implies s.length < s0.length
                  Some(Cat(Lit(s0.drop(subs.length)), tail))
                }
              }
              else None
            }
          case Cat(Var(_) | Wildcard, _) => None
        }
    }
*/
    def onlyMatchesEmpty: Boolean =
      this match {
        case Lit("") => true
        case Lit(_) | Var(_) | Wildcard => false
        case Cat(h, t) => h.onlyMatchesEmpty && t.onlyMatchesEmpty
      }
  }

  sealed trait Pattern1 extends Pattern

  case class Lit(str: String) extends Pattern1
  case class Var(name: String) extends Pattern1
  case object Wildcard extends Pattern1
  case class Cat(head: Pattern1, tail: Pattern) extends Pattern

  /**
   * If two vars are adjacent, the left one will always match empty string
   * this normalize just removes the left var
   */
  def normalize(p: Pattern): Pattern =
    p match {
      case lvw@(Lit(_) | Var(_) | Wildcard) => lvw
      case Cat(Var(_) | Wildcard, tail@Cat(Var(_) | Wildcard, _)) =>
        normalize(tail)
      case Cat(h, tail) =>
        Cat(h, normalize(tail))
    }


  def render(p: Pattern, vars: Map[String, String]): Option[String] =
    p match {
      case Lit(str) => Some(str)
      case Var(x) => vars.get(x)
      case Wildcard => None
      case Cat(head, rest) =>
        for {
          hstr <- render(head, vars)
          tstr <- render(rest, vars)
        } yield hstr ++ tstr
    }

  object Pattern {
    def apply(str: String): Pattern = {
      import fastparse.all._

      val varWild = Identifier.nameParser.map { n => Var(n.asString) } | P("_").map(_ => Wildcard)
      val ppat = StringUtil.interpolatedString('\'', P("${"), varWild, P("}"))

      def toPat(l: List[Either[Pattern1, (Region, String)]]): Pattern =
        l match {
          case Nil => Lit("")
          case Left(p1) :: tail =>
            Cat(p1, toPat(tail))
          case Right((_, lit)) :: tail =>
            Cat(Lit(lit), toPat(tail))
        }

      ppat.map(toPat(_)).parse(str) match {
        case Parsed.Success(pat, _) => pat
        case other => sys.error(s"could not parse: $other")
      }
    }
  }

  private[this] val sme = Some(Map.empty[String, String])

  def matches(p: Pattern, str: String): Option[Map[String, String]] =
    p match {
      case Lit(s0) =>
        if (s0 == str) sme
        else None
      case Var(n) => Some(Map(n -> str))
      case Wildcard => sme
      case Cat(Lit(s0), rest) =>
        if (str.startsWith(s0)) matches(rest, str.drop(s0.length))
        else None
      case Cat(Var(n), rest) =>
        matchEnd(rest, str) match {
          case None => None
          case Some((offset, m1)) =>
            if (m1.contains(n)) Some(m1)
            else {
              val s0 = str.substring(0, offset)
              Some(m1.updated(n, s0))
            }
        }
      case Cat(Wildcard, rest) =>
        matchEnd(rest, str).map(_._2)
    }

  // return the offset in str of the first match of p
  def matchEnd(p: Pattern, str: String): Option[(Int, Map[String, String])] =
    p match {
      case Lit(s0) =>
        if (str.endsWith(s0)) Some((str.length - s0.length, Map.empty))
        else None
      case Var(n) => Some((0, Map(n -> str)))
      case Wildcard => Some((0, Map.empty))
      case Cat(Lit(""), rest) => matchEnd(rest, str)
      case Cat(Lit(s0), rest) =>
        val idx0 = str.indexOf(s0)
        if (idx0 < 0) None
        else {
          // there is at least one match
          val nextStr = str.drop(idx0 + s0.length)
          matches(rest, nextStr) match {
            case Some(res) => Some((idx0, res))
            case None =>
              // if r1 is no good, try searching at idx0 + 1
              val nextIdx = idx0 + 1
              if (nextIdx < str.length) {
                val str1 = str.substring(idx0 + 1)
                matchEnd(p, str1).map { case (idx, m) => (idx + idx0 + 1, m) }
              }
              else None
            }
        }
      case Cat(Var(n), rest) =>
        matchEnd(rest, str).map { case res@(idx, m) =>
          if (m.contains(n)) res
          else (0, m.updated(n, str.substring(0, idx)))
        }
      case Cat(Wildcard, rest) =>
        matchEnd(rest, str).map { case (_, m) => (0, m) }
    }
}
