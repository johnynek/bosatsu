package org.bykn.bosatsu

object SimpleStringPattern {
  sealed trait Pattern {
    def matchesAny: Boolean =
      this match {
        case Var(_) | Wildcard => true
        case Lit(_) => false
        case Cat(h, t) =>
          if (h.isEmpty) t.matchesAny
          else if (t.isEmpty) h.matchesAny
          else (h.matchesAny && t.matchesAny)
      }

    def isEmpty: Boolean =
      this match {
        case Lit(s) => s.isEmpty
        case Var(_) | Wildcard => false
        case Cat(h, t) => h.isEmpty && t.isEmpty
      }

    def isLit: Boolean =
      this match {
        case Lit(_) => true
        case _ => false
      }

    @annotation.tailrec
    final def rightMost: Pattern1 =
      this match {
        case Cat(_, tail) => tail.rightMost
        case p: Pattern1 => p
      }

    /**
     * Concat that Pattern on the right
     */
    def +(that: Pattern): Pattern =
      Pattern.fromList(toList ::: that.toList)

    def reverse: Pattern =
      Pattern.fromList(toList.map {
        case Lit(s) => Lit(s.reverse)
        case notLit => notLit
      }
      .reverse)

    def prependWild: Pattern =
      this match {
        case Wildcard | Var(_) | Cat(Wildcard | Var(_), _) => this
        case notAlreadyWild => Cat(Wildcard, notAlreadyWild)
      }

    def appendString(str: String): Pattern =
      if (str == "") this
      else {
        Pattern.fromList(toList :+ Lit(str))
      }

    def matches(str: String): Option[Map[String, String]] =
      SimpleStringPattern.matches(this, str)

    def toList: List[Pattern1] =
      this match {
        case Lit(s) if s.isEmpty => Nil
        case p1: Pattern1 => p1 :: Nil
        case Cat(h, tail) =>
          val tailList = tail.toList
          h.toList match {
            case (l@Lit(s1)) :: Nil =>
              tailList match {
                case Lit(s2) :: tail =>
                  Lit(s1 ++ s2) :: tail
                case tl =>
                  l :: tl
              }
            case other =>
              other ::: tailList
          }
      }

    def onlyMatchesEmpty: Boolean =
      this match {
        case Lit("") => true
        case Lit(_) | Var(_) | Wildcard => false
        case Cat(h, t) => h.onlyMatchesEmpty && t.onlyMatchesEmpty
      }

    def doesMatch(str: String): Boolean =
      matches(str).isDefined

    /**
     * If two vars are adjacent, the left one will always match empty string
     * this normalize just removes the left var
     *
     * combine adjacent strings
     */
    def normalize: Pattern = {
      @annotation.tailrec
      def loop(current: List[Pattern1], front: List[Pattern1]): List[Pattern1] =
        current match {
          case Nil => front.reverse
          case (chead@(Var(_) | Wildcard)) :: ctail =>
            val ftail = front match {
              case (Var(_) | Wildcard) :: ftail =>
                // drop the left, push this on
                ftail
              case other => other
            }
            loop(ctail, chead :: ftail)
          case chead :: ctail =>
            // tolist combines Lit values already
            loop(ctail, chead :: front)
        }

      Pattern.fromList(loop(toList, Nil))
    }

    def render(vars: Map[String, String]): Option[String] =
      this match {
        case Lit(str) => Some(str)
        case Var(x) => vars.get(x)
        case Wildcard => None
        case Cat(head, rest) =>
          for {
            hstr <- head.render(vars)
            tstr <- rest.render(vars)
          } yield hstr ++ tstr
      }

    // replace all Vars with Wildcard
    def unname: Pattern =
      this match {
        case lw@(Lit(_) | Wildcard) => lw
        case Var(_) => Wildcard
        case Cat(Var(_), tail) =>
          tail.unname.prependWild
        case Cat(h, tail) =>
          Cat(h, tail.unname)
      }

    def intersection(that: Pattern): List[Pattern] =
      SimpleStringPattern.intersection(this, that)

    def difference(that: Pattern): List[Pattern] =
      SimpleStringPattern.difference(this, that)
  }

  sealed trait Pattern1 extends Pattern

  case class Lit(str: String) extends Pattern1
  case class Var(name: String) extends Pattern1
  case object Wildcard extends Pattern1
  case class Cat(head: Pattern1, tail: Pattern) extends Pattern

  /**
   * Compute a list of patterns that matches both patterns exactly
   */
  def intersection(p1: Pattern, p2: Pattern): List[Pattern] =
    (p1, p2) match {
      case (lit@Lit(s), _) =>
        if (p2.doesMatch(s)) lit :: Nil
        else Nil
      case (_, lit@Lit(s)) =>
        if (p1.doesMatch(s)) lit :: Nil
        else Nil
      case (Var(_) | Wildcard, Var(_) | Wildcard) => Wildcard :: Nil
      case (Var(_) | Wildcard, that) => that :: Nil
      case (that, Var(_) | Wildcard) => that :: Nil
      case (Cat(lit@Lit(s1), t1), Cat(Lit(s2), t2)) =>
        if (s1.isEmpty) intersection(t1, p2)
        else if (s2.isEmpty) intersection(p1, t2)
        else {
          val sizecase = s1.length.compareTo(s2.length)
          if (sizecase == 0) {
            if (s1 == s2) intersection(t1, t2).map(Cat(lit, _))
            else Nil
          }
          else if (sizecase > 0) {
            // s1 is longer
            val newS1 = s1.take(s2.length)
            if (newS1 == s2) {
              val s1tail = s1.drop(s2.length)
              intersection(Cat(Lit(s1tail), t1), t2)
                .map(Cat(Lit(newS1), _))
            }
            else Nil
          }
          else intersection(p2, p1)
        }
      case (Cat(Var(_) | Wildcard, t1@Cat(Var(_) | Wildcard, _)), _)=>
        // intersections can create non-normal patterns which
        // blow up exponentially, so we need to do this normalization
        // as we work
        intersection(t1, p2)
      case (_, Cat(Var(_) | Wildcard, t2@Cat(Var(_) | Wildcard, _)))=>
        // intersections can create non-normal patterns which
        // blow up exponentially, so we need to do this normalization
        // as we work
        intersection(p1, t2)
      case (Cat(Lit(s1), t1), Cat(Var(_) | Wildcard, t2)) =>
        if (s1.isEmpty) intersection(t1, p2)
        else {
          // split off a single character
          val c1 = Lit(s1.head.toString)
          val r1 = Cat(Lit(s1.tail), t1)
          val left = intersection(p1, t2)
          val right = intersection(r1, p2).map(Cat(c1, _))
          left ::: right
        }
      case (Cat(Var(_) | Wildcard, _), Cat(Lit(_), _)) =>
        // we handled this above
        intersection(p2, p1)
      case (Cat(Var(_) | Wildcard, t1), Cat(Var(_) | Wildcard, t2)) =>
        val t1norm = t1.unname.normalize
        if (t1norm == t2.unname.normalize) (t1norm.prependWild :: Nil)
        else {
          val left = intersection(t1, p2)
          val right = intersection(t2, p1)
          (left ::: right).map(_.prependWild)
        }
    }

  /**
   * return the patterns that match p1 but not p2
   *
   * For fixed sets A, B if we have (A1 x B1) - (A2 x B2) =
   * A1 = (A1 n A2) u (A1 - A2)
   * A2 = (A1 n A2) u (A2 - A1)
   * so we can decompose:
   *
   * A1 x B1 = (A1 n A2)xB1 u (A1 - A2)xB1
   * A2 x B2 = (A1 n A2)xB2 u (A2 - A1)xB2
   *
   * the difference is:
   * (A1 n A2)x(B1 - B2) u (A1 - A2)xB1
   *
   * The last challenge is we need to operate on
   * single characters, so we need to expand
   * wild into [*] = [] | [_, *], since our pattern
   * language doesn't have a symbol for
   * a single character match we have to be a bit more careful
   *
   * also, we can't exactly represent Wildcard - Lit
   * so this is actually an upperbound on the difference
   * which is to say, all the returned patterns match p1,
   * but some of them also match p2
   */
  def difference(p1: Pattern, p2: Pattern): List[Pattern] =
    (p1, p2) match {
      case (Lit(s1), Lit(s2)) => if (s1 == s2) Nil else p1 :: Nil
      case (_, Var(_) | Wildcard) => Nil
      case (Var(_) | Wildcard, _) =>
        if (p2.matchesAny) Nil
        else p1 :: Nil
      case (Lit(l), c@Cat(_, _)) => if (c.doesMatch(l)) Nil else p1 :: Nil
      case (Cat(Var(_) | Wildcard, _), Lit(_)) =>
        // this is an upper bound, we can't represent ${_} - "foo"
        p1 :: Nil
      case (Cat(Lit(""), t), _) => difference(t, p2)
      case (_, Cat(Lit(""), t)) => difference(p1, t)
      case (Cat(_, _), lit2@Lit(s2)) =>
        if (!p1.doesMatch(s2)) p1 :: Nil
        else difference(p1, Cat(lit2, Pattern.Empty))
      case (Cat(lit1@Lit(s1), t1), Cat(lit2@Lit(s2), t2)) =>
        val sizecase = s1.length.compareTo(s2.length)
        if (sizecase == 0) {
          if (s1 == s2) difference(t1, t2).map(Cat(lit1, _))
          else p1 :: Nil
        }
        else if (sizecase > 0) {
          // s1 is longer
          val newS1 = s1.take(s2.length)
          if (newS1 == s2) {
            val s1tail = s1.drop(s2.length)
            difference(Cat(Lit(s1tail), t1), t2).map(Cat(Lit(s2), _))
          }
          else p1 :: Nil
        }
        else {
          // s2 is longer
          val newS2 = s2.take(s1.length)
          if (newS2 == s1) {
            val s2tail = s2.drop(s1.length)
            difference(t1, Cat(Lit(s2tail), t2)).map(Cat(Lit(s1), _))
          }
          else p1 :: Nil
        }
      case (Cat(_, t1), Cat(_, t2)) =>
        // at least one of these patterns starts with Wildcard | Var(_)
        if (p2.matchesAny) Nil
        else {
          p1.normalize match {
            case Lit(s) if p2.doesMatch(s) => Nil
            case n1 if n1.unname == p2.normalize.unname => Nil
            case _ =>
              if (t1.rightMost.isLit && t2.rightMost.isLit) {
                // we can possibly make progress on the right
                difference(p1.reverse, p2.reverse).map(_.reverse)
              }
              else {
                // there is a var on both the front and back of both
                // todo tighten
                p1 :: Nil
              }
          }
        }
    }

  object Pattern {
    val Empty: Pattern1 = Lit("")

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
        case other =>
          // $COVERAGE-OFF$
          sys.error(s"could not parse: $other")
          // $COVERAGE-ON$
      }
    }

    def fromList(ps: List[Pattern1]): Pattern =
      ps match {
        case h :: tail =>
          if (tail.isEmpty) h
          else Cat(h, fromList(tail))
        case Nil => Lit("")
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
