package dev.bosatsu.tool

import cats.data.NonEmptyList
import cats.parse.{Parser => P}
import cats.syntax.all._
import dev.bosatsu.edn.{Edn, EdnCodec}
import dev.bosatsu.rankn.{
  ConstructorFn,
  ConstructorParam,
  DefinedType,
  RefSpace,
  Type,
  TypeEnv
}
import dev.bosatsu.Pattern.{ListPart, StrPart}
import dev.bosatsu.Identifier.{Bindable, Constructor}
import dev.bosatsu.{
  ExportedName,
  Import,
  ImportMap,
  ImportedName,
  Kind,
  Lit,
  Package,
  PackageName,
  Pattern,
  RecursionKind,
  Referant,
  TypedExpr,
  TypeName,
  Identifier,
  Variance,
  Program
}
import org.typelevel.paiges.Doc

object ShowEdn {
  type ErrorOr[A] = Either[String, A]

  import Edn.*

  private def sym(name: String): Edn = ESymbol(name)
  private def kw(name: String): Edn = EKeyword(name)
  private def str(value: String): Edn = EString(value)
  private val reservedSymbols: Set[String] =
    Set("nil", "true", "false")
  private def isSafeSymbolToken(value: String): Boolean =
    value.nonEmpty && {
      val slashCount = value.count(_ == '/')
      val slashOk =
        (slashCount == 0) ||
          ((slashCount == 1) && !value.startsWith("/") && !value.endsWith("/"))
      slashOk
    } &&
      !reservedSymbols(value) &&
      value.forall { ch =>
        !ch.isWhitespace &&
          (ch != ':') &&
          !",()[]{}\";`".contains(ch)
      }
  private def nameAtom(value: String): Edn =
    if (isSafeSymbolToken(value)) sym(value) else str(value)

  private def err[A](message: String): ErrorOr[A] =
    Left(message)

  private def rendered(edn: Edn): String =
    Edn.toDoc(edn).render(100)

  private def asVector(edn: Edn): ErrorOr[List[Edn]] =
    edn match {
      case EVector(items) => Right(items)
      case other          => err(s"expected vector, found: ${rendered(other)}")
    }

  private def asKeyword(edn: Edn): ErrorOr[String] =
    edn match {
      case EKeyword(v) => Right(v)
      case other       => err(s"expected keyword, found: ${rendered(other)}")
    }

  private def asStringAtom(edn: Edn): ErrorOr[String] =
    edn match {
      case EString(v) => Right(v)
      case ESymbol(v) => Right(v)
      case other      => err(s"expected symbol/string, found: ${rendered(other)}")
    }

  private def decodeKeywordArgs(items: List[Edn]): ErrorOr[Map[String, Edn]] = {
    @annotation.tailrec
    def loop(
        rest: List[Edn],
        acc: Map[String, Edn]
    ): ErrorOr[Map[String, Edn]] =
      rest match {
        case Nil => Right(acc)
        case key :: value :: tail =>
          asKeyword(key) match {
            case Left(e) => Left(e)
            case Right(k) =>
              loop(tail, acc.updated(k, value))
          }
        case _ => err("expected keyword/value pairs")
      }

    loop(items, Map.empty)
  }

  private def requiredField(
      args: Map[String, Edn],
      field: String
  ): ErrorOr[Edn] =
    args.get(field) match {
      case Some(v) => Right(v)
      case None    => err(s"missing required field :$field")
    }

  private def optionalField(
      args: Map[String, Edn],
      field: String
  ): Option[Edn] =
    args.get(field)

  private def parseAllWith[A](label: String, input: String, p: P[A]): ErrorOr[A] =
    p.parseAll(input).leftMap(e => s"failed to parse $label: $input, err: $e")

  private def decodePackageName(edn: Edn): ErrorOr[PackageName] =
    asStringAtom(edn).flatMap(s => parseAllWith("package", s, PackageName.parser))

  private def decodeIdentifier(edn: Edn): ErrorOr[Identifier] =
    asStringAtom(edn).flatMap(s =>
      parseAllWith("identifier", s, Identifier.parser)
    )

  private def decodeBindable(edn: Edn): ErrorOr[Bindable] =
    asStringAtom(edn).flatMap(s =>
      parseAllWith("bindable identifier", s, Identifier.bindableParser)
    )

  private def decodeConstructor(edn: Edn): ErrorOr[Constructor] =
    asStringAtom(edn).flatMap(s =>
      parseAllWith("constructor", s, Identifier.consParser)
    )

  private def decodeTypeName(edn: Edn): ErrorOr[TypeName] =
    decodeConstructor(edn).map(TypeName(_))

  private def encodeVariance(v: Variance): Edn =
    kw(
      v match {
        case Variance.Covariant     => "co"
        case Variance.Contravariant => "contra"
        case Variance.Invariant     => "in"
        case Variance.Phantom       => "phantom"
      }
    )

  private def decodeVariance(edn: Edn): ErrorOr[Variance] =
    asKeyword(edn).flatMap {
      case "co"      => Right(Variance.co)
      case "contra"  => Right(Variance.contra)
      case "in"      => Right(Variance.in)
      case "phantom" => Right(Variance.phantom)
      case other     => err(s"unknown variance keyword: $other")
    }

  private def kindToString(k: Kind): String =
    Kind.toDoc(k).render(100)

  private def encodeKind(k: Kind): Edn =
    str(kindToString(k))

  private def decodeKind(edn: Edn): ErrorOr[Kind] =
    asStringAtom(edn).flatMap(s => parseAllWith("kind", s, Kind.parser))

  private def encodeKindBinder(item: (Type.Var.Bound, Kind)): Edn = {
    val (tv, kind) = item
    if (kind.isType) nameAtom(tv.name)
    else EVector(List(nameAtom(tv.name), encodeKind(kind)))
  }

  private def decodeKindBinder(edn: Edn): ErrorOr[(Type.Var.Bound, Kind)] =
    edn match {
      case ESymbol(name) => Right((Type.Var.Bound(name), Kind.Type))
      case EString(name) => Right((Type.Var.Bound(name), Kind.Type))
      case EVector(List(nameEdn, kindEdn)) =>
        (asStringAtom(nameEdn), decodeKind(kindEdn)).mapN { (nm, k) =>
          (Type.Var.Bound(nm), k)
        }
      case other =>
        err(s"invalid type binder: ${rendered(other)}")
    }

  private def encodeKindArgBinder(item: (Type.Var.Bound, Kind.Arg)): Edn = {
    val (tv, arg) = item
    if ((arg.variance == Variance.in) && arg.kind.isType) nameAtom(tv.name)
    else EVector(List(nameAtom(tv.name), encodeVariance(arg.variance), encodeKind(arg.kind)))
  }

  private def decodeKindArgBinder(edn: Edn): ErrorOr[(Type.Var.Bound, Kind.Arg)] =
    edn match {
      case ESymbol(name) => Right((Type.Var.Bound(name), Kind.Type.in))
      case EString(name) => Right((Type.Var.Bound(name), Kind.Type.in))
      case EVector(List(nameEdn, varianceEdn, kindEdn)) =>
        (asStringAtom(nameEdn), decodeVariance(varianceEdn), decodeKind(kindEdn))
          .mapN { (nm, variance, kind) =>
            (Type.Var.Bound(nm), Kind.Arg(variance, kind))
          }
      case other =>
        err(s"invalid kind arg binder: ${rendered(other)}")
    }

  private val typeConstSeparator = "::"

  private def typeConstSymbol(p: PackageName, t: TypeName): String =
    s"${p.asString}$typeConstSeparator${t.asString}"

  private def parseConstSymbolWith(
      symValue: String,
      separator: String
  ): Option[Type.Const.Defined] = {
    val idx = symValue.lastIndexOf(separator)
    if ((idx <= 0) || (idx >= (symValue.length - separator.length))) None
    else {
      val pkgStr = symValue.substring(0, idx)
      val typeStr = symValue.substring(idx + separator.length)
      (PackageName.parser.parseAll(pkgStr).toOption, Identifier.consParser
        .parseAll(typeStr)
        .toOption).mapN { (p, c) =>
        Type.Const.Defined(p, TypeName(c))
      }
    }
  }

  private def parseConstSymbol(symValue: String): Option[Type.Const.Defined] =
    parseConstSymbolWith(symValue, typeConstSeparator)
      .orElse(parseConstSymbolWith(symValue, "/"))

  private def encodeTypeLeaf(t: Type): Edn =
    t match {
      case Type.TyConst(Type.Const.Defined(p, n)) =>
        str(typeConstSymbol(p, n))
      case Type.TyVar(Type.Var.Bound(name)) =>
        nameAtom(name)
      case Type.TyVar(Type.Var.Skolem(name, kind, existential, id)) =>
        EList(
          List(
            sym("skolem"),
            nameAtom(name),
            encodeKind(kind),
            EBool(existential),
            str(id.toString)
          )
        )
      case Type.TyMeta(Type.Meta(kind, id, existential, _)) =>
        EList(
          List(
            sym("meta"),
            encodeKind(kind),
            EBool(existential),
            str(id.toString)
          )
        )
      case other =>
        str(Type.fullyResolvedDocument.document(other).render(100))
    }

  private def encodeTypeRho(t: Type.Rho): Edn = {
    val (fn, args) = Type.unapplyAll(t)
    val fnEdn = encodeTypeLeaf(fn)
    if (args.isEmpty) fnEdn
    else EList(fnEdn :: args.map(encodeType))
  }

  def encodeType(t: Type): Edn =
    t match {
      case Type.ForAll(vars, in) =>
        EList(
          List(
            sym("forall"),
            EVector(vars.toList.map(encodeKindBinder)),
            encodeType(in)
          )
        )
      case Type.Exists(vars, in) =>
        EList(
          List(
            sym("exists"),
            EVector(vars.toList.map(encodeKindBinder)),
            encodeType(in)
          )
        )
      case rho: Type.Rho =>
        encodeTypeRho(rho)
    }

  private def decodeTypeLeaf(edn: Edn): ErrorOr[Type] =
    edn match {
      case ESymbol(s) =>
        parseConstSymbol(s) match {
          case Some(c) => Right(Type.TyConst(c))
          case None    => Right(Type.TyVar(Type.Var.Bound(s)))
        }
      case EString(s) =>
        parseConstSymbol(s) match {
          case Some(c) => Right(Type.TyConst(c))
          case None    => Right(Type.TyVar(Type.Var.Bound(s)))
        }
      case EList(ESymbol("skolem") :: nameEdn :: kindEdn :: EBool(ex) :: idEdn :: Nil) =>
        (asStringAtom(nameEdn), decodeKind(kindEdn), asStringAtom(idEdn))
          .tupled
          .flatMap { case (name, kind, idStr) =>
            Either
              .catchNonFatal(idStr.toLong)
              .leftMap(_ => s"invalid skolem id: $idStr")
              .map { id =>
                Type.TyVar(Type.Var.Skolem(name, kind, ex, id))
              }
          }
      case EList(ESymbol("meta") :: kindEdn :: EBool(ex) :: idEdn :: Nil) =>
        (decodeKind(kindEdn), asStringAtom(idEdn)).tupled.flatMap {
          case (kind, idStr) =>
            Either
              .catchNonFatal(idStr.toLong)
              .leftMap(_ => s"invalid meta id: $idStr")
              .map { id =>
                Type.TyMeta(
                  Type.Meta(kind, id, ex, RefSpace.constRef(Option.empty))
                )
              }
        }
      case other =>
        err(s"invalid type leaf: ${rendered(other)}")
    }

  def decodeType(edn: Edn): ErrorOr[Type] =
    edn match {
      case EList(ESymbol("forall") :: bindersEdn :: onEdn :: Nil) =>
        for {
          binders <- asVector(bindersEdn)
          vars <- binders.traverse(decodeKindBinder)
          on <- decodeType(onEdn)
        } yield Type.forAll(vars, on)
      case EList(ESymbol("exists") :: bindersEdn :: onEdn :: Nil) =>
        for {
          binders <- asVector(bindersEdn)
          vars <- binders.traverse(decodeKindBinder)
          on <- decodeType(onEdn)
        } yield Type.exists(vars, on)
      case EList(head :: args) =>
        for {
          fn <- decodeTypeLeaf(head)
          decodedArgs <- args.traverse(decodeType)
        } yield Type.applyAll(fn, decodedArgs)
      case _ =>
        decodeTypeLeaf(edn)
    }

  given typeCodec: EdnCodec[Type] with {
    def encode(a: Type): Edn = encodeType(a)
    def decode(edn: Edn): ErrorOr[Type] = decodeType(edn)
  }

  private def encodeLit(lit: Lit): Edn =
    lit match {
      case Lit.Integer(i) =>
        EList(List(sym("int"), str(i.toString)))
      case Lit.Str(s) =>
        EList(List(sym("str"), str(s)))
      case c @ Lit.Chr(_) =>
        EList(List(sym("char"), str(c.asStr)))
      case f: Lit.Float64 =>
        EList(List(sym("f64"), str(Lit.Float64.toLiteralString(f))))
    }

  private def decodeLit(edn: Edn): ErrorOr[Lit] =
    edn match {
      case EList(ESymbol("int") :: valueEdn :: Nil) =>
        asStringAtom(valueEdn).flatMap { s =>
          Either
            .catchNonFatal(new java.math.BigInteger(s))
            .leftMap(_ => s"invalid integer literal: $s")
            .map(Lit.Integer(_))
        }
      case EList(ESymbol("str") :: EString(value) :: Nil) =>
        Right(Lit.Str(value))
      case EList(ESymbol("char") :: EString(value) :: Nil) =>
        if (value.codePointCount(0, value.length) == 1)
          Right(Lit.Chr.fromCodePoint(value.codePointAt(0)))
        else err(s"char literal must have one codepoint: $value")
      case EList(ESymbol("f64") :: valueEdn :: Nil) =>
        asStringAtom(valueEdn).flatMap(s =>
          parseAllWith("float64 literal", s, Lit.float64Parser).map(identity)
        )
      case other =>
        err(s"invalid literal encoding: ${rendered(other)}")
    }

  private def encodePattern(
      pat: Pattern[(PackageName, Constructor), Type]
  ): Edn =
    pat match {
      case Pattern.WildCard =>
        EList(List(sym("wild")))
      case Pattern.Literal(lit) =>
        EList(List(sym("plit"), encodeLit(lit)))
      case Pattern.Var(name) =>
        EList(List(sym("pvar"), nameAtom(name.sourceCodeRepr)))
      case Pattern.Named(name, p) =>
        EList(List(sym("pnamed"), nameAtom(name.sourceCodeRepr), encodePattern(p)))
      case Pattern.StrPat(parts) =>
        EList(
          List(
            sym("pstr"),
            EVector(parts.toList.map {
              case StrPart.WildStr      => kw("wild-str")
              case StrPart.WildChar     => kw("wild-char")
              case StrPart.NamedStr(n)  =>
                EList(List(sym("named-str"), nameAtom(n.sourceCodeRepr)))
              case StrPart.NamedChar(n) =>
                EList(List(sym("named-char"), nameAtom(n.sourceCodeRepr)))
              case StrPart.LitStr(s)    =>
                EList(List(sym("lit-str"), str(s)))
            })
          )
        )
      case Pattern.ListPat(parts) =>
        EList(
          List(
            sym("plist"),
            EVector(parts.map {
              case ListPart.WildList      => kw("wild-list")
              case ListPart.NamedList(n)  =>
                EList(List(sym("named-list"), nameAtom(n.sourceCodeRepr)))
              case ListPart.Item(pat)     =>
                EList(List(sym("item"), encodePattern(pat)))
            })
          )
        )
      case Pattern.Annotation(pattern, tpe) =>
        EList(List(sym("pann"), encodePattern(pattern), encodeType(tpe)))
      case Pattern.PositionalStruct((pack, cons), params) =>
        EList(
          List(
            sym("pstruct"),
            nameAtom(s"${pack.asString}/${cons.asString}"),
            EVector(params.map(encodePattern))
          )
        )
      case Pattern.Union(head, rest) =>
        EList(
          List(
            sym("punion"),
            EVector((head :: rest.toList).map(encodePattern))
          )
        )
    }

  private def decodePattern(
      edn: Edn
  ): ErrorOr[Pattern[(PackageName, Constructor), Type]] =
    edn match {
      case EList(ESymbol("wild") :: Nil) =>
        Right(Pattern.WildCard)
      case EList(ESymbol("wildcard") :: Nil) =>
        Right(Pattern.WildCard)
      case EList(ESymbol("plit") :: litEdn :: Nil) =>
        decodeLit(litEdn).map(Pattern.Literal(_))
      case EList(ESymbol("pvar") :: nameEdn :: Nil) =>
        decodeBindable(nameEdn).map(Pattern.Var(_))
      case EList(ESymbol("pnamed") :: nameEdn :: patEdn :: Nil) =>
        (decodeBindable(nameEdn), decodePattern(patEdn)).mapN(Pattern.Named(_, _))
      case EList(ESymbol("pstr") :: partsEdn :: Nil) =>
        for {
          partsRaw <- asVector(partsEdn)
          parts <- partsRaw.traverse {
            case EKeyword("wild-str") =>
              Right(StrPart.WildStr)
            case EKeyword("wild-char") =>
              Right(StrPart.WildChar)
            case EList(ESymbol("named-str") :: nameEdn :: Nil) =>
              decodeBindable(nameEdn).map(StrPart.NamedStr(_))
            case EList(ESymbol("named-char") :: nameEdn :: Nil) =>
              decodeBindable(nameEdn).map(StrPart.NamedChar(_))
            case EList(ESymbol("lit-str") :: EString(value) :: Nil) =>
              Right(StrPart.LitStr(value))
            case other =>
              err[StrPart](s"invalid string pattern part: ${rendered(other)}")
          }
          nel <- NonEmptyList
            .fromList(parts)
            .toRight("string pattern cannot be empty")
        } yield Pattern.StrPat(nel)
      case EList(ESymbol("plist") :: partsEdn :: Nil) =>
        for {
          partsRaw <- asVector(partsEdn)
          parts <- partsRaw.traverse {
            case EKeyword("wild-list") =>
              Right(ListPart.WildList)
            case EList(ESymbol("named-list") :: nameEdn :: Nil) =>
              decodeBindable(nameEdn).map(ListPart.NamedList(_))
            case EList(ESymbol("item") :: itemEdn :: Nil) =>
              decodePattern(itemEdn).map(ListPart.Item(_))
            case other =>
              err[ListPart[Pattern[(PackageName, Constructor), Type]]](
                s"invalid list pattern part: ${rendered(other)}"
              )
          }
        } yield Pattern.ListPat(parts)
      case EList(ESymbol("pann") :: patEdn :: typeEdn :: Nil) =>
        (decodePattern(patEdn), decodeType(typeEdn)).mapN(Pattern.Annotation(_, _))
      case EList(ESymbol("pstruct") :: fqnEdn :: paramsEdn :: Nil) =>
        for {
          fqn <- asStringAtom(fqnEdn)
          idx <- {
            val last = fqn.lastIndexOf('/')
            if ((last <= 0) || (last >= (fqn.length - 1)))
              err[Int](s"invalid constructor reference: $fqn")
            else Right(last)
          }
          pack <- parseAllWith("package name", fqn.substring(0, idx), PackageName.parser)
          cons <- parseAllWith("constructor", fqn.substring(idx + 1), Identifier.consParser)
          paramsRaw <- asVector(paramsEdn)
          params <- paramsRaw.traverse(decodePattern)
        } yield Pattern.PositionalStruct((pack, cons), params)
      case EList(ESymbol("punion") :: patsEdn :: Nil) =>
        for {
          patsRaw <- asVector(patsEdn)
          pats <- patsRaw.traverse(decodePattern)
          head <- pats.headOption.toRight("union requires at least two patterns")
          tail = pats.drop(1)
          _ <- Either.cond(tail.nonEmpty, (), "union requires at least two patterns")
        } yield Pattern.union(head, tail)
      case other =>
        err(s"invalid pattern: ${rendered(other)}")
    }

  given patternCodec: EdnCodec[Pattern[(PackageName, Constructor), Type]] with {
    def encode(a: Pattern[(PackageName, Constructor), Type]): Edn =
      encodePattern(a)
    def decode(edn: Edn): ErrorOr[Pattern[(PackageName, Constructor), Type]] =
      decodePattern(edn)
  }

  private def encodeQuant(q: TypedExpr.Quantification): Edn =
    EList(
      List(
        sym("quant"),
        EVector(q.forallList.map(encodeKindBinder)),
        EVector(q.existList.map(encodeKindBinder))
      )
    )

  private def decodeQuant(edn: Edn): ErrorOr[TypedExpr.Quantification] =
    edn match {
      case EList(ESymbol("quant") :: forallEdn :: existsEdn :: Nil) =>
        for {
          forallRaw <- asVector(forallEdn)
          existsRaw <- asVector(existsEdn)
          foralls <- forallRaw.traverse(decodeKindBinder)
          exists <- existsRaw.traverse(decodeKindBinder)
          quant <- TypedExpr.Quantification
            .fromLists(foralls, exists)
            .toRight("quantification cannot be empty")
        } yield quant
      case other =>
        err(s"invalid quantification: ${rendered(other)}")
    }

  private def encodeTypedExpr(te: TypedExpr[Unit]): Edn =
    te match {
      case TypedExpr.Generic(quant, in) =>
        EList(List(sym("generic"), encodeQuant(quant), encodeTypedExpr(in)))
      case TypedExpr.Annotation(term, coerce) =>
        EList(List(sym("ann"), encodeType(coerce), encodeTypedExpr(term)))
      case TypedExpr.AnnotatedLambda(args, expr, ()) =>
        EList(
          List(
            sym("lambda"),
            EVector(args.toList.map { case (name, tpe) =>
              EVector(List(nameAtom(name.sourceCodeRepr), encodeType(tpe)))
            }),
            encodeTypedExpr(expr)
          )
        )
      case TypedExpr.Local(name, tpe, ()) =>
        EList(List(sym("local"), nameAtom(name.sourceCodeRepr), encodeType(tpe)))
      case TypedExpr.Global(pack, name, tpe, ()) =>
        EList(
          List(
            sym("global"),
            nameAtom(pack.asString),
            nameAtom(name.sourceCodeRepr),
            encodeType(tpe)
          )
        )
      case TypedExpr.App(fn, args, result, ()) =>
        EList(
          List(
            sym("app"),
            encodeTypedExpr(fn),
            EVector(args.toList.map(encodeTypedExpr)),
            encodeType(result)
          )
        )
      case TypedExpr.Let(arg, expr, in, rec, ()) =>
        EList(
          List(
            sym(if (rec.isRecursive) "letrec" else "let"),
            nameAtom(arg.sourceCodeRepr),
            encodeTypedExpr(expr),
            encodeTypedExpr(in)
          )
        )
      case TypedExpr.Loop(args, body, ()) =>
        EList(
          List(
            sym("loop"),
            EVector(args.toList.map { case (name, rhs) =>
              EVector(List(nameAtom(name.sourceCodeRepr), encodeTypedExpr(rhs)))
            }),
            encodeTypedExpr(body)
          )
        )
      case TypedExpr.Recur(args, tpe, ()) =>
        EList(
          List(
            sym("recur"),
            EVector(args.toList.map(encodeTypedExpr)),
            encodeType(tpe)
          )
        )
      case TypedExpr.Literal(lit, tpe, ()) =>
        EList(List(sym("lit"), encodeLit(lit), encodeType(tpe)))
      case TypedExpr.Match(arg, branches, ()) =>
        EList(
          List(
            sym("match"),
            encodeTypedExpr(arg),
            EVector(branches.toList.map { branch =>
              branch.guard match {
                case None =>
                  EList(
                    List(
                      sym("branch"),
                      encodePattern(branch.pattern),
                      encodeTypedExpr(branch.expr)
                    )
                  )
                case Some(g) =>
                  EList(
                    List(
                      sym("branch"),
                      encodePattern(branch.pattern),
                      encodeTypedExpr(g),
                      encodeTypedExpr(branch.expr)
                    )
                  )
              }
            })
          )
        )
    }

  private def decodeTypedExpr(edn: Edn): ErrorOr[TypedExpr[Unit]] =
    edn match {
      case EList(ESymbol("generic") :: quantEdn :: inEdn :: Nil) =>
        (decodeQuant(quantEdn), decodeTypedExpr(inEdn)).mapN(TypedExpr.Generic(_, _))
      case EList(ESymbol("ann") :: typeEdn :: termEdn :: Nil) =>
        (decodeType(typeEdn), decodeTypedExpr(termEdn)).mapN {
          (coerce, term) => TypedExpr.Annotation(term, coerce)
        }
      case EList(ESymbol("lambda") :: argsEdn :: bodyEdn :: Nil) =>
        for {
          argsRaw <- asVector(argsEdn)
          args <- argsRaw.traverse {
            case EVector(List(nameEdn, typeEdn)) =>
              (decodeBindable(nameEdn), decodeType(typeEdn)).tupled
            case other =>
              err[(Bindable, Type)](s"invalid lambda arg: ${rendered(other)}")
          }
          nel <- NonEmptyList.fromList(args).toRight("lambda args cannot be empty")
          body <- decodeTypedExpr(bodyEdn)
        } yield TypedExpr.AnnotatedLambda(nel, body, ())
      case EList(ESymbol("local") :: nameEdn :: typeEdn :: Nil) =>
        (decodeBindable(nameEdn), decodeType(typeEdn)).mapN(TypedExpr.Local(_, _, ()))
      case EList(ESymbol("global") :: packEdn :: nameEdn :: typeEdn :: Nil) =>
        (
          decodePackageName(packEdn),
          decodeIdentifier(nameEdn),
          decodeType(typeEdn)
        ).mapN(TypedExpr.Global(_, _, _, ()))
      case EList(ESymbol("app") :: fnEdn :: argsEdn :: resultEdn :: Nil) =>
        for {
          fn <- decodeTypedExpr(fnEdn)
          argsRaw <- asVector(argsEdn)
          args <- argsRaw.traverse(decodeTypedExpr)
          nel <- NonEmptyList.fromList(args).toRight("app args cannot be empty")
          res <- decodeType(resultEdn)
        } yield TypedExpr.App(fn, nel, res, ())
      case EList(ESymbol("let") :: nameEdn :: rhsEdn :: bodyEdn :: Nil) =>
        (decodeBindable(nameEdn), decodeTypedExpr(rhsEdn), decodeTypedExpr(bodyEdn))
          .mapN(TypedExpr.Let(_, _, _, RecursionKind.NonRecursive, ()))
      case EList(ESymbol("letrec") :: nameEdn :: rhsEdn :: bodyEdn :: Nil) =>
        (decodeBindable(nameEdn), decodeTypedExpr(rhsEdn), decodeTypedExpr(bodyEdn))
          .mapN(TypedExpr.Let(_, _, _, RecursionKind.Recursive, ()))
      case EList(ESymbol("loop") :: argsEdn :: bodyEdn :: Nil) =>
        for {
          argsRaw <- asVector(argsEdn)
          args <- argsRaw.traverse {
            case EVector(List(nameEdn, rhsEdn)) =>
              (decodeBindable(nameEdn), decodeTypedExpr(rhsEdn)).tupled
            case other =>
              err[(Bindable, TypedExpr[Unit])](
                s"invalid loop binding: ${rendered(other)}"
              )
          }
          nel <- NonEmptyList.fromList(args).toRight("loop args cannot be empty")
          body <- decodeTypedExpr(bodyEdn)
        } yield TypedExpr.Loop(nel, body, ())
      case EList(ESymbol("recur") :: argsEdn :: typeEdn :: Nil) =>
        for {
          argsRaw <- asVector(argsEdn)
          args <- argsRaw.traverse(decodeTypedExpr)
          nel <- NonEmptyList.fromList(args).toRight("recur args cannot be empty")
          tpe <- decodeType(typeEdn)
        } yield TypedExpr.Recur(nel, tpe, ())
      case EList(ESymbol("lit") :: litEdn :: typeEdn :: Nil) =>
        (decodeLit(litEdn), decodeType(typeEdn)).mapN(TypedExpr.Literal(_, _, ()))
      case EList(ESymbol("match") :: argEdn :: branchesEdn :: Nil) =>
        for {
          arg <- decodeTypedExpr(argEdn)
          branchesRaw <- asVector(branchesEdn)
          branches <- branchesRaw.traverse {
            case EList(ESymbol("branch") :: patEdn :: exprEdn :: Nil) =>
              (decodePattern(patEdn), decodeTypedExpr(exprEdn)).mapN {
                (pat, expr) => TypedExpr.Branch(pat, None, expr)
              }
            case EList(
                  ESymbol("branch") :: patEdn :: guardEdn :: exprEdn :: Nil
                ) =>
              (decodePattern(patEdn), decodeTypedExpr(guardEdn), decodeTypedExpr(exprEdn))
                .mapN { (pat, guard, expr) =>
                  TypedExpr.Branch(pat, Some(guard), expr)
                }
            case other =>
              err[TypedExpr.Branch[Unit]](s"invalid branch: ${rendered(other)}")
          }
          nel <- NonEmptyList.fromList(branches).toRight("match branches cannot be empty")
        } yield TypedExpr.Match(arg, nel, ())
      case other =>
        err(s"invalid typed expression: ${rendered(other)}")
    }

  given typedExprCodec: EdnCodec[TypedExpr[Unit]] with {
    def encode(a: TypedExpr[Unit]): Edn = encodeTypedExpr(a)
    def decode(edn: Edn): ErrorOr[TypedExpr[Unit]] = decodeTypedExpr(edn)
  }

  private def encodeConstructorFn(cf: ConstructorFn[Kind.Arg]): Edn = {
    val base = List(sym("constructor-fn"), nameAtom(cf.name.sourceCodeRepr))
    val attrs =
      List(
        if (cf.args.isEmpty) None
        else
          Some(
            kw("fields") -> EVector(cf.args.map { param =>
              val defaultPart =
                param.defaultBinding match {
                  case None =>
                    Nil
                  case Some(defaultName) =>
                    nameAtom(defaultName.sourceCodeRepr) ::
                      param.defaultType.toList.map(encodeType)
                }
              EVector(
                List(nameAtom(param.name.sourceCodeRepr), encodeType(param.tpe)) ++
                  defaultPart
              )
            })
          ),
        if (cf.exists.isEmpty) None
        else
          Some(
            kw("exists") -> EVector(cf.exists.map(encodeKindArgBinder))
          )
      ).flatten

    EList(base ++ attrs.flatMap { case (k, v) => List(k, v) })
  }

  private def decodeConstructorFn(edn: Edn): ErrorOr[ConstructorFn[Kind.Arg]] =
    edn match {
      case EList(ESymbol("constructor-fn") :: nameEdn :: rest) =>
        for {
          name <- decodeConstructor(nameEdn)
          kv <- decodeKeywordArgs(rest)
          fields <- optionalField(kv, "fields") match {
            case None => Right(Nil)
            case Some(value) =>
              asVector(value).flatMap(
                _.traverse {
                  case EVector(List(nameEdn, typeEdn)) =>
                    (decodeBindable(nameEdn), decodeType(typeEdn)).mapN {
                      ConstructorParam(_, _, None, None)
                    }
                  case EVector(List(nameEdn, typeEdn, defaultEdn)) =>
                    (
                      decodeBindable(nameEdn),
                      decodeType(typeEdn),
                      decodeBindable(defaultEdn)
                    ).mapN { (name, tpe, defaultName) =>
                      ConstructorParam(name, tpe, Some(defaultName), None)
                    }
                  case EVector(List(nameEdn, typeEdn, defaultEdn, defaultTypeEdn)) =>
                    (
                      decodeBindable(nameEdn),
                      decodeType(typeEdn),
                      decodeBindable(defaultEdn),
                      decodeType(defaultTypeEdn)
                    ).mapN { (name, tpe, defaultName, defaultType) =>
                      ConstructorParam(
                        name,
                        tpe,
                        Some(defaultName),
                        Some(defaultType)
                      )
                    }
                  case other =>
                    err[ConstructorParam](
                      s"invalid constructor field: ${rendered(other)}"
                    )
                }
              )
          }
          exists <- optionalField(kv, "exists") match {
            case None => Right(Nil)
            case Some(value) =>
              asVector(value).flatMap(_.traverse(decodeKindArgBinder))
          }
        } yield ConstructorFn(name = name, args = fields, exists = exists)
      case other =>
        err(s"invalid constructor-fn: ${rendered(other)}")
    }

  private def encodeDefinedType(dt: DefinedType[Kind.Arg]): Edn = {
    val base = List(
      sym("defined-type"),
      nameAtom(dt.packageName.asString),
      nameAtom(dt.name.asString)
    )
    val attrs =
      List(
        if (dt.annotatedTypeParams.isEmpty) None
        else
          Some(
            kw("params") -> EVector(dt.annotatedTypeParams.map(encodeKindArgBinder))
          ),
        if (dt.constructors.isEmpty) None
        else
          Some(
            kw("constructors") -> EVector(dt.constructors.map(encodeConstructorFn))
          )
      ).flatten

    EList(base ++ attrs.flatMap { case (k, v) => List(k, v) })
  }

  private def decodeDefinedType(edn: Edn): ErrorOr[DefinedType[Kind.Arg]] =
    edn match {
      case EList(ESymbol("defined-type") :: packageEdn :: nameEdn :: rest) =>
        for {
          packageName <- decodePackageName(packageEdn)
          name <- decodeTypeName(nameEdn)
          kv <- decodeKeywordArgs(rest)
          params <- optionalField(kv, "params") match {
            case None => Right(Nil)
            case Some(value) =>
              asVector(value).flatMap(_.traverse(decodeKindArgBinder))
          }
          constructors <- optionalField(kv, "constructors") match {
            case None => Right(Nil)
            case Some(value) =>
              asVector(value).flatMap(_.traverse(decodeConstructorFn))
          }
        } yield DefinedType(
          packageName = packageName,
          name = name,
          annotatedTypeParams = params,
          constructors = constructors
        )
      case other =>
        err(s"invalid defined type: ${rendered(other)}")
    }

  private def encodeReferant(ref: Referant[Kind.Arg]): Edn =
    ref match {
      case Referant.Value(tpe) =>
        EList(List(sym("ref-value"), encodeType(tpe)))
      case Referant.DefinedT(dt) =>
        EList(List(sym("ref-defined"), encodeDefinedType(dt)))
      case Referant.Constructor(dt, fn) =>
        EList(List(sym("ref-constructor"), encodeDefinedType(dt), encodeConstructorFn(fn)))
    }

  private def decodeReferant(edn: Edn): ErrorOr[Referant[Kind.Arg]] =
    edn match {
      case EList(ESymbol("ref-value") :: typeEdn :: Nil) =>
        decodeType(typeEdn).map(Referant.Value(_))
      case EList(ESymbol("ref-defined") :: dtEdn :: Nil) =>
        decodeDefinedType(dtEdn).map(Referant.DefinedT(_))
      case EList(ESymbol("ref-constructor") :: dtEdn :: fnEdn :: Nil) =>
        (decodeDefinedType(dtEdn), decodeConstructorFn(fnEdn)).mapN {
          (dt, fn) => Referant.Constructor(dt, fn)
        }
      case other =>
        err(s"invalid referant: ${rendered(other)}")
    }

  private def encodeImportedName(
      in: ImportedName[NonEmptyList[Referant[Kind.Arg]]]
  ): Edn = {
    val itemKind =
      in match {
        case ImportedName.OriginalName(_, _) => kw("original")
        case ImportedName.Renamed(_, _, _)   => kw("renamed")
      }

    EList(
      List(
        sym("item"),
        str(in.originalName.sourceCodeRepr),
        str(in.localName.sourceCodeRepr),
        EVector(in.tag.toList.map(encodeReferant)),
        kw("kind"),
        itemKind
      )
    )
  }

  private def decodeImportedName(
      edn: Edn
  ): ErrorOr[ImportedName[NonEmptyList[Referant[Kind.Arg]]]] =
    edn match {
      case EList(ESymbol("item") :: origEdn :: localEdn :: refsEdn :: rest) =>
        for {
          original <- decodeIdentifier(origEdn)
          local <- decodeIdentifier(localEdn)
          refsRaw <- asVector(refsEdn)
          refs <- refsRaw.traverse(decodeReferant)
          nel <- NonEmptyList.fromList(refs).toRight("import item referants cannot be empty")
          args <- decodeKeywordArgs(rest)
          kind <- optionalField(args, "kind") match {
            case None =>
              Right(if (original == local) "original" else "renamed")
            case Some(value) =>
              asKeyword(value).flatMap {
                case "original" => Right("original")
                case "renamed"  => Right("renamed")
                case other      => err(s"unknown import item kind: $other")
              }
          }
          res <- kind match {
            case "original" =>
              Either.cond(
                original == local,
                ImportedName.OriginalName(original, nel),
                s"import item kind :original requires matching names: ${original.sourceCodeRepr} vs ${local.sourceCodeRepr}"
              )
            case "renamed" =>
              Right(ImportedName.Renamed(original, local, nel))
            case _ =>
              err[ImportedName[NonEmptyList[Referant[Kind.Arg]]]](
                s"invalid import item kind: $kind"
              )
          }
        } yield res
      case other =>
        err(s"invalid import item: ${rendered(other)}")
    }

  private def emptyInterface(name: PackageName): Package.Interface =
    Package(name = name, imports = Nil, exports = Nil, program = ())

  private def encodeImport(
      imp: Import[Package.Interface, NonEmptyList[Referant[Kind.Arg]]]
  ): Edn =
    EList(
      List(
        sym("import"),
        str(imp.pack.name.asString),
        EVector(imp.items.toList.map(encodeImportedName))
      )
    )

  private def decodeImport(
      edn: Edn
  ): ErrorOr[Import[Package.Interface, NonEmptyList[Referant[Kind.Arg]]]] =
    edn match {
      case EList(ESymbol("import") :: packageEdn :: itemsEdn :: Nil) =>
        for {
          packageName <- decodePackageName(packageEdn)
          itemsRaw <- asVector(itemsEdn)
          decodedItems <- itemsRaw.traverse(decodeImportedName)
          nel <- NonEmptyList.fromList(decodedItems).toRight("import items cannot be empty")
        } yield Import(emptyInterface(packageName), nel)
      case other =>
        err(s"invalid import: ${rendered(other)}")
    }

  private def encodeExport(exp: ExportedName[Referant[Kind.Arg]]): Edn =
    exp match {
      case ExportedName.Binding(name, tag) =>
        EList(
          List(
            sym("export"),
            kw("binding"),
            str(name.sourceCodeRepr),
            encodeReferant(tag)
          )
        )
      case ExportedName.TypeName(name, tag) =>
        EList(
          List(
            sym("export"),
            kw("typename"),
            str(name.sourceCodeRepr),
            encodeReferant(tag)
          )
        )
      case ExportedName.Constructor(name, tag) =>
        EList(
          List(
            sym("export"),
            kw("constructor"),
            str(name.sourceCodeRepr),
            encodeReferant(tag)
          )
        )
    }

  private def decodeExport(edn: Edn): ErrorOr[ExportedName[Referant[Kind.Arg]]] =
    edn match {
      case EList(ESymbol("export") :: kindEdn :: nameEdn :: tagEdn :: Nil) =>
        for {
          kind <- asKeyword(kindEdn)
          tag <- decodeReferant(tagEdn)
          res <- kind match {
            case "binding" =>
              decodeBindable(nameEdn).map(ExportedName.Binding(_, tag))
            case "typename" =>
              decodeConstructor(nameEdn).map(ExportedName.TypeName(_, tag))
            case "constructor" =>
              decodeConstructor(nameEdn).map(ExportedName.Constructor(_, tag))
            case other =>
              err[ExportedName[Referant[Kind.Arg]]](s"unknown export kind: $other")
          }
        } yield res
      case other =>
        err(s"invalid export: ${rendered(other)}")
    }

  private def encodeExternal(ext: (Bindable, Type)): Edn =
    EVector(List(nameAtom(ext._1.sourceCodeRepr), encodeType(ext._2)))

  private def decodeExternal(edn: Edn): ErrorOr[(Bindable, Type)] =
    edn match {
      case EVector(List(nameEdn, typeEdn)) =>
        (decodeBindable(nameEdn), decodeType(typeEdn)).tupled
      case other =>
        err(s"invalid external definition: ${rendered(other)}")
    }

  private def encodeTopLet(
      item: (Bindable, RecursionKind, TypedExpr[Unit])
  ): Edn = {
    val (name, rec, expr) = item
    EList(
      List(
        sym(if (rec.isRecursive) "defrec" else "def"),
        nameAtom(name.sourceCodeRepr),
        encodeTypedExpr(expr)
      )
    )
  }

  private def decodeTopLet(
      edn: Edn
  ): ErrorOr[(Bindable, RecursionKind, TypedExpr[Unit])] =
    edn match {
      case EList(ESymbol("def") :: nameEdn :: exprEdn :: Nil) =>
        (decodeBindable(nameEdn), decodeTypedExpr(exprEdn)).mapN {
          (name, expr) => (name, RecursionKind.NonRecursive, expr)
        }
      case EList(ESymbol("defrec") :: nameEdn :: exprEdn :: Nil) =>
        (decodeBindable(nameEdn), decodeTypedExpr(exprEdn)).mapN {
          (name, expr) => (name, RecursionKind.Recursive, expr)
        }
      case other =>
        err(s"invalid top-level def: ${rendered(other)}")
    }

  private def referantOrderKey(ref: Referant[Kind.Arg]): String =
    rendered(encodeReferant(ref))

  private def normalizeImportedName(
      item: ImportedName[NonEmptyList[Referant[Kind.Arg]]]
  ): ImportedName[NonEmptyList[Referant[Kind.Arg]]] = {
    val refs =
      item.tag.toList.distinct.sortBy(referantOrderKey)
    item.map(_ => NonEmptyList.fromListUnsafe(refs))
  }

  private def normalizeImport(
      imp: Import[Package.Interface, NonEmptyList[Referant[Kind.Arg]]]
  ): Import[Package.Interface, NonEmptyList[Referant[Kind.Arg]]] = {
    val normalizedItems =
      imp.items.toList.map(normalizeImportedName)

    val merged = normalizedItems
      .groupBy(item => (item.originalName, item.localName))
      .values
      .map { group =>
        val head = group.head
        val refs = group.toList
          .flatMap(_.tag.toList)
          .distinct
          .sortBy(referantOrderKey)
        head.map(_ => NonEmptyList.fromListUnsafe(refs))
      }
      .toList
      .sortBy(item => (item.localName.sourceCodeRepr, item.originalName.sourceCodeRepr))

    imp.copy(items = NonEmptyList.fromListUnsafe(merged))
  }

  private def exportOrder(exp: ExportedName[Referant[Kind.Arg]]): Int =
    exp match {
      case ExportedName.TypeName(_, _)    => 0
      case ExportedName.Constructor(_, _) => 1
      case ExportedName.Binding(_, _)     => 2
    }

  private def normalizeExport(
      exp: ExportedName[Referant[Kind.Arg]]
  ): ExportedName[Referant[Kind.Arg]] = {
    val refs = exp.tag match {
      case r => r
    }
    exp match {
      case ExportedName.TypeName(name, _) =>
        ExportedName.TypeName(name, refs)
      case ExportedName.Constructor(name, _) =>
        ExportedName.Constructor(name, refs)
      case ExportedName.Binding(name, _) =>
        ExportedName.Binding(name, refs)
    }
  }

  def normalizeForRoundTrip(pack: Package.Typed[Unit]): ErrorOr[Package.Typed[Unit]] = {
    val imports1 = pack.imports
      .map(imp => imp.copy(pack = emptyInterface(imp.pack.name)))
      .map(normalizeImport)
      .sortBy(_.pack.name)
    val exports1 = pack.exports
      .map(normalizeExport)
      .distinct
      .sortBy(exp =>
        (exportOrder(exp), exp.name.sourceCodeRepr, referantOrderKey(exp.tag))
      )
    val (collisions, importMap1) =
      ImportMap.fromImports(imports1)((_, _) => ImportMap.Unify.Error)
    NonEmptyList.fromList(collisions) match {
      case Some(nel) =>
        val names = nel.toList.map { case (_, in) =>
          in.localName.sourceCodeRepr
        }.distinct.sorted
        err(s"import collisions while normalizing package ${pack.name.asString}: ${names.mkString(", ")}")
      case None =>
        val prog0 = pack.program._1
        val prog1 =
          Program(
            types = prog0.types,
            lets = prog0.lets,
            externalDefs = prog0.externalDefs,
            from = ()
          )
        Right(pack.copy(imports = imports1, exports = exports1, program = (prog1, importMap1)))
    }
  }

  private def encodePackage(pack: Package.Typed[Unit]): Edn = {
    val normalized = normalizeForRoundTrip(pack).getOrElse(pack)
    val prog = normalized.program._1

    val localTypes =
      prog.types.allDefinedTypes.filter(_.packageName == normalized.name)

    val externals =
      prog.externalDefs.flatMap { name =>
        prog.types
          .getExternalValue(normalized.name, name)
          .map(name -> _)
      }

    val exportedTypes =
      normalized.exports.collect {
        case ExportedName.TypeName(name, _) => name.sourceCodeRepr
      }
    val exportedValues =
      normalized.exports.collect {
        case ExportedName.Binding(name, _)     => name.sourceCodeRepr
        case ExportedName.Constructor(name, _) => name.sourceCodeRepr
      }

    val attrs =
      List(
        Some(kw("imports") -> EVector(normalized.imports.map(encodeImport))),
        if (exportedTypes.isEmpty) None
        else Some(kw("exported-types") -> EVector(exportedTypes.map(str))),
        if (exportedValues.isEmpty) None
        else Some(kw("exported-values") -> EVector(exportedValues.map(str))),
        if (normalized.exports.isEmpty) None
        else Some(kw("exports") -> EVector(normalized.exports.map(encodeExport))),
        Some(kw("types") -> EVector(localTypes.map(encodeDefinedType))),
        Some(kw("externals") -> EVector(externals.map(encodeExternal))),
        Some(kw("defs") -> EVector(prog.lets.map(encodeTopLet)))
      ).flatten

    EList(
      List(sym("package"), kw("name"), str(normalized.name.asString)) ++
        attrs.flatMap { case (k, v) => List(k, v) }
    )
  }

  private def decodePackage(edn: Edn): ErrorOr[Package.Typed[Unit]] =
    edn match {
      case EList(ESymbol("package") :: argsRaw) =>
        for {
          args <- decodeKeywordArgs(argsRaw)
          name <- requiredField(args, "name").flatMap(decodePackageName)
          imports <- optionalField(args, "imports") match {
            case None => Right(Nil)
            case Some(value) =>
              asVector(value).flatMap(_.traverse(decodeImport))
          }
          exports <- optionalField(args, "exports") match {
            case None => Right(Nil)
            case Some(value) =>
              asVector(value).flatMap(_.traverse(decodeExport))
          }
          definedTypes <- optionalField(args, "types") match {
            case None => Right(Nil)
            case Some(value) =>
              asVector(value).flatMap(_.traverse(decodeDefinedType))
          }
          externals <- optionalField(args, "externals") match {
            case None => Right(Nil)
            case Some(value) =>
              asVector(value).flatMap(_.traverse(decodeExternal))
          }
          lets <- optionalField(args, "defs") match {
            case None => Right(Nil)
            case Some(value) =>
              asVector(value).flatMap(_.traverse(decodeTopLet))
          }
          // local external defs are stored for this package name
          localTypeEnv = externals.foldLeft(TypeEnv.fromDefinitions(definedTypes)) {
            case (acc, (extName, tpe)) => acc.addExternalValue(name, extName, tpe)
          }
          externalDefNames = externals.map(_._1)
          (collisions, importMap) =
            ImportMap.fromImports(imports)((_, _) => ImportMap.Unify.Error)
          _ <- Either.cond(
            collisions.isEmpty,
            (),
            s"import collisions while decoding package ${name.asString}"
          )
          program =
            Program(
              types = localTypeEnv,
              lets = lets,
              externalDefs = externalDefNames,
              from = ()
            )
          pack = Package(name, imports, exports, (program, importMap))
          normalized <- normalizeForRoundTrip(pack)
        } yield normalized
      case other =>
        err(s"invalid package value: ${rendered(other)}")
    }

  given packageCodec: EdnCodec[Package.Typed[Unit]] with {
    def encode(a: Package.Typed[Unit]): Edn = encodePackage(a)
    def decode(edn: Edn): ErrorOr[Package.Typed[Unit]] = decodePackage(edn)
  }

  private def encodeInterface(iface: Package.Interface): Edn =
    EList(
      List(
        sym("interface"),
        kw("name"),
        str(iface.name.asString),
        kw("exports"),
        EVector(iface.exports.map(encodeExport))
      )
    )

  private def decodeInterface(edn: Edn): ErrorOr[Package.Interface] =
    edn match {
      case EList(ESymbol("interface") :: argsRaw) =>
        for {
          args <- decodeKeywordArgs(argsRaw)
          name <- requiredField(args, "name").flatMap(decodePackageName)
          exports <- optionalField(args, "exports") match {
            case None => Right(Nil)
            case Some(value) =>
              asVector(value).flatMap(_.traverse(decodeExport))
          }
        } yield Package(name = name, imports = Nil, exports = exports, program = ())
      case other =>
        err(s"invalid interface: ${rendered(other)}")
    }

  given interfaceCodec: EdnCodec[Package.Interface] with {
    def encode(a: Package.Interface): Edn = encodeInterface(a)
    def decode(edn: Edn): ErrorOr[Package.Interface] = decodeInterface(edn)
  }

  private def importNameForShow(
      item: ImportedName[NonEmptyList[Referant[Kind.Arg]]]
  ): Edn = {
    val original = nameAtom(item.originalName.sourceCodeRepr)
    if (item.originalName == item.localName) original
    else
      EMap(
        List(
          kw("name") -> original,
          kw("as") -> nameAtom(item.localName.sourceCodeRepr)
        )
      )
  }

  private def importRefBucket(ref: Referant[Kind.Arg]): String =
    ref match {
      case Referant.DefinedT(_)       => "types"
      case Referant.Constructor(_, _) => "ctors"
      case Referant.Value(_)          => "values"
    }

  private def encodeImportForShow(
      imp: Import[Package.Interface, NonEmptyList[Referant[Kind.Arg]]]
  ): Edn = {
    val bucketedNames =
      imp.items.toList.flatMap(item =>
        item.tag.toList.map(ref => importRefBucket(ref) -> importNameForShow(item))
      )

    def bucketItems(bucket: String): List[Edn] =
      bucketedNames.collect { case (`bucket`, name) => name }.distinct

    val grouped =
      List(
        {
          val types = bucketItems("types")
          if (types.isEmpty) None else Some(kw("types") -> EVector(types))
        },
        {
          val values = bucketItems("values")
          if (values.isEmpty) None else Some(kw("values") -> EVector(values))
        },
        {
          val ctors = bucketItems("ctors")
          if (ctors.isEmpty) None else Some(kw("ctors") -> EVector(ctors))
        }
      ).flatten

    EList(
      List(
        sym("import"),
        nameAtom(imp.pack.name.asString),
        EMap(grouped)
      )
    )
  }

  private def encodeExportsMapForShow(
      exports: List[ExportedName[Referant[Kind.Arg]]]
  ): Option[Edn] = {
    val exportedTypes =
      exports.collect {
        case ExportedName.TypeName(name, _) => name.sourceCodeRepr
      }
    val exportedCtors =
      exports.collect {
        case ExportedName.Constructor(name, _) => name.sourceCodeRepr
      }
    val exportedValues =
      exports.collect {
        case ExportedName.Binding(name, _) => name.sourceCodeRepr
      }

    val items =
      List(
        if (exportedTypes.isEmpty) None
        else Some(kw("types") -> EVector(exportedTypes.map(nameAtom))),
        if (exportedValues.isEmpty) None
        else Some(kw("values") -> EVector(exportedValues.map(nameAtom))),
        if (exportedCtors.isEmpty) None
        else Some(kw("ctors") -> EVector(exportedCtors.map(nameAtom)))
      ).flatten

    if (items.isEmpty) None
    else Some(EMap(items))
  }

  private def encodeInterfaceForShow(iface: Package.Interface): Edn = {
    val attrs = encodeExportsMapForShow(iface.exports) match {
      case Some(exportsMap) => List(kw("exports"), exportsMap)
      case None             => Nil
    }

    EList(
      List(sym("interface"), kw("name"), nameAtom(iface.name.asString)) ++ attrs
    )
  }

  private def encodePackageForShow(normalized: Package.Typed[Unit]): Edn = {
    val prog = normalized.program._1

    val localTypes =
      prog.types.allDefinedTypes.filter(_.packageName == normalized.name)

    val externals =
      prog.externalDefs.flatMap { name =>
        prog.types
          .getExternalValue(normalized.name, name)
          .map(name -> _)
      }

    val exportsMap = encodeExportsMapForShow(normalized.exports)

    val attrs =
      List(
        if (normalized.imports.isEmpty) None
        else Some(kw("imports") -> EVector(normalized.imports.map(encodeImportForShow))),
        exportsMap.map(kw("exports") -> _),
        if (localTypes.isEmpty) None
        else Some(kw("types") -> EVector(localTypes.map(encodeDefinedType))),
        if (externals.isEmpty) None
        else Some(kw("externals") -> EVector(externals.map(encodeExternal))),
        if (prog.lets.isEmpty) None
        else Some(kw("defs") -> EVector(prog.lets.map(encodeTopLet)))
      ).flatten

    EList(
      List(sym("package"), kw("name"), nameAtom(normalized.name.asString)) ++
        attrs.flatMap { case (k, v) => List(k, v) }
    )
  }

  def packageDoc(pack: Package.Typed[Any]): Doc = {
    normalizeForRoundTrip(pack.void) match {
      case Right(normalized) => EdnCodec.toDoc(normalized)
      case Left(message)     =>
        Edn.toDoc(EList(List(sym("show-error"), str(message), str(pack.name.asString))))
    }
  }

  def interfaceDoc(iface: Package.Interface): Doc =
    EdnCodec.toDoc(iface)

  def showDoc(
      packs: List[Package.Typed[Any]],
      ifaces: List[Package.Interface]
  ): Doc = {
    val value =
      EList(
        List(
          sym("show"),
          kw("interfaces"),
          EVector(ifaces.map(encodeInterfaceForShow)),
          kw("packages"),
          EVector(packs.map(p => normalizeForRoundTrip(p.void).fold(msg =>
            EList(List(sym("show-error"), str(msg), str(p.name.asString))),
            encodePackageForShow
          )))
        )
      )
    Edn.toDoc(value)
  }
}
