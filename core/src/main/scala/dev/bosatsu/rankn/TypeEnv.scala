package dev.bosatsu.rankn

import dev.bosatsu.{TypeName, PackageName, Identifier, Kind, Variance}
import dev.bosatsu.Identifier.{Bindable, Constructor}
import scala.collection.immutable.SortedMap
import org.typelevel.paiges.{Doc, Document}

class TypeEnv[+A] private (
    protected val values: SortedMap[(PackageName, Identifier), Type],
    protected val constructors: SortedMap[
      (PackageName, Constructor),
      (DefinedType[A], ConstructorFn[A])
    ],
    val definedTypes: SortedMap[(PackageName, TypeName), DefinedType[A]]
) {

  override def equals(that: Any): Boolean =
    that match {
      case te: TypeEnv[?] =>
        values.equals(te.values) &&
        constructors.equals(te.constructors) &&
        definedTypes.equals(te.definedTypes)
      case _ => false
    }

  lazy val referencedPackages: Set[PackageName] = {
    def keys1[K, B](k: SortedMap[(PackageName, K), B]): Iterator[PackageName] =
      k.keys.iterator.map(_._1)

    (keys1(values) ++ keys1(constructors) ++ keys1(definedTypes)).toSet
  }

  override def hashCode: Int =
    (getClass, values, constructors, definedTypes).hashCode

  override def toString: String =
    s"TypeEnv($values, $constructors, $definedTypes)"

  def allDefinedTypes: List[DefinedType[A]] =
    definedTypes.values.toList.sortBy(dt => (dt.packageName, dt.name))

  def getConstructor(
      p: PackageName,
      c: Constructor
  ): Option[(DefinedType[A], ConstructorFn[A])] =
    constructors.get((p, c))

  def getConstructorParams(
      p: PackageName,
      c: Constructor
  ): Option[List[(Bindable, Type)]] =
    constructors.get((p, c)).map(_._2.args)

  def getType(p: PackageName, t: TypeName): Option[DefinedType[A]] =
    definedTypes.get((p, t))

  def getType(t: Type.TyConst): Option[DefinedType[A]] = {
    val d = t.tpe.toDefined
    getType(d.packageName, d.name)
  }

  def getExternalValue(p: PackageName, n: Identifier): Option[Type] =
    values.get((p, n))

  /** Lisp-style representation of all defined types in this environment.
    */
  def reprDoc(implicit ev: A <:< Kind.Arg): Doc =
    TypeEnv.reprDoc(this)

  /** Lisp-style representation of only the defined types from a given package.
    */
  def reprDocForPackage(p: PackageName)(implicit ev: A <:< Kind.Arg): Doc =
    TypeEnv.reprDocForPackage(this, p)

  // when we have resolved, we can get the types of constructors out
  def getValue(p: PackageName, n: Identifier)(implicit
      ev: A <:< Kind.Arg
  ): Option[Type] =
    n match {
      case c @ Constructor(_) =>
        // constructors are never external defs
        constructors.get((p, c)).map { case (dt, cfn) => dt.fnTypeOf(cfn) }
      case notCons => getExternalValue(p, notCons)
    }

  // when we have resolved, we can get the types of constructors out
  def localValuesOf(
      p: PackageName
  )(implicit ev: A <:< Kind.Arg): SortedMap[Identifier, Type] = {
    val bldr = SortedMap.newBuilder[Identifier, Type]
    // add externals
    bldr ++= values.iterator.collect { case ((pn, n), v) if pn == p => (n, v) }
    // add constructors
    bldr ++= constructors.iterator.collect {
      case ((pn, n), (dt, cf)) if pn == p => (n, dt.fnTypeOf(cf))
    }

    bldr.result()
  }

  def addConstructor[A1 >: A](
      pack: PackageName,
      dt: DefinedType[A1],
      cf: ConstructorFn[A1]
  ): TypeEnv[A1] = {
    val nec = constructors.updated((pack, cf.name), (dt, cf))
    val dt1 = definedTypes.updated((dt.packageName, dt.name), dt)
    new TypeEnv(values = values, constructors = nec, definedTypes = dt1)
  }

  /** only add the type, do not add any of the constructors used when importing
    * values
    */
  def addDefinedType[A1 >: A](dt: DefinedType[A1]): TypeEnv[A1] = {
    val dt1 = definedTypes.updated((dt.packageName, dt.name), dt)
    new TypeEnv(
      constructors = constructors,
      definedTypes = dt1,
      values = values
    )
  }

  /** add a DefinedType and all of its constructors. This is done locally for a
    * package
    */
  def addDefinedTypeAndConstructors[A1 >: A](
      dt: DefinedType[A1]
  ): TypeEnv[A1] = {
    val dt1 = definedTypes.updated((dt.packageName, dt.name), dt)
    val cons1 =
      dt.constructors
        .foldLeft(
          constructors: SortedMap[
            (PackageName, Constructor),
            (DefinedType[A1], ConstructorFn[A1])
          ]
        ) { case (cons0, cf) =>
          cons0.updated((dt.packageName, cf.name), (dt, cf))
        }

    new TypeEnv(constructors = cons1, definedTypes = dt1, values = values)
  }

  /** External values cannot be inferred and have to be fully annotated
    */
  def addExternalValue(
      pack: PackageName,
      name: Identifier,
      t: Type
  ): TypeEnv[A] =
    new TypeEnv(
      constructors = constructors,
      definedTypes = definedTypes,
      values = values.updated((pack, name), t)
    )

  lazy val typeConstructors: SortedMap[
    (PackageName, Constructor),
    (
        List[(Type.Var.Bound, A)],
        List[(Type.Var.Bound, A)],
        List[Type],
        Type.Const.Defined
    )
  ] =
    constructors.map { case (pc, (dt, cf)) =>
      (
        pc,
        (dt.annotatedTypeParams, cf.exists, cf.args.map(_._2), dt.toTypeConst)
      )
    }

  def definedTypeFor(c: (PackageName, Constructor)): Option[DefinedType[A]] =
    typeConstructors.get(c).flatMap { case (_, _, _, d) => toDefinedType(d) }

  def toDefinedType(t: Type.Const): Option[DefinedType[A]] =
    t match {
      case Type.Const.Defined(p, n) => getType(p, n)
    }

  def ++[A1 >: A](that: TypeEnv[A1]): TypeEnv[A1] =
    new TypeEnv(
      values ++ that.values,
      constructors ++ that.constructors,
      definedTypes ++ that.definedTypes
    )

  def toKindMap(implicit ev: A <:< Kind.Arg): Map[Type.Const.Defined, Kind] = {
    type F[+Z] = List[DefinedType[Z]]
    val dts: List[DefinedType[Kind.Arg]] = ev.substituteCo[F](allDefinedTypes)
    DefinedType.toKindMap(dts)
  }
}

object TypeEnv {
  private val tyDoc: Document[Type] = Type.fullyResolvedDocument

  private def block(d: Doc): Doc =
    d.nested(4).grouped

  private def listDoc(items: List[Doc]): Doc =
    Doc.char('[') + block(Doc.intercalate(Doc.line, items)) + Doc.char(']')

  private def bindableTypeDoc(field: (Bindable, Type)): Doc = {
    val (name, tpe) = field
    block(
      Doc.char('[') + Doc.text(name.sourceCodeRepr) + Doc.line + tyDoc
        .document(tpe) + Doc.char(']')
    )
  }

  private def kindArgDoc(arg: (Type.Var.Bound, Kind.Arg)): Doc = {
    val (tv, ka) = arg
    if ((ka.variance == Variance.in) && ka.kind.isType) Doc.text(tv.name)
    else Doc.text(tv.name) + Doc.text(": ") + Kind.argDoc(ka)
  }

  private def constructorDoc(cf: ConstructorFn[Kind.Arg]): Doc = {
    val exists = cf.exists.map(kindArgDoc)
    val fields = cf.args.map(bindableTypeDoc)
    val attrs =
      List(
        if (exists.isEmpty) None
        else Some(Doc.text("(exists") + Doc.line + listDoc(exists) + Doc.char(')')),
        if (fields.isEmpty) None
        else Some(Doc.text("(fields") + Doc.line + listDoc(fields) + Doc.char(')'))
      ).flatten

    block(
      if (attrs.isEmpty)
        Doc.text("(constructor") + Doc.line + Doc.text(
          cf.name.sourceCodeRepr
        ) + Doc.char(')')
      else
        Doc.text("(constructor") + Doc.line + Doc.text(
          cf.name.sourceCodeRepr
        ) + Doc.line + Doc.intercalate(Doc.line, attrs) + Doc.char(')')
    )
  }

  private def definedTypeDoc(dt: DefinedType[Kind.Arg]): Doc = {
    val typeParams = dt.annotatedTypeParams.map(kindArgDoc)
    val constructors = dt.constructors.map(constructorDoc)
    val attrs =
      List(
        if (typeParams.isEmpty) None
        else Some(Doc.text("(params") + Doc.line + listDoc(typeParams) + Doc.char(')')),
        Some(
          Doc.text("(constructors") + Doc.line + listDoc(constructors) + Doc
            .char(')')
        )
      ).flatten

    block(
      Doc.text("(type") + Doc.line + Doc.text(
        dt.name.ident.sourceCodeRepr
      ) + Doc.line + Doc.intercalate(Doc.line, attrs) + Doc.char(')')
    )
  }

  private def definedTypesDoc[A](
      dts: List[DefinedType[A]]
  )(implicit ev: A <:< Kind.Arg): Doc = {
    type DTs[+X] = List[DefinedType[X]]
    val typed = ev.substituteCo[DTs](dts)
    listDoc(typed.map(definedTypeDoc))
  }

  def reprDoc[A](env: TypeEnv[A])(implicit ev: A <:< Kind.Arg): Doc =
    definedTypesDoc(env.allDefinedTypes)

  def reprDocForPackage[A](env: TypeEnv[A], p: PackageName)(implicit
      ev: A <:< Kind.Arg
  ): Doc =
    definedTypesDoc(env.allDefinedTypes.filter(_.packageName == p))

  val empty: TypeEnv[Nothing] =
    new TypeEnv(
      SortedMap.empty[(PackageName, Identifier), Type],
      SortedMap.empty[
        (PackageName, Constructor),
        (DefinedType[Nothing], ConstructorFn[Nothing])
      ],
      SortedMap.empty[(PackageName, TypeName), DefinedType[Nothing]]
    )

  /** Adds all the types and all the constructors from the given types
    */
  def fromDefinitions[A](defs: List[DefinedType[A]]): TypeEnv[A] =
    defs.foldLeft(empty: TypeEnv[A])(_.addDefinedTypeAndConstructors(_))

  def fromParsed[A](p: ParsedTypeEnv[A]): TypeEnv[A] = {
    val t1 = p.allDefinedTypes.foldLeft(empty: TypeEnv[A])(
      _.addDefinedTypeAndConstructors(_)
    )
    p.externalDefs.foldLeft(t1) { case (t1, (p, n, t)) =>
      t1.addExternalValue(p, n, t)
    }
  }

  implicit def catsMonoidTypeEnv[A]: cats.Monoid[TypeEnv[A]] =
    new cats.Monoid[TypeEnv[A]] {
      def empty: TypeEnv[A] = TypeEnv.empty
      def combine(a: TypeEnv[A], b: TypeEnv[A]) = a ++ b
    }
}
