package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.data.NonEmptyList
import dev.bosatsu.cache.CompileCache
import dev.bosatsu.Identifier.{Constructor, Name}
import dev.bosatsu.rankn.{ConstructorFn, ConstructorParam, DataRepr, DefinedType, Type, TypeAlias}
import scala.util.Try

class PackageSignaturesTest extends munit.FunSuite {
  private val owner = PackageName.parts("Owner")
  private val facade = PackageName.parts("Facade")
  private val client = PackageName.parts("Client")

  private def interface(
      name: PackageName, exports: List[ExportedName[Referant[Kind.Arg]]]
  ): Package.Interface = Package(name, Nil, exports, ())

  private val zero = ConstructorFn(Constructor("Zero"), Nil, Nil)
  private val more = ConstructorFn(
    Constructor("More"),
    List(ConstructorParam(
      Name("count"), Type.IntType, Some(Name("default_count")), Some(Type.IntType)
    )),
    List((Type.Var.Bound("element"), Kind.Type.co))
  )
  private val flag = DefinedType(
    owner, TypeName(Constructor("Flag")), List((Type.Var.Bound("a"), Kind.Type.phantom)),
    List(zero, more)
  )
  private val alias = TypeAlias(
    owner, TypeName(Constructor("Flags")), Nil, Type.TyApply(flag.toTypeTyConst, Type.IntType)
  )
  private val ownerIface = interface(owner, List(
    ExportedName.TypeName(flag.name.ident, Referant.DefinedT(flag)),
    ExportedName.Constructor(zero.name, Referant.Constructor(flag, zero)),
    ExportedName.Constructor(more.name, Referant.Constructor(flag, more)),
    ExportedName.TypeName(alias.name.ident, Referant.TypeAliasT(alias))
  ))
  private val facadeIface = interface(facade, List(
    ExportedName.TypeName(Constructor("ForeignFlag"), Referant.DefinedT(flag)),
    ExportedName.Constructor(Constructor("ForeignZero"), Referant.Constructor(flag, zero)),
    ExportedName.TypeName(Constructor("ForeignFlags"), Referant.TypeAliasT(alias))
  ))
  private def success[A](result: Try[A]): A =
    result.fold(e => fail(e.toString), identity)

  private def signature(ifaces: Package.Interface*): proto.Interfaces =
    success(PackageSignatures.encode(ifaces.toList))

  private def decodeRequired(
      bundles: List[(String, proto.Interfaces)], requirements: List[(PackageName, List[PackageName])]
  ): Try[List[Package.Interface]] =
    for {
      stages <- PackageSignatures.plan(bundles, requirements, builtinPredef = true)
      providers <- stages.foldLeft(Try(PackageSignatures.implicitProviders))(
        (acc, stage) => acc.flatMap(stage.bind)
      )
    } yield providers.valuesIterator.map(_.iface).filterNot(_.name.isPredef).toList.sortBy(_.name)

  private def read(bundles: (String, proto.Interfaces)*): Try[List[Package.Interface]] =
    decodeRequired(bundles.toList, List(client -> List(facade)))

  test("external types, constructors and aliases use their owning provider") {
    val front = signature(facadeIface)
    val entry = front.interfaces.head
    assertEquals(entry.definedTypes.size, 0)
    assertEquals(entry.typeAliases.size, 0)
    assertEquals(entry.getSignatureDependencies.exposes.toList, List("Owner"))
    assert(read("front.sig" -> front).failed.get.getMessage.contains("Client -> Facade -> Owner"))

    val original = signature(ownerIface)
    val result = success(read("owner.sig" -> original, "front.sig" -> front))
    assertEquals(result, List(facadeIface, ownerIface).map(PackageSignatures.canonical))
    val ownerType = result.last.exports.flatMap(_.tag.definedType).head
    assertEquals(ownerType.annotatedTypeParams, flag.annotatedTypeParams)
    assertEquals(ownerType.constructors, List(zero, more))
    assertEquals(ownerType.dataRepr(zero.name), DataRepr.Enum(0, 0, List(0, 1)))

    // Changing the actual owner changes the decoded facts even when the
    // re-exporting package's bytes stay the same. Removing an export fails.
    val changedAlias = alias.copy(rhs = Type.IntType)
    val changed = ownerIface.copy(exports = ownerIface.exports.map {
      case ExportedName.TypeName(n, Referant.TypeAliasT(_)) =>
        ExportedName.TypeName(n, Referant.TypeAliasT(changedAlias))
      case exp => exp
    })
    val rebound = success(read("front.sig" -> front, "owner.sig" -> signature(changed)))
    assertEquals(rebound.head.exports.flatMap(_.tag.typeAlias), List(changedAlias))
    val incomplete = ownerIface.copy(exports = ownerIface.exports.filter(_.tag.typeAlias.isEmpty))
    assert(read("owner.sig" -> signature(incomplete), "front.sig" -> front).isFailure)
    val partial = ownerIface.copy(exports = ownerIface.exports.filterNot(_.name == zero.name))
    val hidden = read("owner.sig" -> signature(partial), "front.sig" -> front)
    assert(hidden.failed.get.getMessage.contains("missing exported constructor Owner::Zero"))
  }

  test("whole-file signature decoding rejects a foreign re-export of a hidden constructor") {
    val partial = ownerIface.copy(exports = ownerIface.exports.filterNot(_.name == zero.name))
    val bundle = signature(partial, facadeIface)
    val result = ProtoConverter.interfacesFromProto(bundle)
    assert(result.isFailure, "the whole-file reader exposed Owner::Zero")
    assert(result.failed.get.getMessage.contains("Owner::Zero"))
  }

  test("constructor default-helper types participate in exposure checking") {
    val field = ConstructorParam(
      Name("field"), Type.IntType, Some(Name("default_field")), Some(alias.rhs)
    )
    val ctor = ConstructorFn(Constructor("Wrapped"), List(field), Nil)
    val dt = DefinedType(facade, TypeName(ctor.name), Nil, List(ctor))
    val wrapper = interface(facade, List(
      ExportedName.TypeName(ctor.name, Referant.DefinedT(dt)),
      ExportedName.Constructor(ctor.name, Referant.Constructor(dt, ctor))
    ))
    val bundle = signature(wrapper)
    assertEquals(bundle.interfaces.head.getSignatureDependencies.exposes.toList, List("Owner"))
    assert(read("wrapper" -> bundle).isFailure)
    val decoded = success(read("wrapper" -> bundle, "owner" -> signature(ownerIface)))
    assertEquals(decoded.head, PackageSignatures.canonical(wrapper))
  }

  test("unindexed signatures recover exposures only for selected roots") {
    val missingType = Type.const(PackageName.parts("Missing"), TypeName(Constructor("T")))
    val unused = interface(PackageName.parts("Sibling"),
      List(ExportedName.Binding(Name("unused"), Referant.Value(missingType))))
    val old = success(ProtoConverter.interfacesToProto(List(facadeIface, unused)))
    val owners = success(ProtoConverter.interfacesToProto(List(ownerIface)))
    assert(old.interfaces.forall(_.signatureDependencies.isEmpty))
    val selected = success(read("old.sig" -> old, "owner.sig" -> owners))
    assertEquals(selected.map(_.name), List(facade, owner))
  }

  test("embedded foreign definitions in a legacy signature are rebound to the declared owner") {
    val old = success(ProtoConverter.interfaceToProto(ownerIface))
    val embedded = old.copy(strings = old.strings :+ facade.asString, packageName = old.strings.size + 1)
    val bundle = proto.Interfaces(List(embedded))
    assert(read("legacy" -> bundle).failed.get.getMessage.contains("Client -> Facade -> Owner"))

    val authoritative = alias.copy(rhs = Type.StrType)
    val updated = ownerIface.copy(exports = ownerIface.exports.map {
      case ExportedName.TypeName(n, Referant.TypeAliasT(_)) =>
        ExportedName.TypeName(n, Referant.TypeAliasT(authoritative))
      case exp => exp
    })
    val roots = success(read("legacy" -> bundle, "owner" -> signature(updated)))
    assertEquals(roots.head.exports.flatMap(_.tag.typeAlias), List(authoritative))
  }

  test("an explicitly supplied Predef can expose an independent type owner") {
    val publicType = flag.toOpaque.copy(annotatedTypeParams = Nil)
    val owned = interface(owner, List(ExportedName.TypeName(publicType.name.ident, Referant.DefinedT(publicType))))
    val predef = interface(PackageName.PredefName, List(
      ExportedName.Binding(Name("provided"), Referant.Value(publicType.toTypeTyConst))
    ))
    val stages = success(PackageSignatures.plan(
      List("custom.sig" -> signature(predef, owned)), List(client -> Nil), builtinPredef = false
    ))
    val restored = stages.foldLeft(Map.empty[PackageName, PackageSignatures.PublicInterface])(
      (providers, stage) => success(stage.bind(providers))
    )
    assertEquals(restored(PackageName.PredefName).iface, predef)
    assertEquals(restored(owner).iface, owned)
  }

  test("the compiler cache fingerprints resolved foreign definitions supplied by typed callers") {
    val changed = facadeIface.copy(exports = facadeIface.exports.map {
      case ExportedName.TypeName(n, Referant.TypeAliasT(ta)) =>
        ExportedName.TypeName(n, Referant.TypeAliasT(ta.copy(rhs = Type.StrType)))
      case e => e
    })
    // The distributable signature deliberately records only the symbolic owner.
    assertEquals(signature(facadeIface), signature(changed))
    assertNotEquals(success(CompileCache.interfaceHash(facadeIface)), success(CompileCache.interfaceHash(changed)))
    val conflicting = facadeIface.copy(exports = ExportedName.TypeName(
      Constructor("OtherAlias"), Referant.TypeAliasT(alias.copy(rhs = Type.StrType))
    ) :: facadeIface.exports)
    assert(CompileCache.interfaceHash(conflicting).failed.get.getMessage.contains("conflicting definition"))
  }

  test("public changes affect bytes and constructor tag order survives export reordering") {
    val normal = signature(ownerIface, facadeIface)
    val reversedExports = ownerIface.copy(exports = ownerIface.exports.reverse)
    assertEquals(signature(facadeIface, reversedExports), normal)

    val reordered = flag.copy(constructors = flag.constructors.reverse)
    val changed = ownerIface.copy(exports = ownerIface.exports.map {
      case ExportedName.TypeName(n, Referant.DefinedT(_)) =>
        ExportedName.TypeName(n, Referant.DefinedT(reordered))
      case ExportedName.Constructor(n, Referant.Constructor(_, cf)) =>
        ExportedName.Constructor(n, Referant.Constructor(reordered, cf))
      case e => e
    })
    assertNotEquals(signature(changed), signature(ownerIface))
  }

  test("missing type definitions, forged local pointers and stale indexes fail") {
    val value = interface(facade, List(ExportedName.Binding(Name("flags"), Referant.Value(alias.rhs))))
    val single = signature(value)
    val wrong = read("value.sig" -> single, "owner.sig" -> signature(interface(owner, Nil)))
    assert(wrong.failed.get.getMessage.contains("missing exported type"))
    val stale = single.copy(interfaces = single.interfaces.map(
      _.withSignatureDependencies(proto.InterfaceDependencies())
    ))
    assert(read("stale.sig" -> stale).failed.get.getMessage.contains("incorrect exposure index"))
    val own = signature(ownerIface).interfaces.head
    val forged = own.copy(
      strings = own.strings :+ facade.asString,
      packageName = own.strings.size + 1
    )
    val impersonation = read("forged.sig" -> proto.Interfaces(List(forged)))
    assert(impersonation.failed.get.getMessage.contains("definition owner"))
    val ambiguous = read("a.sig" -> single, "b.sig" -> single)
    assert(ambiguous.failed.get.getMessage.contains("a.sig, b.sig"))
  }

  test("a private constructor field type does not mask a stale kind in a sibling argument") {
    val hidden = Type.const(facade, TypeName(Constructor("Hidden")))
    def wrapper(result: Type): proto.Interfaces = {
      val field = ConstructorParam(Name("apply"), Type.Fun(NonEmptyList.one(hidden), result), None)
      val ctor = ConstructorFn(Constructor("Wrap"), field :: Nil, Nil)
      val dt = DefinedType(facade, TypeName(ctor.name), Nil, ctor :: Nil)
      signature(interface(facade, List(
        ExportedName.TypeName(ctor.name, Referant.DefinedT(dt)),
        ExportedName.Constructor(ctor.name, Referant.Constructor(dt, ctor))
      )))
    }
    val owningSignature = signature(ownerIface)
    assert(read("wrap" -> wrapper(alias.rhs), "owner" -> owningSignature).isSuccess)
    assert(read("wrap" -> wrapper(flag.toTypeTyConst), "owner" -> owningSignature).isFailure)
  }

  test("checking the kind of a deeply nested public value is stack safe") {
    val depth = 6000
    // Build the table in wire order so the fixture does not recursively encode
    // the nested type while testing the reader.
    val leaves = Vector(
      proto.Type().withTypeConst(proto.TypeConst(2, 3)),
      proto.Type().withTypeConst(proto.TypeConst(2, 4))
    )
    val applications = Vector.tabulate(depth)(i =>
      proto.Type().withTypeApply(proto.TypeApply(left = 1, right = 2 + i)))
    val entry = proto.Interface(
      strings = Vector("Facade", "Bosatsu/Predef", "List", "Int", "deep"),
      packageName = 1,
      types = leaves ++ applications,
      exports = List(proto.ExportedName(name = 5, referant = Some(proto.Referant().withValue(2 + depth)))),
      signatureDependencies = Some(proto.InterfaceDependencies())
    )
    val result =
      try read("deep.sig" -> proto.Interfaces(List(entry)))
      catch { case _: StackOverflowError => fail("stack overflow validating nested List applications") }
    assertEquals(success(result).map(_.name), List(facade))
  }

  test("whole-file and selected readers handle a deep legacy re-export chain in either order") {
    val depth = 3000
    val own = TypeName(Constructor("Own"))
    def dt(i: Int) = DefinedType(PackageName.parts(f"P$i%04d"), own, Nil, Nil)
    val entries = (0 until depth).toList.map { i =>
      val current = dt(i)
      val local = ExportedName.TypeName(own.ident, Referant.DefinedT(current))
      val next = Option.when(i + 1 < depth)(
        ExportedName.TypeName(Constructor("Next"), Referant.DefinedT(dt(i + 1)))
      )
      success(ProtoConverter.interfaceToProto(interface(current.packageName, local :: next.toList)))
    }
    for (ordered <- List(entries, entries.reverse)) {
      val chain = proto.Interfaces(ordered)
      val whole = success(ProtoConverter.interfacesFromProto(chain))
      val selected = success(decodeRequired(List("chain" -> chain), List(client -> List(dt(0).packageName))))
      assertEquals(selected, whole.map(PackageSignatures.canonical).sortBy(_.name))
      assertEquals(selected.size, depth)
    }
  }

  test("index traversal is stack safe and diagnoses exposure cycles") {
    val depth = 3000
    def name(i: Int): PackageName = PackageName.parts(s"P$i")
    def root(i: Int, next: Option[PackageName]): Package.Interface = {
      val pn = name(i)
      val dt = DefinedType(pn, TypeName(Constructor("T")), Nil, Nil)
      val typ = ExportedName.TypeName(dt.name.ident, Referant.DefinedT(dt))
      val binding = next.toList.map { dep =>
        ExportedName.Binding(Name("next"), Referant.Value(Type.const(dep, dt.name)))
      }
      interface(pn, typ :: binding)
    }
    val roots = (0 until depth).toList.map { i =>
      root(i, Option.when(i + 1 < depth)(name(i + 1)))
    }
    def decode(bundle: proto.Interfaces) =
      decodeRequired(List("chain" -> bundle), List(client -> List(name(0))))
    assertEquals(success(decode(success(PackageSignatures.encode(roots)))).size, depth)
    val cycle = signature(root(0, Some(name(1))), root(1, Some(name(0))))
    assert(decode(cycle).failed.get.getMessage.contains("Client -> P0 -> P1 -> P0"))
  }
}
