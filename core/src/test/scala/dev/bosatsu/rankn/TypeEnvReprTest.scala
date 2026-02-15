package dev.bosatsu.rankn

import dev.bosatsu.{Kind, PackageName, TypeName, Identifier}

class TypeEnvReprTest extends munit.FunSuite {
  private val localPack = PackageName.parts("MyLib", "Foo")
  private val otherPack = PackageName.parts("Other", "Pkg")

  private def normalizeWhitespace(str: String): String =
    str.replaceAll("\\s+", " ").trim

  test("reprDocForPackage includes constructor existentials and named fields") {
    val a = Type.Var.Bound("a")
    val b = Type.Var.Bound("b")

    val dt = DefinedType[Kind.Arg](
      packageName = localPack,
      name = TypeName("Pair"),
      annotatedTypeParams = List((a, Kind.Type.in)),
      constructors = List(
        ConstructorFn[Kind.Arg](
          name = Identifier.Constructor("MkPair"),
          args = List(
            Identifier.Name("left") -> Type.TyVar(a),
            Identifier.Name("right") -> Type.TyVar(b),
            Identifier.Name("count") -> Type.IntType
          ),
          exists = List((b, Kind.Type.in))
        )
      )
    )

    val env = TypeEnv.fromDefinitions(List(dt))
    val rendered = normalizeWhitespace(env.reprDocForPackage(localPack).render(200))

    assert(rendered.contains("(type Pair"), rendered)
    assert(rendered.contains("(params [a])"), rendered)
    assert(
      rendered.contains(
        "(constructor MkPair (exists [b]) (fields [[left a] [right b] [count Bosatsu/Predef::Int]]))"
      ),
      rendered
    )
    assert(!rendered.contains("forall"), rendered)
  }

  test("reprDocForPackage omits empty constructor attrs") {
    val a = Type.Var.Bound("a")
    val dt = DefinedType[Kind.Arg](
      packageName = localPack,
      name = TypeName("Opt"),
      annotatedTypeParams = List((a, Kind.Type.in)),
      constructors = List(
        ConstructorFn[Kind.Arg](name = Identifier.Constructor("N"), args = Nil),
        ConstructorFn[Kind.Arg](
          name = Identifier.Constructor("S"),
          args = List(Identifier.Name("value") -> Type.TyVar(a))
        )
      )
    )

    val env = TypeEnv.fromDefinitions(List(dt))
    val rendered = normalizeWhitespace(env.reprDocForPackage(localPack).render(200))

    assert(rendered.contains("(constructor N)"), rendered)
    assert(rendered.contains("(constructor S (fields [[value a]]))"), rendered)
    assert(!rendered.contains("(exists [])"), rendered)
    assert(!rendered.contains("(fields [])"), rendered)
  }

  test("reprDocForPackage only renders types for the requested package") {
    val local =
      DefinedType[Kind.Arg](
        packageName = localPack,
        name = TypeName("Local"),
        annotatedTypeParams = Nil,
        constructors = Nil
      )
    val remote =
      DefinedType[Kind.Arg](
        packageName = otherPack,
        name = TypeName("Remote"),
        annotatedTypeParams = Nil,
        constructors = Nil
      )

    val env = TypeEnv.fromDefinitions(List(remote, local))
    val rendered = normalizeWhitespace(env.reprDocForPackage(localPack).render(120))

    assert(rendered.contains("Local"), rendered)
    assert(!rendered.contains("Remote"), rendered)
    assert(!rendered.contains("(params [])"), rendered)
  }
}
