package dev.bosatsu.permissions

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import cats.data.NonEmptyList
import dev.bosatsu.{Identifier, Matchless, PackageName}
import dev.bosatsu.Identifier.Bindable

object PermissionAnalyzerTest extends Properties("PermissionAnalyzer") {
  import PermissionGen._

  val testPkg: PackageName = PackageName.parts("Test", "Pkg")
  val testName: Bindable = Identifier.Name("testFn")

  property("empty expression has no permissions") = {
    val expr: Matchless.Expr[Unit] = Matchless.Literal(dev.bosatsu.Lit.Str(""))
    val result = PermissionAnalyzer.analyze(expr, Map.empty)
    result.isEmpty
  }

  property("local variable has no permissions") = {
    val name = Identifier.Name("x")
    val expr: Matchless.Expr[Unit] = Matchless.Local(name)
    val result = PermissionAnalyzer.analyze(expr, Map.empty)
    result.isEmpty
  }

  property("global with annotation includes permissions") = forAll(permission) { perm =>
    val globalPkg = PackageName.parts("Other", "Pkg")
    val globalName = Identifier.Name("otherFn")
    val annotations: PermissionAnalyzer.PermissionAnnotations = Map(
      (globalPkg, globalName) -> PermissionConfig.require(perm)
    )

    val expr: Matchless.Expr[Unit] = Matchless.Global((), globalPkg, globalName)
    val result = PermissionAnalyzer.analyze(expr, annotations)

    result.staticRequirements.contains(perm) &&
      result.calledFunctions.contains((globalPkg, globalName))
  }

  property("global without annotation has no permissions") = {
    val globalPkg = PackageName.parts("Other", "Pkg")
    val globalName = Identifier.Name("otherFn")

    val expr: Matchless.Expr[Unit] = Matchless.Global((), globalPkg, globalName)
    val result = PermissionAnalyzer.analyze(expr, Map.empty)

    result.staticRequirements.isEmpty &&
      result.calledFunctions.contains((globalPkg, globalName))
  }

  property("app merges fn and arg permissions") = forAll(permission, permission) { (perm1, perm2) =>
    val pkg1 = PackageName.parts("Pkg", "One")
    val name1 = Identifier.Name("fn1")
    val pkg2 = PackageName.parts("Pkg", "Two")
    val name2 = Identifier.Name("fn2")

    val annotations: PermissionAnalyzer.PermissionAnnotations = Map(
      (pkg1, name1) -> PermissionConfig.require(perm1),
      (pkg2, name2) -> PermissionConfig.require(perm2)
    )

    val fn: Matchless.Expr[Unit] = Matchless.Global((), pkg1, name1)
    val arg: Matchless.Expr[Unit] = Matchless.Global((), pkg2, name2)
    val expr: Matchless.Expr[Unit] = Matchless.App(fn, NonEmptyList.one(arg))

    val result = PermissionAnalyzer.analyze(expr, annotations)

    result.staticRequirements.contains(perm1) &&
      result.staticRequirements.contains(perm2)
  }

  property("let merges value and body permissions") = forAll(permission, permission) { (perm1, perm2) =>
    val pkg1 = PackageName.parts("Pkg", "One")
    val name1 = Identifier.Name("fn1")
    val pkg2 = PackageName.parts("Pkg", "Two")
    val name2 = Identifier.Name("fn2")

    val annotations: PermissionAnalyzer.PermissionAnnotations = Map(
      (pkg1, name1) -> PermissionConfig.require(perm1),
      (pkg2, name2) -> PermissionConfig.require(perm2)
    )

    val value: Matchless.Expr[Unit] = Matchless.Global((), pkg1, name1)
    val body: Matchless.Expr[Unit] = Matchless.Global((), pkg2, name2)
    val bindName = Identifier.Name("x")
    val expr: Matchless.Expr[Unit] = Matchless.Let(Right(bindName), value, body)

    val result = PermissionAnalyzer.analyze(expr, annotations)

    result.staticRequirements.contains(perm1) &&
      result.staticRequirements.contains(perm2)
  }

  property("lambda analyzes body") = forAll(permission) { perm =>
    val pkg = PackageName.parts("Pkg", "One")
    val name = Identifier.Name("fn")

    val annotations: PermissionAnalyzer.PermissionAnnotations = Map(
      (pkg, name) -> PermissionConfig.require(perm)
    )

    val body: Matchless.Expr[Unit] = Matchless.Global((), pkg, name)
    val argName = Identifier.Name("x")
    val expr: Matchless.Expr[Unit] = Matchless.Lambda(
      Nil,
      None,
      NonEmptyList.one(argName),
      body
    )

    val result = PermissionAnalyzer.analyze(expr, annotations)

    result.staticRequirements.contains(perm)
  }

  property("requiresPermissions returns true when permissions found") = forAll(permission) { perm =>
    val pkg = PackageName.parts("Pkg", "One")
    val name = Identifier.Name("fn")

    val annotations: PermissionAnalyzer.PermissionAnnotations = Map(
      (pkg, name) -> PermissionConfig.require(perm)
    )

    val expr: Matchless.Expr[Unit] = Matchless.Global((), pkg, name)

    PermissionAnalyzer.requiresPermissions(expr, annotations)
  }

  property("requiresPermissions returns false when no permissions") = {
    val expr: Matchless.Expr[Unit] = Matchless.Literal(dev.bosatsu.Lit.Str(""))
    !PermissionAnalyzer.requiresPermissions(expr, Map.empty)
  }

  property("inferPermissionFromName identifies read patterns") = {
    val readNames = List("getUser", "findAccount", "queryOrders", "listItems")
    readNames.forall { name =>
      PermissionAnalyzer.inferPermissionFromName("users", name) match {
        case Some(perm) => perm.action == "read"
        case None => false
      }
    }
  }

  property("inferPermissionFromName identifies create patterns") = {
    val createNames = List("createUser", "addAccount", "insertOrder")
    createNames.forall { name =>
      PermissionAnalyzer.inferPermissionFromName("users", name) match {
        case Some(perm) => perm.action == "create"
        case None => false
      }
    }
  }

  property("inferPermissionFromName identifies update patterns") = {
    val updateNames = List("updateUser", "modifyAccount", "setStatus")
    updateNames.forall { name =>
      PermissionAnalyzer.inferPermissionFromName("users", name) match {
        case Some(perm) => perm.action == "update"
        case None => false
      }
    }
  }

  property("inferPermissionFromName identifies delete patterns") = {
    val deleteNames = List("deleteUser", "removeAccount")
    deleteNames.forall { name =>
      PermissionAnalyzer.inferPermissionFromName("users", name) match {
        case Some(perm) => perm.action == "delete"
        case None => false
      }
    }
  }

  property("inferPermissionFromName returns None for unknown patterns") = {
    val unknownNames = List("process", "handle", "execute", "run")
    unknownNames.forall { name =>
      PermissionAnalyzer.inferPermissionFromName("users", name).isEmpty
    }
  }

  property("analyzeBindings creates map of results") = forAll(permission) { perm =>
    val pkg = PackageName.parts("Pkg", "One")
    val globalName = Identifier.Name("globalFn")
    val bindName = Identifier.Name("myFn")

    val annotations: PermissionAnalyzer.PermissionAnnotations = Map(
      (pkg, globalName) -> PermissionConfig.require(perm)
    )

    val expr: Matchless.Expr[Unit] = Matchless.Global((), pkg, globalName)
    val bindings: List[(Bindable, Matchless.Expr[Unit])] = List((bindName, expr))

    val results = PermissionAnalyzer.analyzeBindings(bindings, annotations)

    results.contains(bindName) &&
      results(bindName).staticRequirements.contains(perm)
  }

  property("analysis result merges correctly") = forAll(permission, permission) { (perm1, perm2) =>
    val result1 = PermissionAnalyzer.AnalysisResult.static(List(perm1))
    val result2 = PermissionAnalyzer.AnalysisResult.static(List(perm2))
    val merged = result1.merge(result2)

    merged.staticRequirements.contains(perm1) &&
      merged.staticRequirements.contains(perm2)
  }

  property("analysis result to manifest preserves permissions") = forAll(permission) { perm =>
    val result = PermissionAnalyzer.AnalysisResult.static(List(perm))
    val manifest = result.toManifest("test")

    manifest.name == "test" &&
      manifest.staticPermissions.contains(perm)
  }
}
