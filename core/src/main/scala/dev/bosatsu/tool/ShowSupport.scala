package dev.bosatsu.tool

import cats.data.{Validated, ValidatedNel}
import cats.syntax.all._
import com.monovore.decline.Argument
import dev.bosatsu.{
  CompileOptions,
  ExportedName,
  Identifier,
  Matchless,
  Package
}

object ShowSupport {
  final case class Request(
      selection: ShowSelection.Request,
      ir: Output.ShowIr,
      compileOptions: CompileOptions,
      matchlessPassOptions: Matchless.PassOptions,
      packageNamesOnly: Boolean,
      validateTypedExpr: Boolean
  ) {
    def typedPasses: List[CompileOptions.TypedPass] =
      compileOptions.enabledTypedPasses
  }

  private def invalidChoice(
      kind: String,
      value: String,
      valid: List[String]
  ): String =
    s"invalid $kind: $value. Expected one of: ${valid.mkString(", ")}"

  private def argumentFor[A](
      metavar: String,
      kind: String,
      valid: List[String]
  )(
      fromCliName: String => Option[A]
  ): Argument[A] =
    new Argument[A] {
      def defaultMetavar: String = metavar

      def read(
          string: String
      ): ValidatedNel[String, A] =
        fromCliName(string).toValidNel(invalidChoice(kind, string, valid))
    }

  implicit val showIrArgument: Argument[Output.ShowIr] =
    argumentFor(
      "ir",
      "IR",
      Output.ShowIr.values.toList.map(_.cliName)
    )(Output.ShowIr.fromCliName)

  implicit val typedPassArgument: Argument[CompileOptions.TypedPass] =
    argumentFor(
      "typed-pass",
      "typed pass",
      CompileOptions.TypedPass.ordered.map(_.cliName)
    )(CompileOptions.TypedPass.fromCliName)

  implicit val matchlessPassArgument: Argument[Matchless.Pass] =
    argumentFor(
      "matchless-pass",
      "matchless pass",
      Matchless.Pass.ordered.map(_.cliName)
    )(Matchless.Pass.fromCliName)

  def request(
      selection: ShowSelection.Request,
      ir: Output.ShowIr,
      noOpt: Boolean,
      disabledTypedPasses: Set[CompileOptions.TypedPass],
      disabledMatchlessPasses: Set[Matchless.Pass],
      packageNamesOnly: Boolean,
      validateTypedExpr: Boolean
  ): ValidatedNel[String, Request] = {
    val noOptValidation =
      Validated.condNel(
        !(noOpt && disabledTypedPasses.nonEmpty),
        (),
        "cannot combine --no-opt with --disable-typed-pass"
      )
    val matchlessValidation =
      Validated.condNel(
        (ir == Output.ShowIr.Matchless) || disabledMatchlessPasses.isEmpty,
        (),
        "--disable-matchless-pass requires --ir matchless"
      )
    val validateTypedExprValidation =
      Validated.condNel(
        (ir == Output.ShowIr.TypedExpr) || !validateTypedExpr,
        (),
        "--validate-typedexpr requires --ir typedexpr"
      )

    (
      noOptValidation,
      matchlessValidation,
      validateTypedExprValidation
    ).mapN { (_, _, _) =>
      val compileOptions =
        if (noOpt) CompileOptions.NoOptimize
        else
          CompileOptions.fromDisabledTypedPasses(
            disabledTypedPasses,
            CompileOptions.Mode.Emit
          )
      val matchlessPassOptions =
        Matchless.PassOptions(
          Matchless.Pass.defaultSet -- disabledMatchlessPasses
        )

      Request(
        selection = selection,
        ir = ir,
        compileOptions = compileOptions,
        matchlessPassOptions = matchlessPassOptions,
        packageNamesOnly = packageNamesOnly,
        validateTypedExpr = validateTypedExpr
      )
    }
  }

  def matchlessRoots(
      packs: List[Package.Typed[Any]]
  ): Set[(dev.bosatsu.PackageName, Identifier)] =
    packs.iterator
      .flatMap(pack =>
        pack.lets.iterator.map { case (name, _, _) =>
          (pack.name, name: Identifier)
        }
      )
      .toSet

  def typedShowValue(
      packs: List[Package.Typed[Any]],
      ifaces: List[Package.Interface],
      request: Request
  ): Output.ShowValue.Typed =
    Output.ShowValue.Typed(
      packages = packs,
      interfaces = ifaces,
      typedPasses = request.typedPasses,
      packageNamesOnly = request.packageNamesOnly
    )

  def matchlessShowValue(
      packs: List[Package.Typed[Any]],
      request: Request,
      defsFor: Package.Typed[Any] => List[
        (Identifier.Bindable, Matchless.Expr[?])
      ]
  ): Output.ShowValue.Matchless =
    Output.ShowValue.Matchless(
      packages = packs.map { pack =>
        val exportedValues =
          pack.exports.collect {
            case ExportedName.Binding(name, _)     => name
            case ExportedName.Constructor(name, _) => name
          }

        Output.ShowValue.MatchlessPackage(
          name = pack.name,
          imports = pack.imports,
          exportedValues = exportedValues,
          externals = pack.externalDefs,
          defs = defsFor(pack)
        )
      },
      typedPasses = request.typedPasses,
      matchlessPasses = request.matchlessPassOptions.enabledPasses,
      packageNamesOnly = request.packageNamesOnly
    )
}
