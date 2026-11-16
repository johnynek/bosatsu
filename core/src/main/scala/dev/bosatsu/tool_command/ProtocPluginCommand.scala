package dev.bosatsu.tool_command

import cats.data.Chain
import com.monovore.decline.Opts
import dev.bosatsu.{Bytes, PlatformIO}
import dev.bosatsu.protobuf.CodeGenerator
import dev.bosatsu.tool.Output
import cats.syntax.all._

object ProtocPluginCommand {

  def opts[F[_], Path](
      platformIO: PlatformIO[F, Path]
  ): Opts[F[Output[Path]]] = {
    import platformIO.moduleIOMonad

    Opts.subcommand(
      "protoc-plugin",
      "run the protoc stdin/stdout plugin protocol for bosatsu code generation"
    ) {
      Opts(
        platformIO.readStdinBytes.flatMap { requestBytes =>
          val responseBytes = CodeGenerator.generateResponseBytes(requestBytes)
          platformIO
            .writeStdoutBytes(Bytes.unsafeFromArray(responseBytes))
            .as(Output.Many(Chain.empty): Output[Path])
        }
      )
    }
  }
}
