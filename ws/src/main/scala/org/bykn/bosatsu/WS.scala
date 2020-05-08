package org.bykn.bosatsu

import cats.effect._
import cats.implicits._
import cats.data.{NonEmptyList}

import org.http4s.{HttpRoutes, Header}
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze._
import org.http4s.server.middleware._
import org.http4s.util.CaseInsensitiveString

import com.monovore.decline.{Command, Opts}
import java.nio.file.{Path => JPath}

import org.http4s.circe.jsonOf

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

import PathModule.MainCommand.{MainIdentifier, PackageResolver, NEvaluate}

case class WebServer(inputs: PathGen[IO, JPath], log: Option[JPath]) {
  private implicit val keysDecoder = jsonOf[IO, List[String]]
  private val methodConfig = CORSConfig(
    anyOrigin = true,
    anyMethod = true,
    allowCredentials = false,
    maxAge = 1.day.toSeconds
  )
  val service = CORS(
    HttpRoutes
      .of[IO] {
        case req @ OPTIONS -> _ => {
          Ok()
            .map(_.putHeaders(Header("Access-Control-Allow-Origin", "*")))
            .map { res =>
              req.headers.get(
                CaseInsensitiveString("Access-Control-Request-Headers")
              ) match {
                case None => res
                case Some(h) =>
                  res
                    .putHeaders(Header("Access-Control-Allow-Headers", h.value))
              }
            }
        }

        case GET -> "report" /: packageName => {
          NonEmptyList.fromList(packageName.toList) match {
            case None => NoContent()
            case Some(p) =>
              NEvaluate(
                inputs,
                MainIdentifier.FromPackage(PackageName(p), None),
                PathGen.pathGenMonoid.empty,
                LocationMap.Colorize.Console,
                PackageResolver.ExplicitOnly
              ).eval
                .flatMap { _ =>
                  Ok(s"report: ${packageName.toList.mkString("/")}")
                }
          }
        }
        case req @ POST -> Root / "data" =>
          for {
            keys <- req.as[List[String]]
            resp <- Ok(keys.toString)
          } yield (resp)
      },
    methodConfig
  ).orNotFound
}

object WSCommand {
  val PathGen = org.bykn.bosatsu.PathGen
  type PathGen = org.bykn.bosatsu.PathGen[IO, JPath]
  def toList[A](neo: Opts[NonEmptyList[A]]): Opts[List[A]] =
    neo.orNone.map {
      case None     => Nil
      case Some(ne) => ne.toList
    }
  def unfoldDir: Option[JPath => IO[Option[IO[List[JPath]]]]] = Some {
    { path: JPath =>
      IO {
        val f = path.toFile

        if (f.isDirectory()) {
          Some(IO {
            f.listFiles.iterator.map(_.toPath).toList
          })
        } else None
      }
    }
  }

  val opts: Opts[WebServer] = {
    def pathGen(arg: String, help: String, ext: String): Opts[PathGen] = {
      val direct = toList(Opts.options[JPath](arg, help = help))
        .map { paths => paths.foldMap(PathGen.Direct[IO, JPath](_): PathGen) }

      unfoldDir match {
        case None => direct
        case Some(unfold) =>
          val select = PathModule.hasExtension(ext)
          val child1 = toList(
            Opts.options[JPath](arg + "_dir", help = s"all $help in directory")
          ).map { paths =>
            paths.foldMap(
              PathGen
                .ChildrenOfDir[IO, JPath](_, select, false, unfold): PathGen
            )
          }
          val childMany = toList(
            Opts.options[JPath](
              arg + "_all_subdir",
              help = s"all $help recursively in all directories"
            )
          ).map { paths =>
            paths.foldMap(
              PathGen
                .ChildrenOfDir[IO, JPath](_, select, true, unfold): PathGen
            )
          }

          (direct, child1, childMany).mapN { (a, b, c) =>
            (a :: b :: c :: Nil).combineAll
          }
      }
    }

    val ins = pathGen("input", help = "input files", ".bosatsu")
    val log = Opts.option[JPath]("log file", help = "file to log to").orNone
    (ins, log).mapN(WebServer(_, _))
  }

}

object WS extends IOApp {
  def command: Command[WebServer] =
    Command(
      "bosatsu-server",
      "a backend for building hosted reports in bosatsu"
    )(WSCommand.opts)

  def run(args: List[String]): IO[ExitCode] = {
    command.parse(args).map { ws =>
      BlazeServerBuilder[IO](global)
        .bindHttp(8080, "localhost")
        .withHttpApp(ws.service)
        .serve
        .compile
        .drain
        .as(ExitCode.Success)
    } match {
      case Right(ioCode) => ioCode
      case Left(help) =>
        IO(System.err.println(help.toString)).map(_ => ExitCode(1))
    }
  }
}
