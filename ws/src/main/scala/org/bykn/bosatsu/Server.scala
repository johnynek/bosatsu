package org.bykn.bosatsu

import cats.Eval
import cats.data.{NonEmptyList}
import cats.effect.IO
import cats.implicits._
import com.monovore.decline.{Command, Opts}
import java.nio.file.Path
import java.util.concurrent.LinkedBlockingQueue
// import java.util.concurrent.atomic.AtomicReference

import scala.util.{Failure, Success, Try}
import org.eclipse.jetty.server.{Server => JettyServer}
import org.eclipse.jetty.servlet.{DefaultServlet}
// , ServletContextHandler}
import org.eclipse.jetty.webapp.WebAppContext
import org.scalatra.servlet.ScalatraListener
import org.scalatra.{CorsSupport, ScalatraServlet, AsyncResult}
import scala.concurrent.{ExecutionContext, Future}

import io.circe.parser._
// io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

import org.scalatra.FutureSupport

object JettyLauncher { // this is my entry object as specified in sbt project definition
  def startServer(
      reportBody: Seq[String] => IO[String],
      cacheResult: List[String] => String
  ): Unit = {
    val server = new JettyServer(8080)
    val context = new WebAppContext()
    context setContextPath "/"
    context.setResourceBase("src/main/webapp")
    context.addEventListener(new ScalatraListener)
    context.addServlet(classOf[DefaultServlet], "/")
    context.setAttribute("reportBody", reportBody)
    context.setAttribute("cacheResult", cacheResult)

    server.setHandler(context)

    server.start
    server.join
  }
}

class ReactiveBosatsuServlet(
    reportBody: Seq[String] => IO[String],
    cacheResult: List[String] => String
) extends ScalatraServlet with FutureSupport
    with CorsSupport {

  protected implicit def executor: ExecutionContext = ExecutionContext.global

  options("/*") {
    response
      .setHeader(
        "Access-Control-Allow-Headers",
        request.getHeader("Access-Control-Request-Headers")
      )
    response.setHeader("Access-Control-Allow-Origin", "*")
  }

  get("/") {
    "hello!"
  }

  get("/report/*") {
    new AsyncResult {
      val params: Seq[String] = multiParams("splat").flatMap(_.split("/"))
      val is = reportBody(params).unsafeToFuture.andThen {
        case Success(_) => response
          .setHeader("Access-Control-Allow-Origin", "*")
      }
    }
  }

  post("/cache") {
    new AsyncResult {
      val is = Future(decode[List[String]](request.body) match {
        case Left(e)     => s"$e, ${request.body}"
        case Right(keys) => cacheResult(keys)
      }).andThen {
        case Success(_) => response
          .setHeader("Access-Control-Allow-Origin", "*")
      }
    }
  }
}

sealed abstract class ServerCommand {
  def run: LinkedBlockingQueue[ServerResult]
}

object ServerCommand {
  /*def tokenizeValue(v: Evaluation.Value[(NormalExpression, List[Eval[Evaluation.Value[NormalExpression]]])]): String = v match {
    case ConsValue(head, tail) => head.tokenize.zip(tail.tokenize).map { case (h,t) => s"($h,$t)" }.headOption
    case SumValue(varian, value) => value.tokenize.map(vt => s"SV($variant, $vt)")
    case FnValue(toFn, (normalExpression, scope)) => s"Fn($normalExpression, ${scope.map(_.value.tokenize).mkString(",")})"
    case ExternalValue(toAny, tokenizeFn) => tokenizeFn(toAny)
  }*/

  // type ValueType[T] = (NormalExpression, List[Eval[Evaluation.Value[T]]])
  // type Value = Value[ValueType[ValueType[_]]

  def blockingQueue(serverResult: ServerResult) = {
    val bq: LinkedBlockingQueue[ServerResult] = new LinkedBlockingQueue()
    bq.put(serverResult)
    bq
  }

  def typeCheck(inputs: NonEmptyList[Path], ifs: List[Package.Interface]): IO[(PackageMap.Inferred, List[(Path, PackageName)])] =
    MainCommand.typeCheck(inputs, ifs)
  
  type NEValueTag[X] = (NormalExpression, List[Eval[X]])
  val neTokenImplicits: Evaluation.NETokenImplicits[(Declaration, Normalization.NormalExpressionTag)] = Evaluation.NETokenImplicits()(_._2.ne)
  import neTokenImplicits._

  case class WebServer(inputs: NonEmptyList[Path], log: Option[Path])
      extends ServerCommand {
    def result(mainPackage: PackageName) = {
      typeCheck(inputs, Nil).map { case (packs, _) =>
        val normPM = NormalizePackageMap(packs).normalizePackageMap
        val lets = Evaluation(normPM, Predef.jvmExternals[NEValueTag])
          .evaluateLets(mainPackage)
        val typeMap: Map[rankn.Type, TypeRef] = TypeRef.fromTypes(Some(mainPackage), lets.map(_._2._2))
        val bindings = lets.map(let => Json.JArray(List(
          let._1.asString,
          typeMap(let._2._2).toDoc.render(1000),
          let._2._3.toString
        ).map(Json.JString).toVector))
        Json.JArray(bindings.toVector).toDoc.render(100)
      }
    }
/*    
            val ev = Evaluation(packs, Predef.jvmExternals ++ ext)
            println(s"evaluating $mainPackage")
            ev.evaluateLast(mainPackage, combinedCache) match {
              case None => MainResult.Error(1, List(s"found no main expression $mainPackage"))
              case Some((eval, scheme, ne, c)) =>
                cache.put(c)
                val res = eval.value
                println(s"res: $res")
                Evaluation.toJson(res, scheme, packs) match {
                  case None =>
                    MainResult.Error(1, List(s"cannot convert type to Json: $scheme"))
                  case Some(j) =>
                    MainResult.Success(
                      Json.JObject(Map(
                        "type" -> Json.JString("Document"),
                        "variant" -> Json.JString("Document"),
                        "contents" -> Json.JObject(Map(
                          "document" -> Json.JObject(Map(
                            "data" -> j,
                            "expression" -> Json.JString(s"$ne")
                          ))
                        ))
                      )).toDoc.render(100), true)
                }
    */

    def run = {
      val bq: LinkedBlockingQueue[ServerResult] = new LinkedBlockingQueue()
      JettyLauncher.startServer(
        { params =>
          NonEmptyList.fromList(params.toList.filter(_ != "")).map(nel => result(PackageName(nel))).getOrElse(IO.pure("Can't be empty"))
        }, { keys =>
          "fuzz"
        }
      )
      bq
    }
  }

  val opts: Opts[ServerCommand] = {

    def toList[A](neo: Opts[NonEmptyList[A]]): Opts[List[A]] = {
      neo.orNone.map {
        case None     => Nil
        case Some(ne) => ne.toList
      }
    }

    val ins = Opts.options[Path]("input", help = "input files")
    val log = Opts.option[Path]("log file", help = "file to log to").orNone
    (ins, log).mapN(WebServer(_, _))
  }
}

sealed abstract class ServerResult {}
object ServerResult {
  case class Error(
      code: Int,
      errLines: List[String],
      stdOut: List[String] = Nil,
      intermediate: Boolean = false
  ) extends ServerResult
  case class Success(
      result: List[String],
      intermediate: Boolean = false,
      cache: Map[NormalExpression, Eval[Any]] = Map()
  ) extends ServerResult
}

object Server {
  def command: Command[ServerCommand] =
    Command(
      "bosatsu-server",
      "a backend for building hosted reports in bosatsu"
    )(ServerCommand.opts)

  @annotation.tailrec
  def runLoop(resultQueue: LinkedBlockingQueue[ServerResult]): Unit =
    resultQueue.take match {
      case ServerResult.Error(code, errs, stdout, intermediate) =>
        errs.foreach(System.err.println)
        stdout.foreach(println)
        if (intermediate) runLoop(resultQueue) else System.exit(code)
      case ServerResult.Success(lines, intermediate, _) =>
        lines.foreach(println)
        if (intermediate) runLoop(resultQueue) else System.exit(0)
    }

  def main(args: Array[String]): Unit =
    command.parse(args.toList) match {
      case Right(cmd) =>
        Try(runLoop(cmd.run)) match {
          case Failure(err) =>
            err.printStackTrace
            System.exit(1)
          case Success(_) =>
            System.exit(0)
        }
      case Left(help) =>
        System.err.println(help.toString)
        System.exit(1)
    }

}
