package org.bykn.bosatsu

import cats.Eval
import cats.data.{NonEmptyList}
import cats.implicits._
import com.monovore.decline.{Command, Opts}
import java.nio.file.Path
import java.util.concurrent.LinkedBlockingQueue
// import java.util.concurrent.atomic.AtomicReference

import scala.util.Try
import scala.util.{ Failure, Success, Try }
import org.eclipse.jetty.server.{Server => JettyServer}
import org.eclipse.jetty.servlet.{DefaultServlet}
// , ServletContextHandler}
import org.eclipse.jetty.webapp.WebAppContext
import org.scalatra.servlet.ScalatraListener
import org.scalatra.{CorsSupport, ScalatraServlet}

import io.circe.parser._
// io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

object JettyLauncher { // this is my entry object as specified in sbt project definition
    def startServer(reportBody: Seq[String] => String, cacheResult: List[String] => String) : Unit = {
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

  class ReactiveBosatsuServlet(reportBody: Seq[String] => String, cacheResult: List[String] => String) extends ScalatraServlet with CorsSupport {

    options("/*"){
      response
        .setHeader("Access-Control-Allow-Headers", request.getHeader("Access-Control-Request-Headers"))
      response.setHeader("Access-Control-Allow-Origin", "*")
    }
  
    get("/") {
      "hello!"
    }
  
    get("/report/*") {
      println("report")
      val params: Seq[String] = multiParams("splat").flatMap(_.split("/"))
      response
        .setHeader("Access-Control-Allow-Origin", "*")
      reportBody(params)
    }
  
    post("/cache") {
      response
        .setHeader("Access-Control-Allow-Origin", "*")
      decode[List[String]](request.body) match {
        case Left(e) => s"$e, ${request.body}"
        case Right(keys) => cacheResult(keys)
      }
    }
  }

sealed abstract class ServerCommand {
  def run: LinkedBlockingQueue[ServerResult]
}

object ServerCommand {
  def blockingQueue(serverResult: ServerResult) = {
    val bq: LinkedBlockingQueue[ServerResult] = new LinkedBlockingQueue()
    bq.put(serverResult)
    bq
  }

  case class WebServer(inputs: NonEmptyList[Path], log: Option[Path]) extends ServerCommand {
    def run = {
      val bq: LinkedBlockingQueue[ServerResult] = new LinkedBlockingQueue()
      JettyLauncher.startServer(
      {
        params => "fizz"
      },
      {
        keys => "fuzz"
      }
      )
      bq
    }
  }

  val opts: Opts[ServerCommand] = {

    def toList[A](neo: Opts[NonEmptyList[A]]): Opts[List[A]] = {
      neo.orNone.map {
        case None => Nil
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
  case class Error(code: Int, errLines: List[String], stdOut: List[String] = Nil, intermediate: Boolean = false) extends ServerResult
  case class Success(result: List[String], intermediate: Boolean = false, cache: Map[NormalExpression, Eval[Any]] = Map()) extends ServerResult
}

object Server {
  def command: Command[ServerCommand] =
    Command("bosatsu-server", "a backend for building hosted reports in bosatsu")(ServerCommand.opts)

  @annotation.tailrec
  def runLoop(resultQueue: LinkedBlockingQueue[ServerResult]): Unit = resultQueue.take match {
    case ServerResult.Error(code, errs, stdout, intermediate) =>
      errs.foreach(System.err.println)
      stdout.foreach(println)
      if(intermediate) runLoop(resultQueue) else System.exit(code)
    case ServerResult.Success(lines, intermediate, _) =>
      lines.foreach(println)
      if(intermediate) runLoop(resultQueue) else System.exit(0)
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