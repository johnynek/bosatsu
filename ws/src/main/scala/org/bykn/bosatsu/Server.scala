package org.bykn.bosatsu

// import cats.{Eval, Id}
import cats.data.{NonEmptyList}
import cats.implicits._
import cats.effect.IO
import com.monovore.decline.{Command, Opts}
import java.nio.file.Path
// import java.util.concurrent.LinkedBlockingQueue
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
  def run: IO[List[String]]
}

object ServerCommand {
  case class WebServer(inputs: NonEmptyList[Path], log: Option[Path]) extends ServerCommand {
    def run = ???
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

object Server {
  def command: Command[ServerCommand] =
    Command("bosatsu-server", "a backend for building hosted reports in bosatsu")(ServerCommand.opts)

  def main(args: Array[String]): Unit =
    command.parse(args.toList) match {
      case Right(cmd) =>
        Try(cmd.run.unsafeRunSync) match {
          case Failure(err) =>
            // TODO use some verbosity flag to modulate this
            err.printStackTrace
            //System.err.println(err.getMessage)
            System.exit(1)
          case Success(lines) =>
            lines.foreach(println)
            System.exit(0)
        }
      case Left(help) =>
        System.err.println(help.toString)
        System.exit(1)
    }

}