package org.bykn.bosatsu

// import cats.{Eval, Id}
// import cats.data.{Validated, ValidatedNel, NonEmptyList}
// import cats.implicits._
// import com.monovore.decline._
// import java.nio.file.Path
// import java.util.concurrent.LinkedBlockingQueue
// import java.util.concurrent.atomic.AtomicReference

// import scala.util.Try
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

object ServerCommand {

}

object Server {

}