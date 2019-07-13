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
// import org.scalatra.{CorsSupport, ScalatraServlet}

// import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

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

object ServerCommand {

}

object Server {

}