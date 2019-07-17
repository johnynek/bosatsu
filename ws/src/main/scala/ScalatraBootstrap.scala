import org.scalatra.LifeCycle
import javax.servlet.ServletContext
import org.bykn.bosatsu._

class ScalatraBootstrap extends LifeCycle {

  override def init(context: ServletContext): Unit = {
    context.initParameters("org.scalatra.cors.allowedOrigins") = "http://localhost:8000"
    context.mount(
      new ReactiveBosatsuServlet(
        context.getAttribute("reportBody").asInstanceOf[Seq[String] => String],
        context.getAttribute("cacheResult").asInstanceOf[List[String] => String]),
      "/*"
    )
  }
}
