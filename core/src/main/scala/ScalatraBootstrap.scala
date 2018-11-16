import org.scalatra.LifeCycle
import javax.servlet.ServletContext
import org.bykn.bosatsu._

class ScalatraBootstrap extends LifeCycle {

  override def init(context: ServletContext): Unit = {

    context.mount(new ReactiveBosatsuServlet, "/*")
  }
}
