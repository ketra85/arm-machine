import com.ketra85.ArmMachine.Servlet
import org.scalatra._
import javax.servlet.ServletContext

class ScalatraBootstrap extends LifeCycle {
  override def init(context: ServletContext) {
    context.mount(new Servlet, "/*")

    context.initParameters("org.scalatra.HostName") = "AltMachine.local"
    context.initParameters("org.scalatra.Port") = "443"
    context.initParameters("org.scalatra.ForceHttps") = "true"
  }
}
