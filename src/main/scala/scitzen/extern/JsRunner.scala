package scitzen.extern

import org.graalvm.polyglot.*
import org.graalvm.polyglot.proxy.ProxyArray
import scitzen.sast.{Attribute, Attributes}

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import scala.util.Try

// should be safe as JS execution seems™ to be fully sandboxed
class JsRunner:

  val engine = Engine.newBuilder().build()

  def run(javascript: String, attributes: Attribute) =
    val os       = ByteArrayOutputStream()
    val ctx      = Context.newBuilder("js").engine(engine).out(os).build()
    val bindings = ctx.getBindings("js")
    bindings.putMember("scitzenArgument", attributes.asTarget)
    val ex = Try(ctx.eval("js", javascript))

    s"${os.toString(StandardCharsets.UTF_8)}${ex.fold(t => s"\n$t", _ => "")}"
