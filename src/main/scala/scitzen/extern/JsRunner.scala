package scitzen.extern

import org.graalvm.polyglot.*
import org.graalvm.polyglot.proxy.ProxyArray
import scitzen.sast.Attributes

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import scala.util.Try

// should be safe as JS execution seems™ to be fully sandboxed
class JsRunner:

  val engine = Engine.newBuilder().build()

  def run(javascript: String, attributes: Attributes) =
    val os       = ByteArrayOutputStream()
    val ctx      = Context.newBuilder("js").engine(engine).out(os).build()
    val bindings = ctx.getBindings("js")
    attributes.named.foreach((k, v) => bindings.putMember(k, v))
    val argv: Array[String] = attributes.legacyPositional.toArray
    bindings.putMember("argv", ProxyArray.fromArray(argv: _*))
    val ex = Try(ctx.eval("js", javascript))

    s"${os.toString(StandardCharsets.UTF_8)}${ex.fold(t => s"\n$t", _ => "")}"
