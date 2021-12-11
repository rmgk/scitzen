package scitzen.extern

import java.lang.ProcessBuilder.Redirect
import org.graalvm.polyglot.*
import better.files.*
import org.graalvm.polyglot.proxy.ProxyObject
import scala.jdk.CollectionConverters._

import com.github.plokhotnyuk.jsoniter_scala.core.writeToString

case class KatexConverter(cache: Map[String, String], katex: KatexLibrary):

  def convert(str: String): (String, Option[KatexConverter]) =
    cache.get(str) match
      case Some(res) => (res, None)
      case None =>
        val res   = katex.renderToString(str)
        (res, Some(copy(cache = cache.updated(str, res))))

class KatexLibrary(katexdefs: Option[File]) {
  val katexstr: String = Resource.getAsString("katex.min.js")

  val bindings: Map[String, String] = katexdefs.map { f =>
    f.lineIterator.map(_.split(":", 2)).collect {
      case Array(k, v) => (k, v)
    }.toMap
  }.getOrElse(Map.empty)

  val jsonBindings = writeToString(bindings)(scitzen.compat.Codecs.mapCodec)

  val polyglot = Context.create()
  polyglot.eval("js", katexstr)
  polyglot.getBindings("js").putMember("macro_bindings", jsonBindings)
  polyglot.eval("js", "const katex_settings = {throwOnError: false, macros: JSON.parse(macro_bindings)}")
  val katex = polyglot.eval("js", """s => katex.renderToString(s, katex_settings)""")

  def renderToString(string: String): String =
    katex.execute(string).asString()

}
