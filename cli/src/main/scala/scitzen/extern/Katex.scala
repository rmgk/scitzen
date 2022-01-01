package scitzen.extern

import java.lang.ProcessBuilder.Redirect
import org.graalvm.polyglot.*
import better.files.*
import org.graalvm.polyglot.proxy.ProxyObject

import scala.jdk.CollectionConverters.*
import com.github.plokhotnyuk.jsoniter_scala.core.writeToString

object Katex:
  // this MUST be inlined by native image to work
  private val katexstr: String = Resource.getAsString("scitzen/katex.min.js")

  case class KatexConverter(cache: Map[String, String], katex: KatexLibrary):

    def convert(str: String): (String, Option[KatexConverter]) =
      cache.get(str) match
        case Some(res) => (res, None)
        case None =>
          val res = katex.renderToString(str)
          (res, Some(copy(cache = cache.updated(str, res))))

  end KatexConverter

  // lazily initialize polyglot: pay the cost only once per execution, but
  // prevent native image from trying to compile the js context
  lazy val polyglot: Context =
    val ctx = Context.create()
    ctx.eval("js", katexstr)
    ctx

  class KatexLibrary(katexdefs: Option[File]):

    // also lazily initialize katex only when render to string is actually called
    lazy val katex =
      val bindings: Map[String, String] = katexdefs.map { f =>
        f.lineIterator.map(_.split(":", 2)).collect {
          case Array(k, v) => (k, v)
        }.toMap
      }.getOrElse(Map.empty)

      val jsonBindings = writeToString(bindings)(scitzen.compat.Codecs.mapCodec)


      polyglot.getBindings("js").putMember("macro_bindings", jsonBindings)
      val katexRes = polyglot.eval("js", """
      {const katex_settings = {throwOnError: false, macros: JSON.parse(macro_bindings)};
      s => katex.renderToString(s, katex_settings) }
      """)
      polyglot.getBindings("js").removeMember("macro_bindings")
      katexRes

    def renderToString(string: String): String =
      scribe.info(s"compiling $string")
      katex.execute(string).asString()
