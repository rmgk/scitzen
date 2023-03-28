package scitzen.extern

import java.lang.ProcessBuilder.Redirect
import org.graalvm.polyglot.*
import org.graalvm.polyglot.proxy.ProxyObject
import scitzen.compat.Logging.scribe

import scala.jdk.CollectionConverters.*
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToString}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import org.jsoup.Jsoup

import java.io.ByteArrayOutputStream
import java.nio.file.{Files, Path}
import scala.util.Using

object Katex:

  given mapCodec: JsonValueCodec[Map[String, String]] = JsonCodecMaker.make

  private val katexstr: String =
    val bo = new ByteArrayOutputStream()
    Using.resource(
      getClass.getClassLoader.getResourceAsStream("META-INF/resources/webjars/katex/0.16.4/dist/katex.min.js")
    ) { r =>
      r.transferTo(bo)
      bo.toString
    }

  case class KatexConverter(cache: Map[String, String], katex: KatexLibrary):

    def convert(str: String): (String, Option[KatexConverter]) =
      cache.get(str) match
        case Some(res) => (res, None)
        case None =>
          val katexBlob = katex.renderToString(str)
          val doc       = Jsoup.parse(katexBlob)
          doc.outputSettings().prettyPrint(false)
          val res = doc.selectFirst("math").html()
          (res, Some(copy(cache = cache.updated(str, res))))

  end KatexConverter

  // lazily initialize polyglot: pay the cost only once per execution, but
  // prevent native image from trying to compile the js context
  lazy val polyglot: Context =
    val ctx = Context.create()
    ctx.eval("js", katexstr)
    ctx

  class KatexLibrary(katexdefs: Option[Path]):

    // also lazily initialize katex only when render to string is actually called
    lazy val katex =
      val bindings: Map[String, String] = katexdefs.map { f =>
        Files.lines(f).iterator().asScala.map(_.split(":", 2)).collect {
          case Array(k, v) => (k, v)
        }.toMap
      }.getOrElse(Map.empty)

      val jsonBindings = writeToString(bindings)

      polyglot.getBindings("js").putMember("macro_bindings", jsonBindings)
      val katexRes = polyglot.eval(
        "js",
        """
      {const katex_settings = {throwOnError: false, macros: JSON.parse(macro_bindings)};
      s => katex.renderToString(s, katex_settings) }
      """
      )
      polyglot.getBindings("js").removeMember("macro_bindings")
      katexRes

    def renderToString(string: String): String =
      scribe.info(s"compiling $string")
      katex.execute(string).asString()
