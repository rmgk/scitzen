package scitzen.extern

import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import org.graalvm.polyglot.*
import org.jsoup.Jsoup
import scitzen.compat.Logging.cli
import scitzen.project.ProjectPath

import java.io.ByteArrayOutputStream
import java.nio.file.Files
import scala.jdk.CollectionConverters.*
import scala.util.Using

object Katex:
  private val katexstr: String =
    val bo = new ByteArrayOutputStream()
    Using.resource(
      getClass.getClassLoader.getResourceAsStream("META-INF/resources/webjars/katex/0.16.9/dist/katex.min.js")
    ) { r =>
      r.transferTo(bo)
      bo.toString
    }

  // lazily initialize polyglot: pay the cost only once per execution, but
  // prevent native image from trying to compile the js context
  lazy val polyglot: Context =
    val ctx = Context.create()
    ctx.eval("js", katexstr)
    ctx

  class KatexLibrary(katexdefs: Option[ProjectPath]):

    // also lazily initialize katex only when render to string is actually called
    lazy val katex =
      val bindings: Map[String, String] = katexdefs.map { f =>
        Files.lines(f.absolute).iterator().asScala.map(_.split(":", 2)).collect {
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
      cli.trace(s"katex $string")
      val katexBlob = polyglot.synchronized {
        katex.execute(string).asString()
      }
      val doc = Jsoup.parse(katexBlob)
      doc.outputSettings().prettyPrint(false)
      doc.selectFirst("math").html()
