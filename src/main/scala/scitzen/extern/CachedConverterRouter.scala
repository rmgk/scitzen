package scitzen.extern

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, WriterConfig, readFromStream, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import scitzen.compat.Logging
import scitzen.extern.Katex.KatexLibrary

import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicReference
import scala.util.Using

given mapCodec: JsonValueCodec[Map[String, String]] = JsonCodecMaker.make


class CachedConverterRouter(cachePath: Path, katexLibrary: KatexLibrary):

  val cache: AtomicReference[Map[String, String]] = AtomicReference:
    val start = System.nanoTime()
    val res = Using(Files.newInputStream(cachePath)) { is => readFromStream[Map[String, String]](is) }.getOrElse(Map())
    Logging.cli.info(s"loading cache took ${(System.nanoTime()-start)/1000000}ms")
    res

  def convert(lang: String, str: String): String =
    val cached = cache.get()
    val key    = lang + str
    cached.get(key) match
      case Some(res) => res
      case None =>
        val res = lang match
          case "katex" => katexLibrary.renderToString(str)
          case other   => Prism.highlight(str, lang)
        cache.updateAndGet(cur => cur.updated(key, res))
        res

  def writeCache(): Unit =
    Files.createDirectories(cachePath.getParent)
    Files.write(
      cachePath,
      writeToArray[Map[String, String]](
        cache.get,
        WriterConfig.withIndentionStep(2)
      )(mapCodec)
    )
    ()
