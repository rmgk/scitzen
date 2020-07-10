package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path

import better.files.File
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import com.monovore.decline.{Command, Opts}
import scitzen.generic.{Sast, SastConverter}
import scitzen.parser._

object JsonSast {

  implicit val saneCharsetDefault: Charset = StandardCharsets.UTF_8

  implicit val SastEncoder: JsonValueCodec[Seq[Sast]] =
    JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))

  val command: Command[Unit] = Command(name = "json", header = "Convert Scim to Json") {

    Opts.arguments[Path](metavar = "paths").map {
      _.map(File(_))
        .filter(_.isRegularFile)
        .foreach { file =>
          val content = file.contentAsString
          val sast    = SastConverter().documentString(content, Prov(0, content.length))
          val target  = file.sibling(file.name + ".json")
          val json    = writeToArray(sast, WriterConfig.withIndentionStep(2))(SastEncoder)
          target.writeByteArray(json)
        }
    }

  }

  def jsonFor(file: File): String = {
    val content = file.contentAsString
    val sast    = SastConverter().documentString(content, Prov(0, content.length))
    writeToString(sast)(SastEncoder)
  }

}
