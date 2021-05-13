package scitzen.cli

import better.files.File
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import scitzen.generic.{Document, Project}
import scitzen.outputs.SastToSastConverter
import scitzen.parser._
import scitzen.sast.{Prov, Sast}

import java.nio.charset.{Charset, StandardCharsets}

object JsonSast {

  implicit val saneCharsetDefault: Charset = StandardCharsets.UTF_8

  implicit val SastEncoder: JsonValueCodec[List[Sast]] =
    JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))

  def jsonFor(file: File, project: Project): String = {
    val content   = file.contentAsString
    val sast      = Parse.documentUnwrap(content, Prov(0, content.length))
    val converter = new SastToSastConverter(Document(file, content, sast.toList), project)
    val converted = converter.run().data.toList
    writeToString(converted)(SastEncoder)
  }

}
