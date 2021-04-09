package scitzen.cli

import better.files.File
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import scitzen.contexts.SastContext
import scitzen.generic.Project
import scitzen.generic.Project.ProjectConfig
import scitzen.outputs.SastToSastConverter
import scitzen.parser._
import scitzen.sast.{Prov, Sast}

import java.nio.charset.{Charset, StandardCharsets}

object JsonSast {

  implicit val saneCharsetDefault: Charset = StandardCharsets.UTF_8

  implicit val SastEncoder: JsonValueCodec[List[Sast]] =
    JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))

  def jsonFor(file: File): String = {
    val content   = file.contentAsString
    val sast      = Parse.documentUnwrap(content, Prov(0, content.length))
    val converter = new SastToSastConverter(Project(file.parent, ProjectConfig(), Map.empty), file, _ => "", None)
    val converted = converter.convertSeq(sast)(SastContext(())).data.toList
    writeToString(converted)(SastEncoder)
  }

}
