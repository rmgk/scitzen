package scitzen.cli

import better.files.File
import com.github.plokhotnyuk.jsoniter_scala.core.*
import scitzen.generic.{Document, Project}
import scitzen.outputs.SastToSastConverter
import scitzen.parser.*
import scitzen.sast.{Prov}
import scitzen.compat.Codecs.SastEncoder

import java.nio.charset.{Charset, StandardCharsets}

object JsonSast:

  implicit val saneCharsetDefault: Charset = StandardCharsets.UTF_8

  def jsonFor(file: File, project: Project): String =
    val content   = file.contentAsString
    val sast      = Parse.documentUnwrap(content, Prov(0, content.length))
    val converter = new SastToSastConverter(Document(file, content, sast.toList), project)
    val converted = converter.run().data.toList
    writeToString(converted)(SastEncoder)
