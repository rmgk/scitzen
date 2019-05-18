package scitzen.generic

import better.files.File
import cats.implicits._
import scitzen.extern.Tex
import scitzen.generic.Sast.{AttributedBlock, RawBlock}
import scitzen.parser.Macro


class ImageResolver(val fileSubsts: Map[File, String], val blockSubsts: Map[String, File], cachedir: File) {

  val generated = "generated/"

  def copyToTarget(imageTarget: File) = {
    fileSubsts.foreach {
      case (source, target) =>
        val tar = imageTarget / target
        tar.parent.createDirectories()
        source.copyTo(tar, overwrite = true)
    }
    blockSubsts.foreach{ case (hash, source) =>
      val tar = imageTarget/ generated / source.name
      tar.parent.createDirectories()
      source.copyTo(tar, overwrite = true)
    }
  }

  def image(root: File, target: String): String = fileSubsts(root / target)
  def image(hash: String): String = generated + blockSubsts(hash).name
}

object ImageResolver {
  def fromDM(documentManager: DocumentManager, cachedir: File, keepName: Boolean = false): ImageResolver = {
    val imageMacros = documentManager.documents.flatMap { pd =>
      pd.sdoc.analyzeResult.macros.collect {
        case Macro("image", attributes) => pd.file.parent / attributes.target
      }
    }.mapWithIndex {
      case (image, index) =>
        image ->
        (if (keepName) s"images/${image.name}"
         else s"images/$index${image.extension().getOrElse("")}")
    }.toMap

    val imageBlocks = documentManager.documents.flatMap { pd =>
      pd.sdoc.analyzeResult.blocks.collect({
        case AttributedBlock(attr, content)
          if attr.attributes.positional.headOption.contains("image") =>
          val (hash, pdf)    = Tex.convert(content.asInstanceOf[RawBlock].content, cachedir)
          val svg    = Tex.pdfToSvg(pdf)
          hash -> svg
      })
    }.toMap


    new ImageResolver(imageMacros, imageBlocks, cachedir)
  }
}