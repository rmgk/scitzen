package scitzen.generic

import better.files.File
import cats.implicits._
import scitzen.extern.{Graphviz, ImageConvert, TexTikz}
import scitzen.generic.Sast.{RawBlock, TLBlock}
import scitzen.parser.Macro
import kaleidoscope._
import scitzen.parser.MacroCommand.Image

class ImageResolver(val fileSubsts: Map[File, String], val blockSubsts: Map[String, File], cachedir: File) {

  val generated = "generated-graphics/"

  def copyToTarget(imageTarget: File) = {
    fileSubsts.foreach {
      case (source, target) =>
        val tar = imageTarget / target
        tar.parent.createDirectories()
        source.copyTo(tar, overwrite = true)
    }
    blockSubsts.foreach { case (hash, source) =>
      val tar = imageTarget / generated / source.name
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
        case Macro(Image, attributes) => pd.file.parent / attributes.target
      }
    }.mapWithIndex {
      case (image, index) =>
        image ->
        (if (keepName) s"images/${image.name}"
         else s"images/$index${image.extension().getOrElse("")}")
    }.toMap

    val imageBlocks = documentManager.documents.flatMap { pd =>
      val imageBlocks = pd.sdoc.analyzeResult.blocks.collect({
        case tlb@TLBlock(attr, content) if attr.positional.headOption.contains("image") => tlb
      })
      imageBlocks.flatMap { tlb =>
        tlb.attr.named.get("converter") match {
          case Some("tikz")             =>
            val (hash, pdf) = TexTikz.convert(tlb.content.asInstanceOf[RawBlock].content, cachedir)
            val svg = ImageConvert.pdfToSvg(pdf)
            scribe.info(s"converting $hash to $svg")
            List(hash -> svg)
          case Some(gr @ r"graphviz.*") =>
            val (hash, svg) = Graphviz.convert(tlb.content.asInstanceOf[RawBlock].content,
                                               cachedir,
                                               gr.split("\\s+", 2)(1),
                                               "svg")
            scribe.info(s"converting $hash to $svg")
            List(hash -> svg)
          case other                    =>
            scribe.warn(s"unknown converter $tlb")
            Nil
        }
      }
    }.toMap


    new ImageResolver(imageMacros, imageBlocks, cachedir)
  }
}