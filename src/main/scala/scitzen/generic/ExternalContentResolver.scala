package scitzen.generic

import better.files.File
import cats.implicits._
import scitzen.parser.Macro
import scitzen.parser.MacroCommand.Image

case class ExternalContentResolver(fileSubsts: Map[File, String], blockSubsts: Map[String, File], cachedir: File) {

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

object ExternalContentResolver {

  def fromDM(documentManager: DocumentManager, cachedir: File, keepName: Boolean = false): ExternalContentResolver = {
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



    new ExternalContentResolver(imageMacros, Map(), cachedir)
  }
}



import better.files.File
import scitzen.extern.{Graphviz, TexTikz}
import scitzen.generic.RegexContext.regexStringContext
import scitzen.generic.Sast.{MacroBlock, RawBlock, TLBlock}
import scitzen.parser.{Attribute, Macro, MacroCommand}

case class ExternalContentResolver2(project: Project, files: List[File]) {

  def resolve(anchor: File, target: String): Option[(ExternalContentResolver2, String)] = {
    val source = project.findFile(anchor, target)
    val destination = project.relativize(source)
    if (destination.isEmpty) None
    else Some((copy(files = source :: files), destination.get.toString))
  }

  def convert(tlb: TLBlock, formatHint: String): List[Sast] = {

    def makeImageMacro(file: File) =
      List(MacroBlock(Macro(MacroCommand.Image,
                            tlb.attr.append(List(Attribute("", file.toString()))))))

    tlb.attr.named.get("converter") match {
      case Some("tikz")               =>
        val (hash, pdf) = TexTikz.convert(tlb.content.asInstanceOf[RawBlock].content, project.cacheDir)
        scribe.info(s"converting $hash to $pdf")
        makeImageMacro(pdf)
      case Some(gr @ rex"graphviz.*") =>
        val (hash, svg) = Graphviz.convert(tlb.content.asInstanceOf[RawBlock].content,
                                           project.cacheDir,
                                           gr.split("\\s+", 2)(1),
                                           formatHint)
        scribe.info(s"converting $hash to $svg")
        makeImageMacro(svg)
      case other                      =>
        scribe.warn(s"unknown converter $other")
        List(tlb.copy(attr = tlb.attr.copy(raw = tlb.attr.raw.filterNot(_.id == "converter"))))
    }
  }

}
