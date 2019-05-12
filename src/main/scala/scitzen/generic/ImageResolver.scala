package scitzen.generic

import better.files.File
import scitzen.parser.Macro
import cats.implicits._


class ImageResolver(val substitutions: Map[File, String]) {
  def image(root: File, target: String): String = substitutions(root / target)
}

object ImageResolver {
  def fromDM(documentManager: DocumentManager): ImageResolver = {
    new ImageResolver(documentManager.documents.flatMap{ pd =>
      pd.sdoc.analyzeResult.macros.collect{
        case Macro("image", attributes) => pd.file.parent/attributes.target
      }
    }.mapWithIndex {
      case (image, index) => image -> s"images/$index${image.extension().getOrElse("")}"
    }.toMap)
  }
}