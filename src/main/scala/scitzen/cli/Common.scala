package scitzen.cli

import cats.data.Chain
import scitzen.contexts.SastContext
import scitzen.extern.ImageConverter
import scitzen.generic.{Article, Document, DocumentDirectory, Project, SastRef}
import scitzen.outputs.SastToSastConverter
import scitzen.sast.Sast

object Common {

  case class PreprocessedResults(
      directory: DocumentDirectory,
      labels: Map[String, List[SastRef]],
      articles: List[Article]
  )

  def preprocessDocuments(
      project: Project,
      imageConverter: ImageConverter,
      documents: List[Document]
  ): PreprocessedResults = {

    project.cacheDir.createDirectories()

    val preprocessedCtxs =
      documents.map { doc =>
        new SastToSastConverter(
          project,
          doc.file,
          doc.reporter,
          Some(imageConverter)
        ).convertSeq(doc.sast)(SastContext(()))
      }

    val preprocessedDocuments = DocumentDirectory(preprocessedCtxs.map(_.data).zip(documents).map {
      case (processed, orig) => orig.copy(sast = processed.toList)
    })
    val articles = documents.flatMap(Article.articles)

    PreprocessedResults(preprocessedDocuments, labels(preprocessedCtxs), articles)
  }

  private def labels(preprocessedCtxs: List[SastContext[Chain[Sast]]]): Map[String, List[SastRef]] = {
    val all     = preprocessedCtxs.map(_.labelledThings)
    val allKeys = all.iterator.flatMap(_.keysIterator).toSet
    allKeys.iterator.map { key =>
      key -> all.flatMap(_.getOrElse(key, Nil))
    }.toMap
  }
}
