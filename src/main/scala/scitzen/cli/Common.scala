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
      unprocessedDocuments: DocumentDirectory
  ): PreprocessedResults = {

    project.cacheDir.createDirectories()

    val preprocessedCtxs: List[SastContext[(Document, Chain[Sast])]] =
      unprocessedDocuments.documents.map {
        preprocess(project, imageConverter)
      }

    val preprocessedDocuments = splitPreprocessed(preprocessedCtxs)
    val labels = {
      val all     = preprocessedCtxs.map(_.labelledThings)
      val allKeys = all.iterator.flatMap(_.keysIterator).toSet
      allKeys.iterator.map { key =>
        key -> all.flatMap(_.getOrElse(key, Nil))
      }.toMap
    }
    val articles = preprocessedDocuments.documents.flatMap(Article.articles)
    PreprocessedResults(preprocessedDocuments, labels, articles)
  }

  def preprocess(
      project: Project,
      imageConverter: ImageConverter
  )(doc: Document): SastContext[(Document, Chain[Sast])] = {
    val resCtx = new SastToSastConverter(
      project,
      doc.file,
      doc.reporter,
      Some(imageConverter)
    ).convertSeq(doc.sast)(SastContext(()))
    resCtx.execTasks()
    resCtx.map(doc -> _)
  }

  def splitPreprocessed(preprocessedCtxs: List[SastContext[(Document, Chain[Sast])]]): DocumentDirectory = {
    DocumentDirectory(preprocessedCtxs.map { ctx =>
      val pd      = ctx.data._1
      val content = ctx.data._2.toList
      pd.copy(sast = content)
    })
  }
}
