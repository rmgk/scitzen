package scitzen.cli

import cats.data.Chain
import scitzen.extern.ImageConverter
import scitzen.generic.{
  Article, ConversionContext, Document, DocumentDirectory, Project, RecursiveArticleIncludeResolver, SastRef
}
import scitzen.outputs.SastToSastConverter
import scitzen.sast.Sast

object Common {

  class PreprocessedResults(
      val directory: DocumentDirectory,
      val labels: Map[String, List[SastRef]],
      val articles: List[Article]
  )

  def preprocessDocuments(
      project: Project,
      imageConverter: ImageConverter,
      unprocessedDocuments: DocumentDirectory
  ): PreprocessedResults = {

    project.cacheDir.createDirectories()

    val initialCtx = ConversionContext(())

    import scala.jdk.CollectionConverters._
    val preprocessedCtxs: List[ConversionContext[(Document, Chain[Sast])]] =
      unprocessedDocuments.documents.asJava.parallelStream().map {
        preprocess(project, initialCtx, imageConverter)
      }.iterator().asScala.toList

    val preprocessedDocuments = splitPreprocessed(preprocessedCtxs)
    val labels = {
      val all     = initialCtx.labelledThings :: preprocessedCtxs.map(_.labelledThings)
      val allKeys = all.iterator.flatMap(_.keysIterator).toSet
      allKeys.iterator.map { key =>
        key -> all.flatMap(_.getOrElse(key, Nil))
      }.toMap
    }
    val articles = preprocessedDocuments.documents.flatMap(Article.articles)
      .map { article =>
        val add = RecursiveArticleIncludeResolver.recursiveIncludes(article, project, preprocessedDocuments)
        article.copy(includes = add)
      }
    new PreprocessedResults(preprocessedDocuments, labels, articles)
  }

  def preprocess(
      project: Project,
      initialCtx: ConversionContext[_],
      imageConverter: ImageConverter
  )(doc: Document): ConversionContext[(Document, Chain[Sast])] = {
    val resCtx = new SastToSastConverter(
      project,
      doc.file,
      doc.reporter,
      Some(imageConverter)
    ).convertSeq(doc.sast)(initialCtx)
    resCtx.execTasks()
    resCtx.map(doc -> _)
  }

  def splitPreprocessed(preprocessedCtxs: List[ConversionContext[(Document, Chain[Sast])]]): DocumentDirectory = {
    DocumentDirectory(preprocessedCtxs.map { ctx =>
      val pd      = ctx.data._1
      val content = ctx.data._2.toList
      pd.copy(sast = content, includes = ctx.includes)
    })
  }
}
