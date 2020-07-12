package scitzen.cli

import cats.data.Chain
import scitzen.cli.ConvertHtml.preprocess
import scitzen.generic.{Article, ConversionContext, Document, DocumentDirectory, Project, RecursiveArticleIncludeResolver}
import scitzen.parser.Sast

object Common {

  class PreprocessedResults(
                             val preprocessedDocuments: DocumentDirectory,
                             val preprocessedCtx: ConversionContext[_],
                             val articles: List[Article]
                           )

  def preprocessDocuments(project: Project): PreprocessedResults = {
    val unprocessedDocuments = DocumentDirectory(project.root)
    scribe.info(s"found ${unprocessedDocuments.documents.size} documents")

    project.outputdir.createDirectories()


    val initialCtx = ConversionContext(())

    import scala.jdk.CollectionConverters._
    val preprocessedCtxs: List[ConversionContext[(Document, Chain[Sast])]] =
      unprocessedDocuments.documents.asJava.parallelStream().map {
        preprocess(project, initialCtx)
      }.iterator().asScala.toList
    val preprocessedDocuments = splitPreprocessed(preprocessedCtxs)
    val preprocessedCtx       = preprocessedCtxs.foldLeft(initialCtx) { case (prev, next) => prev.merge(next) }
    val articles = preprocessedDocuments.documents.flatMap(Article.articles)
                                        .map { article =>
                                          val add = RecursiveArticleIncludeResolver.recursiveIncludes(article, project, preprocessedDocuments)
                                          article.copy(includes = add)
                                        }
    new PreprocessedResults(preprocessedDocuments, preprocessedCtx, articles)
  }


  def splitPreprocessed(preprocessedCtxs: List[ConversionContext[(Document, Chain[Sast])]]): DocumentDirectory = {
    DocumentDirectory(preprocessedCtxs.map { ctx =>
      val pd      = ctx.data._1
      val content = ctx.data._2.toList
      pd.copy(sast = content, includes = ctx.includes)
    })
  }
}
