package scitzen.generic

import scitzen.outputs.SastToSastConverter

import java.nio.file.Files

class PreprocessedResults(project: Project, documents: List[Document]):

  Files.createDirectories(project.cacheDir)

  val preprocessedCtxs: List[SastToSastConverter#CtxCS] =
    documents.map { doc =>
      new SastToSastConverter(doc, project).run()
    }

  val docCtx: List[(Document, SastToSastConverter#CtxCS)] = documents.zip(preprocessedCtxs)

  val preprocessedDocuments = docCtx.map {
    case (orig, processed) => orig.copy(sast = processed.data.toList)
  }

  val directory: DocumentDirectory = DocumentDirectory(preprocessedDocuments)

  val labels: Map[String, List[SastRef]] =
    val all     = preprocessedCtxs.map(_.labelledThings)
    val allKeys = all.iterator.flatMap(_.keysIterator).toSet
    allKeys.iterator.map { key =>
      key -> all.flatMap(_.getOrElse(key, Nil))
    }.toMap

  val articles: List[Article]     = preprocessedDocuments.flatMap(Article.articles)
  val articleItems: List[Article] = preprocessedDocuments.flatMap(ArticleItem.items)

  lazy val itemsAndArticlesByLabel: Map[String, Article] = (articles ++ articleItems).map(a => (a.header.autolabel, a)).toMap
