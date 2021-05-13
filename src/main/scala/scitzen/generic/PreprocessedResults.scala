package scitzen.generic

import scitzen.extern.ITargetPrediction
import scitzen.outputs.SastToSastConverter

class PreprocessedResults(project: Project, val documents: List[Document]) {

  project.cacheDir.createDirectories()

  val preprocessedCtxs: List[SastToSastConverter#CtxCS] =
    documents.map { doc =>
      new SastToSastConverter(doc, Some(ITargetPrediction(project, doc.file.parent))).run()
    }

  val docCtx: List[(Document, SastToSastConverter#CtxCS)] = documents.zip(preprocessedCtxs)

  val preprocessedDocuments = docCtx.map {
    case (orig, processed) => orig.copy(sast = processed.data.toList)
  }

  val directory: DocumentDirectory = DocumentDirectory(preprocessedDocuments)

  val labels: Map[String, List[SastRef]] = {
    val all     = preprocessedCtxs.map(_.labelledThings)
    val allKeys = all.iterator.flatMap(_.keysIterator).toSet
    allKeys.iterator.map { key =>
      key -> all.flatMap(_.getOrElse(key, Nil))
    }.toMap
  }

  val articles: List[Article] = preprocessedDocuments.flatMap { Article.articles }

}
