package scitzen.generic

import better.files.File
import cats.implicits._

case class NLP(stopwords: Map[String, Set[String]], dm: DocumentManager) {

  def noStop(s: String) = stopwords.valuesIterator.forall(stops => !stops.contains(s))


  val totalDocuments = dm.documents.size.toDouble

  lazy val idf = dm.documents.map { doc =>
    doc.sdoc.words.map(_.toLowerCase()).distinct.foldMap(w => Map(w -> 1))
  }.foldMap(identity).mapValues(docWithTerm => Math.log(totalDocuments / docWithTerm))

  def tfidf(words: List[String]) = {
    val size = words.size.toDouble
      words.map(_.toLowerCase()).filter(noStop).foldMap(w => Map(w -> 1))
      .map { case (w, c) => (w, c / size * idf.getOrElse(w, 1d)) }.toSeq.sortBy(-_._2)
  }

  def language(sdoc: Sdoc) = {
    val candidates =
    stopwords.mapValues {
      _.toList.foldMap(sdoc.wordcount.getOrElse(_, 0))
    }.iterator
    (List("" -> 0).iterator ++ candidates).maxBy(_._2)._1
  }

}

object NLP {
  def loadFrom(dir: File, dm: DocumentManager) = {
    val stopwords = dir.glob("stopwords.*").map { sw =>
      val lang = sw.extension(includeDot = false).getOrElse("unknown")
      val words = sw.lines.toSet
      lang -> words
    }.toMap
    NLP(stopwords, dm)
  }
}