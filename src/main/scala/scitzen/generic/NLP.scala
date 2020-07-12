package scitzen.generic

import better.files.File
import cats.implicits._
import scitzen.outputs.SastToTextConverter
import scitzen.parser.Sast

case class NLP(stopwords: Map[String, Set[String]]) {

  def noStop(s: String) = stopwords.valuesIterator.forall(stops => !stops.contains(s))

  def tfidf(wordlist: List[String], dm: DocumentDirectory) = {

    val totalDocuments = dm.documents.size.toDouble

    lazy val idf = dm.documents.map { doc =>
      words(doc.sast).map(_.toLowerCase()).distinct.foldMap(w => Map(w -> 1))
    }.foldMap(identity).view.mapValues(docWithTerm => Math.log(totalDocuments / docWithTerm))

    val size = wordlist.size.toDouble
    wordlist.map(_.toLowerCase()).filter(noStop).foldMap(w => Map(w -> 1))
      .map { case (w, c) => (w, c / size * idf.getOrElse(w, 1d)) }.toSeq.sortBy(-_._2)
  }

  def language(sast: Seq[Sast]): String = {
    val counts = wordcount(sast)
    val candidates = {
      stopwords.view.mapValues {
        _.toList.foldMap(counts.getOrElse(_, 0))
      }.iterator
    }
    (List("" -> 0).iterator ++ candidates).maxBy(_._2)._1
  }

  def words(sast: Seq[Sast]): List[String] =
    SastToTextConverter().convert(sast)
      .flatMap(_.split("[^\\p{L}]+")).toList

  def wordcount(sast: Seq[Sast]): Map[String, Int] =
    words(sast).foldMap(s => Map(s.toLowerCase() -> 1))

}

object NLP {
  def loadFrom(dir: File, dm: DocumentDirectory) = {
    val stopwords = dir.glob("stopwords.*").map { sw =>
      val lang  = sw.extension(includeDot = false).getOrElse("unknown")
      val words = sw.lines.toSet
      lang -> words
    }.toMap
    NLP(stopwords)
  }
}
