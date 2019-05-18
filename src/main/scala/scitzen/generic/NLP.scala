package scitzen.generic

import better.files.File
import cats.implicits._

case class NLP(stopwords: Map[String, Set[String]]) {

  def noStop(s: String) = stopwords.valuesIterator.forall(stops => !stops.contains(s))

  def somestuff(dm: DocumentManager) = {
    val docsize = dm.documents.size.toDouble
    val idf = dm.documents.map { doc =>
      doc.sdoc.words.map(_.toLowerCase()).filter(noStop).distinct.foldMap(w => Map(w -> 1))
    }.foldMap(identity).mapValues(_ / docsize)

    val uncomomn = dm.documents.map { doc =>
      doc.sdoc.title ->
      doc.sdoc.words.map(_.toLowerCase()).filter(noStop).foldMap(w => Map(w -> 1))
      .map { case (w, c) => (w, c * idf.getOrElse(w, 1d)) }.toSeq.sortBy(_._2).take(4).map(_._1)
    }

    uncomomn.foreach(println)
  }

  def language(sdoc: Sdoc) = stopwords.mapValues {
    _.toList.foldMap(sdoc.wordcount.getOrElse(_, 0))
  }.maxBy(_._2)._1

}

object NLP {
  def loadFrom(dir: File) = {
    val stopwords = dir.glob("stopwords.*").map { sw =>
      val lang = sw.extension(includeDot = false).getOrElse("unknown")
      val words = sw.lines.toSet
      lang -> words
    }.toMap
    NLP(stopwords)
  }
}