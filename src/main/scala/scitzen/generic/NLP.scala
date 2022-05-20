package scitzen.generic

import better.files.File
import scitzen.outputs.SastToTextConverter
import scitzen.sast.Sast

case class NLP(stopwords: Map[String, Set[String]]):

  def noStop(s: String) = stopwords.valuesIterator.forall(stops => !stops.contains(s))

  def tfidf(wordlist: List[String], dm: DocumentDirectory) =

    val totalDocuments = dm.documents.size.toDouble

    extension (list: List[String])
      def wordcount: Map[String, Int] = list.foldLeft(Map.empty[String, Int]) {
        case (curr, s) => curr.updatedWith(s) { _.map(_ + 1).orElse(Some(1)) }
      }

    lazy val idf = dm.documents.flatMap { doc =>
      words(doc.sast).distinct
    }.wordcount.view.mapValues(docWithTerm => Math.log(totalDocuments / docWithTerm))

    val size = wordlist.size.toDouble
    wordlist.map(_.toLowerCase()).filter(noStop).wordcount
      .map { case (w, c) => (w, c / size * idf.getOrElse(w, 1d)) }.toSeq.sortBy(-_._2)

  def language(sast: Seq[Sast]): Option[String] =
    val counts = wordcount(sast)
    val candidates =
      stopwords.view.mapValues {
        _.toList.map(counts.getOrElse(_, 0)).sum
      }.iterator
    candidates.maxByOption(_._2).map(_._1)

  def words(sast: Seq[Sast]): List[String] =
    SastToTextConverter().convert(sast).iterator
      .flatMap(_.split("[^\\p{L}]+")).map(_.toLowerCase).toList

  def wordcount(sast: Seq[Sast]): Map[String, Int] =
    words(sast).groupBy(identity).view.mapValues(_.length).toMap

object NLP:
  def loadFrom(dir: File): NLP =
    val stopwords = dir.glob("stopwords._").map { sw =>
      val lang  = sw.extension(includeDot = false).getOrElse("unknown")
      val words = sw.lines.toSet
      lang -> words
    }.toMap
    NLP(stopwords)
