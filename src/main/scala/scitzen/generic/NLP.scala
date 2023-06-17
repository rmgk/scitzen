package scitzen.generic

import scitzen.extern.ResourceUtil
import scitzen.outputs.SastToTextConverter
import scitzen.sast.Sast

import java.nio.charset.StandardCharsets
import java.nio.file.{FileVisitOption, Files, Path}
import scala.jdk.CollectionConverters.*

case class NLP(stopwords: Map[String, Set[String]]):

  def noStop(s: String) = stopwords.valuesIterator.forall(stops => !stops.contains(s))

  def tfidf(wordlist: List[String], articleDirectory: ArticleDirectory) =

    val totalDocuments = articleDirectory.articles.size.toDouble

    extension (list: List[String])
      def wordcount: Map[String, Int] = list.foldLeft(Map.empty[String, Int]) {
        case (curr, s) => curr.updatedWith(s) { _.map(_ + 1).orElse(Some(1)) }
      }

    lazy val idf = articleDirectory.articles.flatMap { art =>
      words(art.content).distinct
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
    SastToTextConverter(Map.empty, new ArticleDirectory(Nil)).convert(sast).iterator
      .flatMap(_.split("[^\\p{L}]+")).map(_.toLowerCase).toList

  def wordcount(sast: Seq[Sast]): Map[String, Int] =
    words(sast).groupBy(identity).view.mapValues(_.length).toMap

object NLP:
  def loadFrom(dir: Path): NLP =
    val stopwords = Files.walk(dir).iterator().asScala.filter(p => p.getFileName.startsWith("stopwords")).map { sw =>
      val lang  = sw.toString.takeRight(2)
      val words = Files.lines(sw).iterator().asScala.toSet
      lang -> words
    }.toMap
    NLP(stopwords)

  def loadFromResources: NLP =
    val stopwords = List("de", "en").map: lang =>
      val bytes = ResourceUtil.load(s"stopwords.$lang")
      val words = new String(bytes, StandardCharsets.UTF_8).linesIterator.toSet
      lang -> words
    NLP(stopwords.toMap)
