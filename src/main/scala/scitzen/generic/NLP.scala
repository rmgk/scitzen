package scitzen.generic

import scitzen.cli.ConversionAnalysis
import scitzen.contexts.ConversionContext
import scitzen.extern.ResourceUtil
import scitzen.outputs.SastToTextConverter
import scitzen.sast.Attributes

import java.nio.charset.StandardCharsets
import java.nio.file.{FileVisitOption, Files, Path}
import scala.jdk.CollectionConverters.*

case class NLP(stopwords: Map[String, Seq[String]], analysis: ConversionAnalysis):

  def noStop(s: String) = stopwords.valuesIterator.forall(stops => !stops.contains(s))

  def tfidf(wordlist: List[String], articleDirectory: ArticleDirectory) =

    val totalDocuments = articleDirectory.articles.size.toDouble

    extension (list: List[String])
      def wordcount: Map[String, Int] = list.foldLeft(Map.empty[String, Int]) {
        case (curr, s) => curr.updatedWith(s) { _.map(_ + 1).orElse(Some(1)) }
      }

    lazy val idf = articleDirectory.articles.flatMap { art =>
      words(art).distinct
    }.wordcount.view.mapValues(docWithTerm => Math.log(totalDocuments / docWithTerm))

    val size = wordlist.size.toDouble
    wordlist.map(_.toLowerCase()).filter(noStop).wordcount
      .map { case (w, c) => (w, c / size * idf.getOrElse(w, 1d)) }.toSeq.sortBy(-_._2)

  def language(article: Article): Option[String] =
    val counts = wordcount(article)
    val candidates =
      stopwords.view.mapValues {
        _.iterator.map(counts.getOrElse(_, 0)).sum
      }.iterator
    candidates.maxByOption(_._2).map(_._1)

  def words(article: Article): List[String] =
    SastToTextConverter(
      article.ref,
      analysis,
      Attributes(analysis.project.config.settings ++ article.titled.map(_.attributes.raw).getOrElse(Nil))
    )
      .convertSastSeq(ConversionContext(()), article.sast).data
      .iterator
      .flatMap(_.split("[^\\p{L}]+")).map(_.toLowerCase).toList

  def wordcount(article: Article): Map[String, Int] =
    words(article).groupBy(identity).view.mapValues(_.length).toMap

object NLP:
  def loadFrom(dir: Path, analysis: ConversionAnalysis): NLP =
    val stopwords = Files.walk(dir).iterator().asScala.filter(p => p.getFileName.startsWith("stopwords")).map { sw =>
      val lang  = sw.toString.takeRight(2)
      val words = Files.lines(sw).iterator().asScala.toSeq
      lang -> words
    }.toMap
    NLP(stopwords, analysis)

  def loadFromResources(analysis: ConversionAnalysis): NLP =
    val stopwords = List("de", "en").map: lang =>
      val bytes = ResourceUtil.load(s"stopwords.$lang")
      val words = new String(bytes, StandardCharsets.UTF_8).linesIterator.toSeq
      lang -> words
    NLP(stopwords.toMap, analysis)
