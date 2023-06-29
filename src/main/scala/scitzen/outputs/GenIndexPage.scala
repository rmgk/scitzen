package scitzen.outputs

import scitzen.generic.{Project, TitledArticle}
import scitzen.sast.DCommand.Other
import scitzen.sast.{Attribute, Attributes, DCommand, Directive, InlineText, Prov, Sast, ScitzenDateTime, Section, Text}

import java.nio.file.Path
import scala.collection.immutable.ArraySeq

object GenIndexPage:

  val months = ArraySeq(
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December"
  )

  def sectionFor(name: String) =
    if name.isBlank
    then Section(Text(List()), prefix = "#", Attributes(Seq(Attribute("unique ref", "unnamed"))))(Prov())
    else Section(Text(List(InlineText(name))), prefix = "#", Attributes(Seq(Attribute("unique ref", name))))(Prov())

  def sectionTitle(fd: TitledArticle): String =
    fd.header.date.fold("") { date =>
      val m = date.date.month
      s"${date.year}-$m " + months(m.toInt - 1)
    }

  def directiveFor(project: Project, indexDir: Path, doc: TitledArticle): Directive =
    def categories = doc.attr.plain("tags").iterator.flatMap(_.split(",")).map(_.trim).filter(!_.isBlank).toList
    Directive(
      Other("article"),
      Attributes(
        List(
          Some(Attribute(doc.header.titleText)),
          Some(Attribute(indexDir.relativize(project.htmlPaths.articleOutputPath(doc.header)).toString)),
          doc.date.map(date => Attribute("datetime", date.dayTime))
        ).flatten ++ categories.map(Attribute("category", _))
      )
    )(Prov())

  def makeIndex(
      articles: List[TitledArticle],
      project: Project,
      indexDir: Path
  ): List[Sast] =
    if articles.isEmpty then return Nil

    given Ordering[Option[ScitzenDateTime]] = Ordering.Option(Ordering.apply[ScitzenDateTime].reverse)

    val ordered = articles.sortBy(f => (f.date, f.title))

    def rec(ordered: List[TitledArticle], acc: List[Sast]): List[Sast] =
      ordered match
        case Nil => acc
        case h :: tail =>
          val current         = sectionTitle(h)
          val (include, rest) = tail.span(v => sectionTitle(v) == current)
          val content         = (h :: include).map(art => directiveFor(project, indexDir, art))
          rec(
            rest,
            (if current.isBlank
             then content
             else sectionFor(current) :: content)
              .:::(acc)
          )

    rec(ordered, Nil)
