package scitzen.outputs

import scitzen.project.{Project, TitledArticle}
import scitzen.sast.{
  Attribute, Attributes, DCommand, Directive, InlineText, Meta, Prov, Sast, ScitzenDateTime, Section, Text
}

import scala.collection.immutable.ArraySeq

object GenIndexPage:

  val months = ArraySeq(
    "Nonuary",
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

  def sectionFor(name: String): Section =
    if name.isBlank
    then Section(Text.empty, prefix = "#", Attributes(Seq(Attribute("unique ref", "unnamed"))), Meta.synth)
    else Section(Text.of(name), prefix = "#", Attributes(Seq(Attribute("unique ref", name))), Meta.synth)

  def sectionTitle(fd: TitledArticle): String =
    fd.header.date.fold("") { date =>
      val m = date.date.month.getOrElse("0")
      s"${date.year}-$m " + months(m.toInt)
    }

  def directiveFor(project: Project, doc: TitledArticle): Directive =
    Directive(
      DCommand.Ref,
      Attributes(List(
        Attribute(doc.header.autolabel),
        Attribute("scope", doc.article.doc.path.projectAbsolute.toString)
      )),
      Meta.synth
    )

  def makeIndex(
      articles: List[TitledArticle],
      project: Project,
  ): List[Sast] =
    if articles.isEmpty then return Nil

    given Ordering[Option[ScitzenDateTime]] = Ordering.Option(Ordering.apply[ScitzenDateTime].reverse)

    val ordered = articles.sortBy(f => (f.header.date, f.header.title))

    def rec(ordered: List[TitledArticle], acc: List[Section | Directive]): List[Sast] =
      ordered match
        case Nil => acc
        case h :: tail =>
          val current         = sectionTitle(h)
          val (include, rest) = tail.span(v => sectionTitle(v) == current)
          val content         = (h :: include).map(art => directiveFor(project, art))
          rec(
            rest,
            ((if current.isBlank
              then content
              else sectionFor(current) :: content): List[Section | Directive])
              .:::(acc)
          )

    rec(ordered, Nil)
