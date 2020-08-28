package scitzen.outputs

import scitzen.generic.{Article, HtmlPathManager}
import scitzen.sast.MacroCommand.Other
import scitzen.sast.{Attribute, Attributes, Block, InlineText, Macro, Parsed, Prov, Sast, Section, Text}

object GenIndexPage {

  val months = Array(
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

  def makeIndex(
      articles: List[Article],
      htmlPathManager: HtmlPathManager,
      reverse: Boolean = false
  ): List[Sast] = {
    def ordering[T: Ordering]: Ordering[T] = if (reverse) Ordering[T].reverse else Ordering[T]

    def sectionBy(pdocs: List[Article])(f: Article => String)(cont: List[Article] => List[Sast]): List[Sast] = {
      val sectionTitle = pdocs.groupBy(f)
      sectionTitle.toList.sortBy(_._1)(ordering).flatMap {
        case (key, docs) =>
          val inner = cont(docs)
          List[Sast](
            Section(Text(List(InlineText(key))), prefix = "#", Attributes.synthetic(Attribute("label", key))),
            Block(Attributes.synthetic(), Parsed("", inner))
          )
      }
    }

    def secmon(fd: Article): String = {
      fd.date.fold("") { date =>
        val m = date.date.month
        s"${date.year}-$m " + months(m.toInt - 1)
      }
    }

    sectionBy(articles)(secmon) { idocs =>
      idocs.sortBy(_.date)(ordering).flatMap { doc =>
        val categories =
          List("categories", "people").flatMap(doc.named.get).flatMap(_.split(","))
        List(Macro(
          Other("article"),
          Attributes(
            List(
              Some(Attribute("", doc.header.title.str)),
              Some(Attribute(
                "target",
                htmlPathManager.project.outputdir.relativize(htmlPathManager.articleOutputPath(doc)).toString
              )),
              doc.date.map(date => Attribute("datetime", date.monthDayTime))
            ).flatten ++ categories.map(Attribute("category", _)),
            Prov()
          )
        ))
      }
    }
  }

}
