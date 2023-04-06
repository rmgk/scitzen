package scitzen.outputs

import scitzen.generic.{Article, HtmlPathManager}
import scitzen.sast.DCommand.Other
import scitzen.sast.{Attribute, Attributes, Block, Directive, InlineText, Parsed, Prov, Sast, Section, Text}

import java.nio.file.Path
import scala.collection.immutable.ArraySeq

object GenIndexPage:

  /** ```scala
    * assert(months(1) == "January")
    * ```
    */
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

  def makeIndex(
      articles: List[Article],
      htmlPathManager: HtmlPathManager,
      indexDir: Path
  ): List[Sast] =
    def ordering[T: Ordering]: Ordering[T] = Ordering[T].reverse

    def sectionBy(pdocs: List[Article])(f: Article => String)(cont: List[Article] => List[Sast]): List[Sast] =
      val sectionTitle = pdocs.groupBy(f)
      sectionTitle.toList.sortBy(_._1)(ordering).flatMap {
        case (key, docs) =>
          val inner = cont(docs)
          List[Sast](
            Section(Text(List(InlineText(key))), prefix = "#", Attributes(Seq(Attribute("unique ref", key))))(Prov()),
            Block(Attributes(Nil), Parsed("", inner), Prov())
          )
      }

    def secmon(fd: Article): String =
      fd.date.fold("Dateless") { date =>
        val m = date.date.month
        s"${date.year}-$m " + months(m.toInt - 1)
      }

    sectionBy(articles)(secmon) { idocs =>
      idocs.sortBy(_.date)(ordering).flatMap { doc =>
        def categories = doc.named.get("tags").iterator.flatMap(_.split(",")).map(_.trim).filter(!_.isBlank).toList
        List(Directive(
          Other("article"),
          Attributes(
            List(
              Some(Attribute("", doc.header.title.plainString)),
              Some(Attribute(
                "target",
                indexDir.relativize(htmlPathManager.articleOutputPath(doc)).toString
              )),
              doc.date.map(date => Attribute("datetime", date.dayTime))
            ).flatten ++ categories.map(Attribute("category", _))
          )
        )(
          Prov()
        ))
      }
    }
