package scitzen.generic

import scitzen.generic.Sast.{Parsed, SBlock, SMacro, Section, Text}
import scitzen.parser.MacroCommand.Ref
import scitzen.parser.{Attribute, Attributes, InlineText, Macro}

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
      dm: DocumentManager,
      project: Project,
      reverse: Boolean = false,
      nlp: Option[NLP] = None
  ): List[Sast] = {
    def ordering[T: Ordering]: Ordering[T] = if (reverse) Ordering[T].reverse else Ordering[T]

    def sectionBy(pdocs: List[FullDoc])(f: FullDoc => String)(cont: List[FullDoc] => List[Sast]): List[Sast] = {
      val sectionTitle = pdocs.groupBy(f)
      sectionTitle.toList.sortBy(_._1)(ordering).flatMap {
        case (key, docs) =>
          val inner = cont(docs).map {
            case s: Section =>
              s.copy(attributes = Attributes.synthetic(Attribute("label", s"$key " + s.attributes.named("label"))))
            case other => other
          }
          List[Sast](
            Section(Text(List(InlineText(key))), prefix = "#", Attributes.synthetic(Attribute("label", key))),
            SBlock(Attributes.synthetic(), Parsed("", inner))
          )
      }
    }

    def secmon(fd: FullDoc): String = {
      fd.analyzed.date.fold("") { date =>
        val m = date.date.month
        m + " " + months(m.toInt - 1)
      }
    }

    sectionBy(dm.fulldocs)(_.analyzed.date.fold("Drafts")(_.year)) { docs =>
      sectionBy(docs)(secmon) { idocs =>
        idocs.sortBy(_.analyzed.date)(ordering).flatMap { doc =>
          List(SMacro(Macro(
            Ref,
            Attributes.synthetic(
              Attribute("", project.root.relativize(doc.parsed.file).toString),
              Attribute("type", "article")
            )
          )),
          //Paragraph(Text(
          //  nlp.toList.flatMap(nl => nl.tfidf(doc.sdoc.words).take(8).map{
          //    case (word, prob) => InlineText(s"$word ")
          //  })))
          )
        }
      }
    }
  }

}
