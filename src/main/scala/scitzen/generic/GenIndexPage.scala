package scitzen.generic

import scitzen.generic.Sast.{MacroBlock, Section, Text}
import scitzen.parser.MacroCommand.Include
import scitzen.parser.{Attribute, Attributes, InlineText, Macro}

object GenIndexPage {


  def makeIndex(dm: DocumentManager, project: Project, reverse: Boolean = false, nlp: Option[NLP] = None): List[Section] = {
    def ordering[T: Ordering]:Ordering[T] = if (reverse) Ordering[T].reverse else Ordering[T]

    def sectionBy(pdocs: List[FullDoc])
                 (f: FullDoc => String)
                 (cont: List[FullDoc] => List[Sast])
    : List[Section] = {
      val years = pdocs.groupBy(f)
      years.toList.sortBy(_._1)(ordering).map { case (year, docs) =>
        Section(Text(List(InlineText(year))), cont(docs), Attributes.synt())
      }
    }

    def secmon(fd: FullDoc): String = {
      fd.analyzed.date.fold("(???)"){ date =>
        val m = date.date.month
        m +" " + Months.en(m.toInt - 1)
      }
    }


    sectionBy(dm.fulldocs)(_.analyzed.date.fold("(???)")(_.year)) { docs =>
      sectionBy(docs)(secmon) { idocs =>
        idocs.sortBy(_.analyzed.date)(ordering).flatMap { doc =>
          List(MacroBlock(Macro(Include,
                           Attributes.synt(
                             Attribute("", project.root.relativize(doc.parsed.file).toString),
                             Attribute("type", "article")))),
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
