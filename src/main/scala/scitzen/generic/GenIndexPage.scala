package scitzen.generic

import scitzen.cli.ParsedDocument
import scitzen.generic.Sast.{MacroBlock, ParsedBlock, Section, Text}
import scitzen.parser.{Attribute, Attributes, InlineText, Macro}

object GenIndexPage {


  def makeIndex(dm: DocumentManager, reverse: Boolean = false, nlp: Option[NLP] = None): List[Sast] = {
    def ordering[T: Ordering]:Ordering[T] = if (reverse) Ordering[T].reverse else Ordering[T]

    def sectionBy(pdocs: List[ParsedDocument])
                 (f: ParsedDocument => String)
                 (cont: List[ParsedDocument] => List[Sast]) = {
      val years = pdocs.groupBy(f)
      years.toList.sortBy(_._1)(ordering).map { case (year, docs) =>
        Section(Text(List(InlineText(year))), cont(docs))
      }
    }

    def secmon(d: ParsedDocument) = {
      d.sdoc.date.fold("(???)"){date =>
        val m = date.date.month
        m +" " + Months.en(m.toInt - 1)
      }
    }


    sectionBy(dm.documents)(_.sdoc.date.fold("(???)")(_.year)) { docs =>
      sectionBy(docs)(secmon) { idocs =>
        idocs.sortBy(_.sdoc.date)(ordering).flatMap { doc =>
          List(MacroBlock(Macro("include",
                           Attributes(List(
                             Attribute("", doc.file.pathAsString),
                             Attribute("type", "article"))))),
               ParsedBlock("", List(Text(
                 nlp.toList.flatMap(nl => nl.tfidf(doc.sdoc.words).take(8).map{
                   case (word, prob) => InlineText(s"$word ")
                 }))
                                    )))
        }
      }
    }
  }

}