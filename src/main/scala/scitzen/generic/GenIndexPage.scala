package scitzen.generic

import scitzen.generic.Sast.{MacroBlock, Section, TLBlock, Text}
import scitzen.parser.MacroCommand.Include
import scitzen.parser.{Attribute, Attributes, InlineText, Macro}

object GenIndexPage {


  def makeIndex(dm: DocumentManager, reverse: Boolean = false, nlp: Option[NLP] = None): List[TLBlock] = {
    def ordering[T: Ordering]:Ordering[T] = if (reverse) Ordering[T].reverse else Ordering[T]

    def sectionBy(pdocs: List[ParsedDocument])
                 (f: ParsedDocument => String)
                 (cont: List[ParsedDocument] => List[TLBlock]) = {
      val years = pdocs.groupBy(f)
      years.toList.sortBy(_._1)(ordering).map { case (year, docs) =>
        TLBlock.synt(Section(Text(List(InlineText(year))), cont(docs)))
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
          List(MacroBlock(Macro(Include,
                           Attributes.synt(
                             Attribute("", doc.file.pathAsString),
                             Attribute("type", "article")))),
               //Paragraph(Text(
               //  nlp.toList.flatMap(nl => nl.tfidf(doc.sdoc.words).take(8).map{
               //    case (word, prob) => InlineText(s"$word ")
               //  })))
               )
          .map(TLBlock.synt)
        }
      }
    }
  }

}
