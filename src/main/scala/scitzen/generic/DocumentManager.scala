package scitzen.generic

import java.time.format.TextStyle
import java.util.Locale

import scitzen.cli.ParsedDocument
import scitzen.generic.Sast.{MacroBlock, Section, Text}
import scitzen.parser.{Attribute, Attributes, InlineText, Macro}

class DocumentManager(val documents: List[ParsedDocument]) {

  def sectionBy(pdocs: List[ParsedDocument])
               (f: ParsedDocument => String)
               (cont: List[ParsedDocument] => List[Sast]) = {
    val years = pdocs.groupBy(f)
    years.toList.sortBy(_._1).map { case (year, docs) =>
      Section(Text(List(InlineText(year))), cont(docs))
    }
  }

  def secmon(d: ParsedDocument) = {
    d.sdoc.date.fold("(???)"){date =>
      val m = date.date.month
      m +" " + java.time.Month.of(m.toInt).getDisplayName(TextStyle.FULL_STANDALONE, Locale.getDefault)
    }
  }

  def makeIndex(): List[Sast] = {
    sectionBy(documents)(_.sdoc.date.fold("(???)")(_.year)) { docs =>
      sectionBy(docs)(secmon) { idocs =>
        idocs.sortBy(_.sdoc.date).map { doc =>
          MacroBlock(Macro("include",
                           Attributes(List(Attribute("", doc.file.pathAsString)))))
        }
      }
    }
  }

  def mainSast() = makeIndex()
  def find(path: String): Option[ParsedDocument] = documents.find(d => d.file.pathAsString == path)

}
