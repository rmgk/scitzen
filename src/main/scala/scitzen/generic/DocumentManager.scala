package scitzen.generic

import better.files.File
import scitzen.cli.ParsedDocument
import scitzen.generic.Sast.{MacroBlock, Section, Text}
import scitzen.parser.{Attribute, Attributes, InlineText, Macro}

object Months {
  val en = Array("January", "February", "March", "April", "May", "June",
                 "July", "August", "September", "October", "November", "December")
}

class DocumentManager(_documents: List[ParsedDocument]) {


  def relTargetPath(root: File, post: ParsedDocument) = {
    "posts/" + post.file.name.toString.replace(".scim", ".html")
  }


  def documents: List[ParsedDocument] = _documents
  val byPath: Map[File, ParsedDocument] = _documents.map(pd => pd.file -> pd).toMap


  def find(root: File, path: String): Option[ParsedDocument] = {
    byPath.get(root / path).filter(d => root.isParentOf(d.file))
  }

}

object GenIndexPage {


  def makeIndex(dm: DocumentManager, reverse: Boolean = false): List[Sast] = {
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
        idocs.sortBy(_.sdoc.date)(ordering).map { doc =>
          MacroBlock(Macro("include",
                           Attributes(List(
                             Attribute("", doc.file.pathAsString),
                             Attribute("type", "article")))))
        }
      }
    }
  }

}