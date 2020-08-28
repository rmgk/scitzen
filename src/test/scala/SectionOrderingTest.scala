import org.scalatest.freespec.AnyFreeSpec
import scitzen.parser.sast.{Attributes, Section, Text}

class SectionOrderingTest extends AnyFreeSpec {

  def sec(pre: String): Section = Section(Text(Nil), pre, Attributes.synthetic())

  "ordering matches basic headers" in {
    val sectionlist = List("=", "==", "===", "#", "##", "###").map(sec)
    assert(sectionlist.sorted == sectionlist)
  }

}
