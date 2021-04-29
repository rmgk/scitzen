import org.scalatest.freespec.AnyFreeSpec
import scitzen.sast.{Attributes, Prov, Section, Text}

class SectionOrderingTest extends AnyFreeSpec {

  def sec(pre: String): Section = Section(Text(Nil), pre, Attributes(Nil), Prov())

  "ordering matches basic headers" in {
    val sectionlist = List("=", "==", "===", "#", "##", "###").map(sec)
    assert(sectionlist.sorted == sectionlist)
  }

}
