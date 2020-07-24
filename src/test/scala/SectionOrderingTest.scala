import org.scalatest.freespec.AnyFreeSpec
import scitzen.parser.{Attributes, Sast}
import scitzen.parser.Sast.Section

class SectionOrderingTest extends AnyFreeSpec {

  def sec(pre: String): Section = Section(Sast.Text(Nil), pre, Attributes.synthetic())


  "ordering matches basic headers" in {
    val sectionlist = List("=", "==", "===", "#", "##", "###").map(sec)
    assert(sectionlist.sorted == sectionlist)
  }

}
