package sciparse

import scitzen.scipparse.{AttributesParser, DirectiveParsers}
import de.rmgk.scip.*
import scitzen.outputs.SastToScimConverter

class SciparseTest extends munit.FunSuite {

  test("basic") {
    try
      val res = DirectiveParsers.full.run0(Scx(""":emph{some plaintext; key= value ; key= [value] }"""))
      println(res)
      val result    = SastToScimConverter.toScimS(List(res))
      val resultStr = result.iterator.mkString("", "\n", "\n")
      println(resultStr)
    catch case e : ScipEx => throw new AssertionError(e.getMessage)
  }
}
