package sciparse

import de.rmgk.scip.*
import munit.Location
import scitzen.bibliography.BibDB
import scitzen.outputs.SastToScimConverter
import scitzen.parser.{Atoms, AttributesParser, CommonParsers, DirectiveParsers, Parse}
import scitzen.sast.InlineText

import java.nio.charset.StandardCharsets

class SciparseTest extends munit.FunSuite {

  val SastToScimConverter = scitzen.outputs.SastToScimConverter(BibDB.empty)

  test("basic directive") {
    try
      val res =
        DirectiveParsers.full.runInContext(
          Scx(""":emph{some plaintext; key= value ; key= "[value]";"[[1, 10, 20, 30, 80]]"  }""").copy(tracing = false)
        )
      //println(res)
      val result    = SastToScimConverter.toScimS(List(res))
      val resultStr = result.iterator.mkString("", "\n", "\n")
      assertEquals(resultStr, ":emph{some plaintext; key=value ; key=value; [1, 10, 20, 30, 80]}\n")
    catch case e: ScipEx => throw new AssertionError(e.getMessage)
  }

  test("nested") {
    try
      val res =
        DirectiveParsers.full.runInContext(Scx(""":emph{ :{ note } }""").copy(tracing = false))
      //println(res)
      val result    = SastToScimConverter.toScimS(List(res))
      val resultStr = result.iterator.mkString("", "\n", "\n")
      assertEquals(resultStr, ":emph{:{note } }\n")
    catch case e: ScipEx => throw new AssertionError(e.getMessage)
  }

  test("basic header") {
    try

      val input =
        """= Embed JS
          |sectionstyle = article
          |
          |A Paragraph!""".stripMargin

      val res       = Parse.parserDocument.runInContext(Scx(input).copy(tracing = false))
      val result    = SastToScimConverter.toScimS(res)
      val resultStr = result.iterator.mkString("", "\n", "")
      assertEquals(resultStr, input)
    catch case e: ScipEx => throw new AssertionError(e.getMessage)
  }

  test("multi header") {
    try

      val input =
        """= weihnachtsgrüße!

# Gesamte




Immer
"""

      val res       = Parse.parserDocument.runInContext(Scx(input).copy(tracing = false))
      val result    = SastToScimConverter.toScimS(res)
      val resultStr = result.iterator.mkString("", "\n", "\n")
      assertEquals(resultStr, input)
    catch case e: ScipEx => throw new AssertionError(e.getMessage)
  }

  test("block test") {
    try
      val input = raw"""
::figure
	```execute
		stuff
	```
	Just your ordinary Tex in JS in Scim.
::
"""
      val res =
        Parse.parserDocument.runInContext(Scx(input).copy(tracing = false))
      //println(res)
      val result    = SastToScimConverter.toScimS(res)
      val resultStr = result.iterator.mkString("", "\n", "\n")
      assertEquals(resultStr.trim, input.trim)
    catch case e: ScipEx => throw new AssertionError(e.getMessage)
  }

  test("block figure test") {
    try
      val input = s"""
= title
date = 12

huh

```code{lang=scala}
	object Prism:
```

# head

hah

::figure
	```code{lang=javascript}
		const Prism = require('prismjs');
	```
::
"""
      val res =
        Parse.parserDocument.runInContext(Scx(input).copy(tracing = false))
      // println(res)
      val result    = SastToScimConverter.toScimS(res)
      val resultStr = result.iterator.mkString("", "\n", "\n")
      assertEquals(resultStr, input)
    catch case e: ScipEx => throw new AssertionError(e.getMessage)
  }

  test("block content") {
    try
      val input = s"""
```
	a
	}
	b
```
"""
      val res =
        Parse.parserDocument.runInContext(Scx(input).copy(tracing = false))
      // println(res)
      val result    = SastToScimConverter.toScimS(res)
      val resultStr = result.iterator.mkString("", "\n", "\n")
      assertEquals(resultStr, input)
    catch case e: ScipEx => throw new AssertionError(e.getMessage)
  }

  test("super basic") {
    import CommonParsers.*
    try
      val str            = """	}
  b
``
"""
      inline def matcher = ((seq("``")))
      val scx            = Scx(str).copy(tracing = false)
      val bytes          = "``".getBytes(StandardCharsets.UTF_8)
      // println(bytes.toList)
      // println(scx.available)
      // println(printCode(matcher))
      // println(matcher.runInContext(scx))

    catch case e: ScipEx => throw new AssertionError(e.getMessage)
  }

  test("alias") {
    try
      val input = s"""
% fonts
:{alias}
% Memoir style

"""
      val res =
        Parse.parserDocument.runInContext(Scx(input).copy(tracing = false))
      //println(res)
      val result    = SastToScimConverter.toScimS(res)
      val resultStr = result.iterator.mkString("", "\n", "\n")
      assertEquals(resultStr, input)
    catch case e: ScipEx => throw new AssertionError(e.getMessage)
  }

  test("extended alias") {
    try
      val input = s"""
Use like this :{someAlias} and and maybe even this :emph{:{note}}.

"""
      val res =
        Parse.parserDocument.runInContext(Scx(input).copy(tracing = false))
      //println(res)
      val result    = SastToScimConverter.toScimS(res)
      val resultStr = result.iterator.mkString("", "\n", "\n")
      assertEquals(resultStr, input)
    catch case e: ScipEx => throw new AssertionError(e.getMessage)
  }

  def rewrap(input: String)(using Location) =
    try
      val res =
        Parse.parserDocument.runInContext(Scx(input).copy(tracing = false))
      //println(res)
      val result    = SastToScimConverter.toScimS(res)
      val resultStr = result.iterator.mkString("", "\n", "\n")
      assertEquals(resultStr.trim, input.trim)
    catch case e: ScipEx => throw new AssertionError(e.getMessage)

  test("more headline weirdness") {
    rewrap(
      s"""# Header

 indented
"""
    )
  }

  test("lists") {
    rewrap("""
    |
    |• a
    |• b
    |• c
    |
    |""".stripMargin)
  }


  test("text parser") {
    val ctx = Scx("actual text\n    \n").copy(tracing = false)
    val res = Atoms.textline.runInContext(ctx)
    assertEquals(res, List(InlineText("actual text", 0)))
  }

}
