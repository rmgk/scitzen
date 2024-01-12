package sciparse

import de.rmgk.scip.*
import munit.Location
import scitzen.bibliography.BibDB
import scitzen.outputs.AtomToScimConverter
import scitzen.parser.{Atoms, AttributesParser, CommonParsers, DirectiveParsers, Fusion, Parse}
import scitzen.sast.InlineText

import java.nio.charset.StandardCharsets

class SciparseTest extends munit.FunSuite {

  val SastToScimConverter = scitzen.outputs.AtomToScimConverter(BibDB.empty)

  test("basic directive") {
    rewrap(
      """:emph{some plaintext; key= value ; key= "[value]";"[[1, 10, 20, 30, 80]]"  }""",
      """:emph{some plaintext; key=value ; key=value; [1, 10, 20, 30, 80]}"""
    )
  }

  test("nested") {
    rewrap(""":emph{ :{ note } }""", """:emph{:{note } }""")
  }

  test("basic header") {
    rewrap("""= Embed JS
          |sectionstyle = article
          |
          |A Paragraph!""".stripMargin)
  }

  test("multi header") {
    rewrap("""= weihnachtsgrüße!

# Gesamte




Immer
""")
  }

  test("block test") {
    rewrap(raw"""
::figure
	```execute
		stuff
	```
	Just your ordinary Tex in JS in Scim.
::
""")
  }

  test("block figure test") {
    rewrap(s"""
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
""")
  }

  test("block content") {
    rewrap(s"""
```
	a
	}
	b
```
""")
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
    rewrap(s"""
% fonts
:{alias}
% Memoir style

""")
  }

  test("extended alias") {
    rewrap(s"""
Use like this :{someAlias} and and maybe even this :emph{:{note}}.

""")
  }

  def rewrap(input: String, output: String | Null = null)(using Location) =
    try
      val res =
        Fusion.atoms.runInContext(Scx(input).copy(tracing = false))
      // println(res)
      val result    = SastToScimConverter.toScimS(res)
      val resultStr = result.iterator.mkString("", "", "\n")
      assertEquals(resultStr.trim, Option(output).getOrElse(input).trim)
    catch case e: ScipEx => throw new AssertionError(e.getMessage)

  test("more headline weirdness") {
    rewrap(
      s"""# Header

 indented
""", s"""# Header

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

  test("nested lists") {
    rewrap("""• this
             |	• is a nested
             |	• list
             |• with
             |• a couple
             |	• of
             |		• items
             |	• and stuff
             |
             |text after""".stripMargin)
  }

}
