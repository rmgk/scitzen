package asciidocii

import fastparse.core.Parsed
import org.scalatest.FreeSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class ParserTest extends FreeSpec with GeneratorDrivenPropertyChecks {

  "identifier" - {
    import asciidocii.AsciidociiParser.Identifier.identifier.parse
    "simple" in { assert(parse("Good123_-test").get.value === AttributeName("Good123_-test")) }
    "fail" in { assert(parse("-Bad").isInstanceOf[Parsed.Failure[_, _]] )}
  }

  "quoted" - {
    import asciidocii.AsciidociiParser.quoted
    "string" in assert { quoted("'").parse("""'success! :D'""").get.value === "success! :D"}
    "parens" in assert { quoted(")", open = Some("(")).parse("""(success! :D)""").get.value === "success! :D"}
    "escapes" in assert { quoted("'").parse("""'success! \a \\ \' '""").get.value === """success! \a \ ' """}
    "escape stack" in assert { quoted("'").parse("""'success! \a \\\\\\\''""").get.value === """success! \a \\\'"""}
    "escape parens" in assert { quoted(")", open = Some("(")).parse("""(success! \\ \) )""").get.value === """success! \ ) """}
    "horror" in assert { quoted(")", open = Some("(")).parse("""(success! \\\)\\)""").get.value === """success! \)\"""}
    "degenerate" in assert { quoted("\\").parse("""\success!\""").get.value === """success!"""}
    "degenerate horror" in assert { quoted("\\").parse("""\\\success!\\\\\""").get.value === """\success!\\"""}
  }

  "parse links" - {
    "simple" in {
      println(AsciidociiParser.document.parse(ExampleFiles.link).get.value)
    }
  }

}
