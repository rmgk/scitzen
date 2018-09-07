package asciidocii

import fastparse.core.Parsed
import org.scalatest.FreeSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class ParserTest extends FreeSpec with GeneratorDrivenPropertyChecks {

  "identifier" - {
    import asciidocii.AsciidociiParser.IdentifierP.identifier.parse
    "simple" in {assert(parse("Good123_-test").get.value === "Good123_-test")}
    "fail" in {assert(parse("-Bad").isInstanceOf[Parsed.Failure[_, _]])}
  }

  "quoted" - {
    import asciidocii.AsciidociiParser.quoted
    "string" in assert {quoted("'").parse("""'success! :D'""").get.value === "success! :D"}
    "parens" in assert {quoted(")", open = Some("(")).parse("""(success! :D)""").get.value === "success! :D"}
    "escapes" in assert {quoted("'").parse("""'success! \a \\ \' '""").get.value === """success! \a \ ' """}
    "escape stack" in assert {quoted("'").parse("""'success! \a \\\\\\\''""").get.value === """success! \a \\\'"""}
    "escape parens" in assert {
      quoted(")", open = Some("(")).parse("""(success! \\ \) )""").get
      .value === """success! \ ) """
    }
    "horror" in assert {quoted(")", open = Some("(")).parse("""(success! \\\)\\)""").get.value === """success! \)\"""}
    "degenerate" in assert {quoted("\\").parse("""\success!\""").get.value === """success!"""}
    "degenerate horror" in assert {quoted("\\").parse("""\\\success!\\\\\""").get.value === """\success!\\"""}
  }

  "parse document" - {
    import AsciidociiParser.document.parse
    "link" in {
      assert(parse(ExampleFiles.link).get.value ===
             Document(None, Seq(Paragraph("We're parsing link:http://asciidoc.org[AsciiDoc] markup"))))
    }

    "attributed block" in {
      assert(parse(ExampleFiles.attributedParagraph).get.value ===
             Document(None, Seq(BlockWithAttributes(Paragraph("A paragraph"),
                                                    Seq(Attribute("someAttribute", None),
                                                        Attribute("someOtherAttribute", Some("test, nochnTest")))))))
    }

  }

}
