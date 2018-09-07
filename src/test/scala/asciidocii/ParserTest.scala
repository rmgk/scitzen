package asciidocii

import fastparse.core.Parsed
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FreeSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class ParserTest extends FreeSpec with GeneratorDrivenPropertyChecks {

  "identifier" - {
    import asciidocii.AsciidociiParser.Identifier.identifier.parse
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

  "unquoted" - {
    import fastparse.all._
    val p = AsciidociiParser.unquoted(Seq(AsciidociiParser.sws))
    "anychar" in forAll { c: Char =>
      assert(AnyChar.!.parse(c.toString).get.value === c.toString)
    }

    "any string roundtrip" in forAll { str: String =>
      whenever(str.exists(c => !c.isWhitespace)) {
        assert(p.parse(str).get.value === str)
      }
    }
  }

  "attribute lists" - {
    import asciidocii.AsciidociiParser.Attributes.list.parse
    "empty" in assert(parse("[]").get.value === Seq())
    "empty2" in assert(parse("[ ] ").get.value === Seq())
    "not quite empty" in assert(parse("[ \0] ").get.value === Seq(Attribute("", "\0")))
    "weird whitespace" in assert(parse("[\u001E]").get.value === Seq())
    "positional" in assert(parse("[cheese]").get.value === Seq(Attribute("", "cheese")))
    "many positional" in forAll(Gen.listOf(Arbitrary.arbString.arbitrary)) { attrs: List[String] =>
      val sanitized = attrs.map(_.replace(",", "")
                                 .replace("]", "")
                                 .filter(c => !c.isWhitespace))
                      .filter(_.exists(c => !c.isWhitespace))
      val str = sanitized.mkString("[", ",", "]")
      assert(parse(str).get.value === sanitized.map(v => Attribute("", v)), s"str was: $str with ${sanitized.map{_.map(_.toInt)}} && ${str.map{_.isWhitespace}}")
    }
  }

  "parse document" - {
    import asciidocii.AsciidociiParser.document.parse
    "link" in {
      assert(parse(ExampleFiles.link).get.value ===
             Document(None, Seq(Paragraph("We're parsing link:http://asciidoc.org[AsciiDoc] markup"))))
    }

    "attributed block" in {
      assert(parse(ExampleFiles.attributedParagraph).get.value ===
             Document(None, Seq(BlockWithAttributes(Paragraph("A paragraph"),
                                                    Seq(Attribute("", "someAttribute"),
                                                        Attribute("someOtherAttribute", "test, nochnTest"))))))
    }

  }

}
