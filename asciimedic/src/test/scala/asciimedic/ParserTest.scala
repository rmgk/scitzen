package asciimedic

import fastparse.core.Parsed
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FreeSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class ParserTest extends FreeSpec with GeneratorDrivenPropertyChecks {

  "identifier" - {
    import asciimedic.Asciimedic.Identifier.identifier.parse
    "simple" in {assert(parse("Good123_-test").get.value === "Good123_-test")}
    "fail" in {assert(parse("-Bad").isInstanceOf[Parsed.Failure[_, _]])}
  }

  "quoted" - {
    import asciimedic.Asciimedic.quoted
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
    val p = Asciimedic.until(Asciimedic.saws)
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
    import asciimedic.Asciimedic.Attributes.list.parse

    def sanitize(attrs: List[String]): List[String] = {
      attrs.map(_.replace(",", "")
                 .replace("]", "")
                 .filter(c => !c.isWhitespace))
      .filter(_.exists(c => !c.isWhitespace))
    }


    def assertPositional(seq: Seq[String]) = {
      val str = seq.mkString("[", ",", "]")
      assert(parse(str).get.value === seq.map(v => Attribute("", v)), s"str was: $str")
    }
    "positional" - {
      "empty" in assert(parse("[]").get.value === Seq())
      "empty2" in assert(parse("[ ] ").get.value === Seq())
      "positional" in assert(parse("[cheese]").get.value === Seq(Attribute("", "cheese")))
      "not quite empty" in assert(parse("[ \0] ").get.value === Seq(Attribute("", "\0")))
      "weird whitespace" in assertPositional(Seq("\u001E"))
      "weird quotes" in assertPositional(Seq("a\"value"))
      "many positional" in forAll(Gen.listOf(Arbitrary.arbString.arbitrary)) { attrs: List[String] =>
        assertPositional(sanitize(attrs))
      }
    }
    "named" - {
      "one with unquoted" in forAll { str: String =>
        val sanitized = sanitize(List(str))
        whenever(sanitized.nonEmpty) {
          val san = sanitized.head
          val atttrs = s"[val = $san]"
          assert(parse(atttrs).get.value === Seq(Attribute("val", s"$san")))
        }
      }
    }
  }


  "parse document" - {
    import asciimedic.Asciimedic.document.parse
    "link" in {
      pprint.pprintln(parse(ExampleFiles.link).get.value)
      assert(
        parse(ExampleFiles.link).get.value ===
        Document(
          None,
          Seq(
            Paragraph(
              Seq(
                InlineText("We're "),
                InlineText("parsing "),
                InlineMacro("link", "http://asciidoc.org", Seq(Attribute("", "AsciiDoc"))),
                InlineText(" markup")
              )
            )
          )
        )
      )
    }

    "authors" in assert {
      parse(ExampleFiles.multipleAuthors).get.value ===
                      Document(
        Some(
          Header(
            "The Dangerous and Thrilling Documentation Chronicles",
            Seq(
              Author("Kismet Rainbow Chameleon ", Some("kismet@asciidoctor.org")),
              Author(" Lazarus het_Draeke ", Some("lazarus@asciidoctor.org"))
            ),
            Seq()
          )
        ),
        Seq()
      )
    }

    "attributed block" in {
      assert(parse(ExampleFiles.attributedParagraph).get.value === Document(
        None,
        Seq(
          BlockWithAttributes(
            Paragraph(List(InlineText("A "), InlineText("paragraph"))),
            Seq(
              Seq(
                Attribute("", "someAttribute"),
                Attribute("someOtherAttribute", "test, nochnTest")
              )
            ),
            Some(" With a title")
          )
        )
      ))
    }

    "many sections" in {
      val ParsingTest(c, r) = ExampleFiles.manySections
      assert(parse(c).get.value === r)
    }

    "delimited blocks" - {
      "nested document" in {
        assert(parse(ExampleFiles.nestedExample).get.value === ExampleFiles.nestedExampleParsed)
      }
    }

    "lists" in {
      val ParsingTest(c, r) = ExampleFiles.simpleLists
      assert(parse(c).get.value === r)
    }

  }

}