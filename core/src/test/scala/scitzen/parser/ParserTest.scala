package scitzen.parser

import scitzen.parser.BlockType.Delimited
import fastparse.P
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FreeSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class ParserTest extends FreeSpec with GeneratorDrivenPropertyChecks {

//  "identifier" - {
//    import asciimedic.Asciimedic.Identifier.identifier.parse
//    "simple" in {assert(parse("Good123_-test").get.value === "Good123_-test")}
//    "fail" in {assert(parse("-Bad").isInstanceOf[Parsed.Failure[_, _]])}
//  }
//
//  "quoted" - {
//    import asciimedic.Asciimedic.quoted
//    "string" in assert {quoted("'").parse("""'success! :D'""").get.value === "success! :D"}
//    "parens" in assert {quoted(")", open = Some("(")).parse("""(success! :D)""").get.value === "success! :D"}
//    "escapes" in assert {quoted("'").parse("""'success! \a \\ \' '""").get.value === """success! \a \ ' """}
//    "escape stack" in assert {quoted("'").parse("""'success! \a \\\\\\\''""").get.value === """success! \a \\\'"""}
//    "escape parens" in assert {
//      quoted(")", open = Some("(")).parse("""(success! \\ \) )""").get
//      .value === """success! \ ) """
//    }
//    "horror" in assert {quoted(")", open = Some("(")).parse("""(success! \\\)\\)""").get.value === """success! \)\"""}
//    "degenerate" in assert {quoted("\\").parse("""\success!\""").get.value === """success!"""}
//    "degenerate horror" in assert {quoted("\\").parse("""\\\success!\\\\\""").get.value === """\success!\\"""}
//  }
//
//  "unquoted" - {
//    def p[_: P] = CommonParsers.untilE(CommonParsers.saws)
//    "anychar" in forAll { c: Char =>
//      assert(AnyChar.!.parse(c.toString).get.value === c.toString)
//    }
//
//    "any string roundtrip" in forAll { str: String =>
//      whenever(str.exists(c => !c.isWhitespace)) {
//        assert(p.parse(str).get.value === str)
//      }
//    }
//  }
//
//  "block with attributes" - {
//    "simple" in {
//      pprint.pprintln(asciimedic.Asciimedic.document.parse(ExampleFiles.blockWithSingleAttribute))
//    }
//  }
//
//  "attribute lists" - {
//    import asciimedic.Asciimedic.Attributes.list.parse
//
//    def sanitize(attrs: List[String]): List[String] = {
//      attrs.map(_.replace(",", "")
//                 .replace("]", "")
//                 .filter(c => !c.isWhitespace))
//      .filter(_.exists(c => !c.isWhitespace))
//    }
//
//
//    def assertPositional(seq: Seq[String]) = {
//      val str = seq.mkString("[", ",", "]")
//      assert(parse(str).get.value === seq.map(v => Attribute("", v)), s"str was: $str")
//    }
//    "positional" - {
//      "empty" in assert(parse("[]").get.value === Seq())
//      "empty2" in assert(parse("[ ] ").get.value === Seq())
//      "positional" in assert(parse("[cheese]").get.value === Seq(Attribute("", "cheese")))
//      "not quite empty" in assert(parse("[ \0] ").get.value === Seq(Attribute("", "\0")))
//      "weird whitespace" in assertPositional(Seq("\u001E"))
//      "weird quotes" in assertPositional(Seq("a\"value"))
//      "many positional" in forAll(Gen.listOf(Arbitrary.arbString.arbitrary)) { attrs: List[String] =>
//        assertPositional(sanitize(attrs))
//      }
//    }
//    "named" - {
//      "one with unquoted" in forAll { str: String =>
//        val sanitized = sanitize(List(str))
//        whenever(sanitized.nonEmpty) {
//          val san = sanitized.head
//          val atttrs = s"[val = $san]"
//          assert(parse(atttrs).get.value === Seq(Attribute("val", s"$san")))
//        }
//      }
//    }
//  }
//
//

  "parse title" in {
    import fastparse.NoWhitespace._
    import fastparse._
    def untilE[_:P](closing: => P[Unit]) = P(((!closing) ~ AnyChar).rep(1).!)
    assert(fastparse.parse("Test", p => untilE(End(p))(p)).get.value == "Test")

    assert(fastparse.parse("= Title", HeaderParsers.title(_)).get.value == "Title")
  }

  "parse document" - {
    def parse(str: String) = fastparse.parse(str, scitzen.parser.DocumentParsers.document(_))
//    "link" in {
//      pprint.pprintln(parse(ExampleFiles.link).get.value)
//      assert(
//        parse(ExampleFiles.link).get.value ===
//        Document(
//          None,
//          Seq(
//            Paragraph(
//              Seq(
//                InlineText("We're "),
//                InlineText("parsing "),
//                InlineMacro("link", "http://asciidoc.org", Seq(Attribute("", "AsciiDoc"))),
//                InlineText(" markup")
//              )
//            )
//          )
//        )
//      )
//    }

    "authors" in assert {
      parse(ExampleFiles.multipleAuthors).get.value ===
                      Document(
        Some(
          Header(
            "The Dangerous and Thrilling Documentation Chronicles",
            "Kismet Rainbow Chameleon <kismet@asciidoctor.org>; Lazarus het_Draeke <lazarus@asciidoctor.org>",
            "",
            Seq()
          )
        ),
        Seq()
      )
    }
//
//    "attributed block" in {
//      assert(parse(ExampleFiles.attributedParagraph).get.value === Document(
//        None,
//        Seq(
//          BlockWithAttributes(
//            Paragraph(List(InlineText("A "), InlineText("paragraph"))),
//            Seq(
//              Seq(
//                Attribute("", "someAttribute"),
//                Attribute("someOtherAttribute", "test, nochnTest")
//              )
//            ),
//            Some(" With a title")
//          )
//        )
//      ))
//    }
//
//    "many sections" in {
//      val ParsingTest(c, r) = ExampleFiles.manySections
//      assert(parse(c).get.value === r)
//    }
//
//    "delimited blocks" - {
//      "nested document" in {
//        assert(parse(ExampleFiles.nestedExample).get.value === ExampleFiles.nestedExampleParsed)
//      }
//    }
//
//    "lists" in {
//      val ParsingTest(c, r) = ExampleFiles.simpleLists
//      assert(parse(c).get.value === r)
//    }
//
  }

  "delimited blocks" in {
    val str = """[source]
----
block content
----
"""
    val res = fastparse.parse(str, BlockParsers.fullBlock(_)).get.value
    assert (res == BlockWithAttributes(
      NormalBlock(Delimited("----"), "block content"),
      Seq(Seq(Attribute("", "source"))),
      None
    ))
  }

  "paragraph quotes" in {
    val str = "hallo ``schöne`` welt"
    val res = fastparse.parse(str, InlineParser.fullParagraph(_)).get.value
    pprint.pprintln(res)
  }

}
