package sciparse

import de.rmgk.delay
import de.rmgk.scip.*
import munit.Location
import scitzen.bibliography.BibDB
import scitzen.html.sag
import scitzen.html.sag.{Recipe, Sag}
import scitzen.outputs.{HtmlPages, AtomToScimConverter}
import scitzen.parser.{AttributesParser, CommonParsers, DirectiveParsers, Parse}

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

class SagTest extends munit.FunSuite {

  val SastToScimConverter = scitzen.outputs.AtomToScimConverter(BibDB.empty)

  test("basic directive") {

    val xt = sag.Sag

    val ctx = new sag.SagContext(new ByteArrayOutputStream())
    xt.body(style = "some", xt.p("this is a pragraph with weird symbols \" attribute > value")).runInContext(ctx)
    /*println*/(ctx.baos.toString(StandardCharsets.UTF_8))

  }

  test("code") {
    //println:
      printCode(HtmlPages("").tHead(delay.Sync { () }))
  }
}
