package sciparse

import scitzen.parser.{AttributesParser, BlockParsers, CommonParsers, DirectiveParsers, Parse}
import de.rmgk.scip.*
import munit.Location
import scitzen.bibliography.BibDB
import scitzen.outputs.SastToScimConverter
import scitzen.html.sag
import scitzen.html.sag.{Recipe, XmlTags}

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

class SagTest extends munit.FunSuite {

  val SastToScimConverter = scitzen.outputs.SastToScimConverter(BibDB.empty)

  test("basic directive") {

    val xt = new sag.XmlTags()

    val ctx = new sag.SagContext(new ByteArrayOutputStream())
    xt.body(style = "some", xt.p("this is a pragraph with weird symbols \" attribute > value")).runInContext(ctx)
    println(ctx.baos.toString(StandardCharsets.UTF_8))

  }
}
