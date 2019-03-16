package scitzen.cli

import mdoc.internal.io.ConsoleReporter
import mdoc.internal.markdown.Modifier.Default
import mdoc.internal.markdown.{Instrumenter, MarkdownCompiler, Renderer, SectionInput}

import scala.meta.{Source, dialects}

object CompileTest {

  val mdownCompiler = MarkdownCompiler.default()

  val reporter = ConsoleReporter.default

  def compile(blocks: Iterable[String]): List[String] = {

    println(s"compiling $blocks")

    val parsed = blocks.map(dialects.Sbt1(_).parse[Source].get)
    // null should be the original input map, may be used for error reporting?
    // modifiers customize the output … maybe
    val sections = parsed.map(SectionInput(null, _, Default())).toList
    val instrumented = Instrumenter.instrument(sections)


    val compiled = MarkdownCompiler.buildDocument(
      mdownCompiler,
      reporter,
      sections,
      instrumented,
      "/home/ragnar/Sync/Projekte/scitzen/docs/test.md")

    val rendered = compiled.sections.map { section =>
      Renderer.renderEvaluatedSection(
        compiled,
        section,
        reporter,
        v => s"\n//${v.name}: ${v.staticType} = ${v.runtimeValue}",
        mdownCompiler
      )
    }

    rendered

  }

  //
  //def main(args: Array[String]): Unit = {
  //
  //  //mdoc.Main.main(Array())
  //
  //  val source: Source = dialects.Sbt1("""
  //  1 + 100
  //  val x = 100
  //  """).parse[Source].get
  //
  //  val source2: Source = dialects.Sbt1("""
  //  1 + 10
  //  val y = 100
  //  """).parse[Source].get
  //
  //
  //  val sectionList = List(SectionInput(null, source, Default()),
  //                         SectionInput(null, source2, Default()))
  //
  //  val instrumented = Instrumenter.instrument(sectionList)
  //  val mdownCompiler = MarkdownCompiler.default()
  //
  //  val reporter = ConsoleReporter.default
  //  val rendered = MarkdownCompiler.buildDocument(
  //    mdownCompiler,
  //    reporter,
  //    sectionList,
  //    instrumented,
  //    "/home/ragnar/Sync/Projekte/scitzen/docs/test.md")
  //
  //  val res = rendered.sections.map { section =>
  //    println(section)
  //    println("============f")
  //    Renderer.renderEvaluatedSection(
  //    rendered,
  //    section,
  //    reporter,
  //    { v => s"\n//${v.name}: ${v.staticType} = ${v.runtimeValue}" },
  //    mdownCompiler
  //    )
  //  }
  //
  //
  //  println(res)
  //}
}
