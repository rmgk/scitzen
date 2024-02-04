package scitzen.blockconverters

import de.rmgk.logging.Loggable
import scitzen.cli.{ConversionAnalysis, Logging}
import Logging.cli
import scitzen.contexts.ConversionContext
import scitzen.outputs.SastToTextConverter
import scitzen.project.{Article, ArticleDirectory, Project}
import scitzen.sast.{Attribute, Attributes, BCommand, Block, Directive, Fenced, FusedDelimited, Sast, attributes}

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets
import scala.util.control.NonFatal

given Loggable[Throwable] with
  override def normal(v: Throwable): String = s"${v.getClass.getSimpleName}: »${v.getMessage}«"
  override def verbose(v: Throwable): String =
    val baos = new ByteArrayOutputStream()
    v.printStackTrace(new PrintStream(baos))
    baos.toString(StandardCharsets.UTF_8)

case class BlockConversions(mapping: Map[Block, List[Sast]]):
  def substitute(block: Block): List[Sast] = mapping.getOrElse(block, Nil)

class BlockConverter(project: Project, articleDirectory: ArticleDirectory) {

  val modules =
    List(
      TexModule,
      GraphvizModule,
      MermaidModule,
      FileContentModule,
      ScalaCliModule,
      TemplateModule,
      JsModule,
      PikchrModule,
      D2Module
    )

  def run(): BlockConversions =
    BlockConversions:
      articleDirectory.articles.flatMap: art =>
        art.context.fencedConvert.map: fenced =>
          fenced -> applyConversions(art, fenced)
        art.context.delimitedConvert.map: delimited =>
          delimited -> applyConversions(art, delimited)
      .toMap

  def applyConversions(article: Article, block: Block): List[Sast] =
    cli.trace("converting block")
    val (conversions, additionalAttributes) = block.attributes.all.span(attr => attr.asTarget != "done")
    if conversions.isEmpty then
      cli.warn(s"conversion block has no converters", article.doc.reporter.apply(block))
      return Nil

    val (resctx, transformed) = block match
      case FusedDelimited(del, _, content) =>
        val resctx = new SastToTextConverter(
          ::(article.ref, Nil),
          ConversionAnalysis.minimal(project, articleDirectory),
          Attributes(project.config.attrs.all ++ article.titled.map(_.attributes.all).getOrElse(Nil))
        ).convertSastSeq(ConversionContext(()), content)
        (
          Some(resctx),
          Fenced(del.command, del.attributes, resctx.data.mkString("\n"), del.meta)
        )
      case other => (None, other)

    val converted = conversions.foldLeft(List[Sast](transformed)) { case (current, attr) =>
      val name = Option.when(!attr.id.isBlank)(attr.id).getOrElse(attr.asTarget)
      current match
        case Nil => Nil
        case List(block @ Fenced(_, _, content, _)) =>
          try
            cli.trace("applying conversion", name)
            modules.find(_.handles == name) match
              case None =>
                cli.warn(s"could not convert, no handler for $name")
                Nil
              case Some(module) =>
                module.convert(ConverterParams(project, articleDirectory, article, block, content, attr, resctx))
          catch
            case NonFatal(ex) =>
              cli.warn(s"could not convert $name", ex)
              Nil
        case other =>
          cli.warn(s"can not convert $other")
          Nil
    }
    converted match
      case Directive(c, a, m) :: rest => Directive(c, Attributes(a.all ++ additionalAttributes.drop(1)), m) :: rest
      case other                      => other

}
