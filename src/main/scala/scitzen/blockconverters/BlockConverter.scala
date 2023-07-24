package scitzen.blockconverters

import de.rmgk.logging.Loggable
import scitzen.cli.ConversionAnalysis
import scitzen.compat.Logging
import scitzen.compat.Logging.cli
import scitzen.contexts.ConversionContext
import scitzen.outputs.SastToTextConverter
import scitzen.project.{Article, ArticleDirectory, Project}
import scitzen.sast.Attribute.Nested
import scitzen.sast.{Attribute, Attributes, BCommand, Block, Fenced, Parsed, Sast}

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
    List(TexModule, GraphvizModule, MermaidModule, FileContentModule, ScalaCliModule, TemplateModule, JsModule, PikchrModule)

  def run(): BlockConversions =
    BlockConversions:
      articleDirectory.articles.flatMap: art =>
        art.context.convertBlocks.map: block =>
          block -> applyConversions(art, block)
      .toMap

  def applyConversions(article: Article, block: Block): List[Sast] =
    cli.trace("converting block")
    val conversions = block.attributes.raw.collect:
      case n: Nested => n
    if conversions.isEmpty then
      cli.warn(s"conversion block has no converters", article.doc.reporter.apply(block))
      return Nil

    val (resctx, transformed) = block match
      case Block(_, _, Parsed(_, content)) =>
        val resctx = new SastToTextConverter(
          ::(article.ref, Nil),
          ConversionAnalysis.minimal(project, articleDirectory),
          Attributes(project.config.attrs.raw ++ article.titled.map(_.attributes.raw).getOrElse(Nil))
        ).convertSastSeq(ConversionContext(()), content)
        (Some(resctx), block.copy(content = Fenced(resctx.data.mkString("\n")))(block.prov))
      case other => (None, other)

    conversions.foldLeft(List[Sast](transformed)) { case (current, Nested(name, attrs)) =>
      current match
        case Nil => Nil
        case List(block @ Block(_, _, Fenced(content))) =>
          try
            cli.trace("applying conversion", name)
            modules.find(_.handles == name) match
              case None =>
                cli.warn(s"could not convert, no handler for $name")
                Nil
              case Some(module) =>
                module.convert(ConverterParams(project, articleDirectory, article, block, content, attrs, resctx))
          catch
            case NonFatal(ex) =>
              cli.warn(s"could not convert $name", ex)
              Nil
        case other =>
          cli.warn(s"can not convert $other")
          Nil

    }

}
