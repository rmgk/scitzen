package scitzen.outputs

import scitzen.generic.{DocumentDirectory, Project}
import scitzen.sast.DCommand.{Include, Lookup}
import scitzen.sast.{
  Block, Directive, Fenced, Inline, InlineText, ListItem, Paragraph, Parsed, Sast, Section, Slist, SpaceComment, Text
}
import scitzen.compat.Logging.scribe

import java.nio.file.Path

case class Includes(project: Project, cwf: Path, includeResolver: DocumentDirectory)

case class SastToTextConverter(
    definitions: Map[String, String] = Map.empty,
    includes: Option[Includes] = None
):

  def convert(b: Seq[Sast]): Seq[String] =
    b.flatMap {
      case Section(title, _, _) =>
        List(convertInline(title.inl))

      case Slist(children) => children.flatMap {
          case ListItem(_, Text(inl), None) =>
            List(convertInline(inl))
          case ListItem(_, text, Some(inner)) =>
            convertInline(text.inl) +: convert(List(inner))
        }

      case Block(attr, blockType, _) =>
        val filterBlock =
          attr.legacyPositional match
            case "if" :: parameter :: _ =>
              val res =
                definitions.get(parameter) match
                  case Some(value) =>
                    attr.named.get("equals").forall(_ == value)
                  case None => false
              if attr.named.contains("not") then !res else res
            case _ => true

        if !filterBlock then Nil
        else
          blockType match
            case Paragraph(content)      => List(convertInline(content.inl))
            case Parsed(_, blockContent) => convert(blockContent)
            case Fenced(text)            => List(text)
            case SpaceComment(_)         => Nil

      case mcro: Directive =>
        val attributes = mcro.attributes
        mcro.command match

          case Lookup =>
            definitions.get(attributes.target).orElse(attributes.named.get("default")) match
              case None =>
                scribe.error(s"could not resolve ${attributes.target}")
                Nil
              case Some(res) => List(res)

          case Include =>
            includes match
              case None => Nil
              case Some(Includes(project, cwf, includeResolver)) =>
                project.resolve(cwf.getParent, attributes.target).flatMap(includeResolver.byPath.get) match
                  case Some(doc) =>
                    val included = includeResolver.byPath(doc.file)

                    new SastToTextConverter(definitions, includes)
                      .convert(included.sast)

                  case None =>
                    val pos = includes match
                      case None => ""
                      case Some(inc) => s" ${inc.includeResolver.byPath(inc.cwf).reporter(mcro)}"
                    scribe.error(s"unknown include ${attributes.target} in template ${cwf}$pos")
                    Nil

          case _ => Nil

    }

  def convertInline(inners: Seq[Inline]): String =
    inners.map {
      case InlineText(str) => str
      case Directive(Lookup, attr) =>
        definitions.get(attr.target).orElse(attr.named.get("default")).getOrElse("")
      case _: Directive => ""
    }.mkString("")
