package scitzen.outputs

import better.files.File
import scitzen.generic.{DocumentDirectory, Project}
import scitzen.sast.MacroCommand.{Include, Lookup}
import scitzen.sast.{Block, Fenced, Inline, InlineText, ListItem, Macro, Paragraph, Parsed, Sast, Section, Slist, SpaceComment, Text}

case class Includes(    project: Project,
                        cwf: File,
                        includeResolver: DocumentDirectory)

case class SastToTextConverter(
    definitions: Map[String, String] = Map.empty,
    includes: Option[Includes] = None
) {

  def convert(b: Seq[Sast]): Seq[String] = {
    b.flatMap {
      case Section(title, _, _) =>
        List(convertInline(title.inl))

      case Slist(children) => children.flatMap {
          case ListItem(_, Text(inl), None) =>
            List(convertInline(inl))
          case ListItem(_, text, Some(inner)) =>
            convertInline(text.inl) +: convert(List(inner))
        }

      case Macro(Lookup, attr) =>
        if (!definitions.contains(attr.target)) scribe.error(s"could not resolve ${attr.target}")
        List(definitions(attr.target))

      case Macro(Include, attributes) =>
        includes match {
          case None => Nil
          case Some(Includes(project, cwf, includeResolver)) =>
            project.resolve(cwf.parent, attributes.target).flatMap(includeResolver.byPath.get) match {
              case Some(doc) =>
                val included = includeResolver.byPath(doc.file)

                new SastToTextConverter(definitions, includes)
                .convert(included.sast)

              case None =>
                scribe.error(s"unknown include ${attributes.target} in template ${cwf}")
                Nil
            }
        }


      case Macro(_, _) => Nil

      case Block(attr, blockType) =>
        val filterBlock = attr.positional match {
          case "if" :: parameter :: _ =>
            val res = definitions.get(parameter) match {
              case Some(value) =>
                attr.named.get("equals").forall(_ == value)
              case None => false
            }
            if (attr.named.contains("not")) !res else res
          case _ => true
        }

        if (!filterBlock) Nil
        else {
          blockType match {
            case Paragraph(content)      => List(convertInline(content.inl))
            case Parsed(_, blockContent) => convert(blockContent)
            case Fenced(text)            => List(text)
            case SpaceComment(_)         => Nil
          }
        }

    }
  }

  def convertInline(inners: Seq[Inline]): String =
    inners.map {
      case InlineText(str) => str
      case Macro(Lookup, attr) =>
        definitions.get(attr.target).orElse(attr.named.get("default")).get
      case _: Macro => ""
    }.mkString("")
}
