package scitzen.extern

import scitzen.compat.Logging
import scitzen.generic.{Article, ArticleDirectory, Project}
import scitzen.sast.{Attribute, Attributes, BCommand, Block, DCommand, Directive, Fenced, Sast}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

case class BlockConversions(mapping: Map[Block, List[Sast]])

class BlockConverter(project: Project, articleDirectory: ArticleDirectory) {

  def run(): BlockConversions =
    BlockConversions:
      articleDirectory.articles.flatMap: art =>
        art.context.convertBlocks.map: block =>
          block -> applyConversions(art, block)
      .toMap

  def applyConversions(article: Article, block: Block) =
    val conversions = block.attributes.nested
    conversions.foldLeft(List[Sast](block)) { case (current, (name, attrs)) =>
      name match
        case "js"  => convertJS(current, attrs)
        case "tex" => convertTex(article, current, attrs)

    }

  def convertJS(sast: List[Sast], attr: Attributes): List[Sast] =
    sast match
      case List(block @ Block(_, _, Fenced(content))) =>
        val res = scitzen.extern.JsRunner().run(content, attr)
        List(Block(BCommand.Code, Attributes.emtpy, Fenced(res))(block.prov))
      case other =>
        Logging.scribe.error(s"js conversion not applicable")
        sast

  def convertTex(article: Article, sast: List[Sast], attr: Attributes): List[Sast] =
    sast match
      case List(block @ Block(_, _, Fenced(origContent))) =>
        val content =
          if !attr.named.contains("template")
          then origContent
          else
            ImageConverter.applyTemplate(
              attr,
              origContent,
              article.sourceDoc.path.directory,
              article.sourceDoc.path.project,
              articleDirectory
            )

        val texbytes    = content.getBytes(StandardCharsets.UTF_8)
        val contentHash = Hashes.sha1hex(texbytes)
        val target      = project.cachePath(Path.of(s"$contentHash/$contentHash.pdf"))
        val res =
          if Files.exists(target.absolute)
          then Some(target.absolute)
          else
            val dir = target.directory
            Files.createDirectories(dir)
            val texfile = dir.resolve(contentHash + ".tex")
            Files.write(texfile, texbytes)
            Latexmk.latexmk(dir, contentHash, texfile)
        res match
          case Some(res) =>
            List(
              Directive(DCommand.Image, Attributes(List(Attribute("", target.projectAbsolute.toString))))(block.prov)
            )
          case None =>
            Nil
      case other =>
        Logging.scribe.error(s"tex conversion not applicable")
        sast

}
