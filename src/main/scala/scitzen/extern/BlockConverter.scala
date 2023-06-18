package scitzen.extern

import scitzen.compat.Logging
import scitzen.compat.Logging.scribe
import scitzen.generic.{Article, ArticleDirectory, Project}
import scitzen.outputs.SastToTextConverter
import scitzen.sast.{Attribute, Attributes, BCommand, Block, DCommand, Directive, Fenced, Sast}

import java.lang.ProcessBuilder.Redirect
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.Using

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
        case "template" => applyTemplate(attrs, current, article)
        case "js"       => convertJS(current, attrs)
        case "tex"      => convertTex(article, current, attrs)

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
      case List(block @ Block(_, _, Fenced(content))) =>
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

  def graphviz(content: String, dir: Path, name: String, format: String): Path =
    val bytes  = content.getBytes(StandardCharsets.UTF_8)
    val target = dir.resolve(name + s".$format")
    if !Files.exists(target) then
      Files.createDirectories(dir)

      val start = System.nanoTime()
      val process = new ProcessBuilder(
        "dot",
        s"-T$format",
        s"-o${target.toAbsolutePath.toString}"
      )
        .inheritIO().redirectInput(Redirect.PIPE).start()
      Using.resource(process.getOutputStream) { os => os.write(bytes) }
      process.waitFor()
      scribe.info(s"graphviz compilation finished in ${(System.nanoTime() - start) / 1000000}ms")
    target

  def mermaid(content: String, dir: Path, name: String, format: String): Path =
    val bytes         = content.getBytes(StandardCharsets.UTF_8)
    val target        = dir.resolve(name + s".$format")
    val mermaidSource = dir.resolve(name + ".mermaid")
    if !Files.exists(target) then
      val start = System.nanoTime()

      Files.createDirectories(mermaidSource.getParent)
      Files.write(mermaidSource, bytes)

      new ProcessBuilder(
        "mmdc",
        "--input",
        mermaidSource.toAbsolutePath.toString,
        "--output",
        target.toAbsolutePath.toString
      )
        .inheritIO().start().waitFor()
      scribe.info(s"mermaid compilation finished in ${(System.nanoTime() - start) / 1000000}ms")
    target

  def applyTemplate(
      attributes: Attributes,
      currentInput: List[Sast],
      article: Article,
  ): List[Sast] =
    currentInput match
      case List(block @ Block(_, _, Fenced(origContent))) =>
        val resolved = attributes.named.get("template") match {
          case None =>
            scribe.error(s"no template")
            origContent
          case Some(pathString) => article.sourceDoc.resolve(pathString) match
              case None =>
                scribe.error(s"could not resolve $pathString")
                origContent
              case Some(templatePath) =>
                articleDirectory.byPath.get(templatePath) match
                  case None =>
                    scribe.error(s"not resolved $templatePath")
                    origContent
                  case Some(articles) =>
                    val sast = articles.flatMap(_.content)
                    SastToTextConverter(
                      project.config.definitions ++ attributes.named + (
                        "template content" -> origContent
                      ),
                      articleDirectory
                    ).convert(sast).mkString("\n")
        }
        List(block.copy(content = Fenced(resolved))(block.prov))
      case other => currentInput

}
