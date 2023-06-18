package scitzen.extern

import scitzen.cli.ConvertTemplate
import scitzen.compat.Logging
import scitzen.compat.Logging.scribe
import scitzen.generic.{Article, ArticleDirectory, Project}
import scitzen.sast.{Attribute, Attributes, BCommand, Block, DCommand, Directive, Fenced, Sast}

import java.lang.ProcessBuilder.Redirect
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.Using

case class BlockConversions(mapping: Map[Block, List[Sast]]):
  def substitute(block: Block): List[Sast] = mapping.getOrElse(block, Nil)

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
      current match
        case Nil => Nil
        case List(block @ Block(_, _, Fenced(content))) =>
          name match
            case "template" => applyTemplate(attrs, block, content, article)
            case "js"       => convertJS(current, attrs)
            case "tex"      => convertTex(article, block, content, attrs)
            case "graphviz" => graphviz(content, block)
            case "mermaid"  => mermaid(content, block)
            case "scalaCli" => convertScalaCli(content, block)
            case "load" => loadFileAsContent(block, article, attrs)
        case other =>
          scribe.error(s"can not convert $other")
          Nil

    }

  def loadFileAsContent(block: Block, article: Article, attributes: Attributes) =
    article.doc.resolve(attributes.target) match
      case Some(path) => List(Block(BCommand.Empty, Attributes.emtpy, Fenced(Files.readString(path.absolute)))(block.prov))
      case None => Nil

  def convertScalaCli(text: String, block: Block) =
    val start      = System.nanoTime()
    val hash       = Hashes.sha1hex(text)
    val sourcepath = project.cachePath(Path.of("scala-cli").resolve(hash + ".scala"))
    Files.createDirectories(sourcepath.directory)
    Files.writeString(sourcepath.absolute, text)
    val outpath   = sourcepath.directory resolve s"$hash.js"
    val errorFile = sourcepath.directory resolve s"log-$hash.txt"
    val returnCode =
      new ProcessBuilder(
        "scala-cli",
        "--power",
        "package",
        "--force",
        "--output",
        outpath.toString,
        sourcepath.absolute.toString,
      ).directory(sourcepath.directory.toFile)
        .redirectOutput(errorFile.toFile)
        .redirectError(errorFile.toFile)
        .start().waitFor()
    if returnCode == 0 then
      scribe.info(s"scala compilation of »$sourcepath« finished in ${(System.nanoTime() - start) / 1000000}ms")
      val pp = project.asProjectPath(outpath)
      List(
        Directive(DCommand.Script, Attributes.target(pp.projectAbsolute.toString))(block.prov)
      )
    else
      scribe.error(s"error scala compiling »$sourcepath« see »$errorFile«")
      Nil

  def convertJS(sast: List[Sast], attr: Attributes): List[Sast] =
    sast match
      case List(block @ Block(_, _, Fenced(content))) =>
        val res = scitzen.extern.JsRunner().run(content, attr)
        List(Block(BCommand.Code, Attributes.emtpy, Fenced(res))(block.prov))
      case other =>
        Logging.scribe.error(s"js conversion not applicable")
        sast

  def convertTex(article: Article, block: Block, content: String, attr: Attributes): List[Sast] =
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
          Directive(
            DCommand.Image,
            Attributes(List(
              Attribute("", target.projectAbsolute.toString),
              Attribute("css_style", "background-color:white")
            ))
          )(block.prov)
        )
      case None =>
        Nil

  def graphviz(content: String, block: Block): List[Sast] =
    val bytes  = content.getBytes(StandardCharsets.UTF_8)
    val name   = Hashes.sha1hex(bytes)
    val format = "pdf"
    val target = project.cachePath(Path.of(s"$name/$name.$format"))
    if !Files.exists(target.absolute) then
      Files.createDirectories(target.directory)

      val start = System.nanoTime()
      val process = new ProcessBuilder(
        "dot",
        s"-T$format",
        s"-o${target.absolute.toString}"
      )
        .inheritIO().redirectInput(Redirect.PIPE).start()
      Using.resource(process.getOutputStream) { os => os.write(bytes) }
      process.waitFor()
      scribe.info(s"graphviz compilation finished in ${(System.nanoTime() - start) / 1000000}ms")
    List(Directive(DCommand.Image, Attributes.target(target.projectAbsolute.toString))(block.prov))

  def mermaid(content: String, block: Block): List[Sast] =
    val bytes         = content.getBytes(StandardCharsets.UTF_8)
    val name          = Hashes.sha1hex(bytes)
    val format        = "svg"
    val target        = project.cachePath(Path.of(s"$name/$name.$format"))
    val mermaidSource = project.cachePath(Path.of(s"$name/$name.mermaid"))
    if !Files.exists(target.absolute) then
      val start = System.nanoTime()

      Files.createDirectories(mermaidSource.directory)
      Files.write(mermaidSource.absolute, bytes)

      new ProcessBuilder(
        "mmdc",
        "--input",
        mermaidSource.absolute.toString,
        "--output",
        target.absolute.toString
      )
        .inheritIO().start().waitFor()
      scribe.info(s"mermaid compilation finished in ${(System.nanoTime() - start) / 1000000}ms")
    List(Directive(DCommand.Image, Attributes.target(target.projectAbsolute.toString))(block.prov))

  def applyTemplate(
      attributes: Attributes,
      block: Block,
      origContent: String,
      article: Article,
  ): List[Sast] =
    val resolved =
      val pathString = attributes.target
      ConvertTemplate.fillTemplate(
        project,
        articleDirectory,
        pathString,
        project.config.definitions ++ attributes.named + ("template content" -> origContent)
      )
    List(block.copy(content = Fenced(resolved))(block.prov))

}
