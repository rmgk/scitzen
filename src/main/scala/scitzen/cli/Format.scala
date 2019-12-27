package scitzen.cli

import java.nio.charset.{Charset, StandardCharsets}

import better.files.File
import scitzen.generic.{AnalyzedDoc, Project, Sast}
import scitzen.outputs.SastToScimConverter
import scitzen.parser.MacroCommand.Image







object Format {

  implicit val saneCharsetDefault: Charset = StandardCharsets.UTF_8


  def formatContents(project: Project): Unit = {

    project.documentManager.documents.foreach { pd =>
      formatContent(pd.file, pd.content, pd.sast)
    }
  }

    def formatRename(project: Project): Unit = {
    project.documentManager.fulldocs.foreach { fd =>
      if (renamePossible(fd.analyzed)) renameFileFromHeader(fd.parsed.file, fd.analyzed)
    }
  }




  def checkReferences(file: File, sdoc: AnalyzedDoc): Unit = {
    sdoc.analyzeResult.macros.foreach { mcro =>
      mcro.command match {
        case Image =>
          val path = file.parent./(mcro.attributes.target.trim)
          if (!path.isRegularFile) scribe.warn(s"${file} references nonexisting $path")
          if (!file.parent.isParentOf(path)) scribe.warn(s"${file} is not a parent of referenced $path")
        case other   => ()
      }
    }
  }

  def formatContent(file: File, originalContent: String, sast: Seq[Sast]): Unit = {
    val result = SastToScimConverter().toScimS(sast)
    val resultStr = result.iterator.mkString("", "\n", "\n")
    if (resultStr != originalContent) {
      scribe.info(s"formatting ${file.name}")
      file.write(resultStr)
    }
  }


  def renameFileFromHeader(f: File, sdoc: AnalyzedDoc): Unit = {
    val newName: String = nameFromHeader(sdoc)

    if (newName != f.name) {
      scribe.info(s"rename ${f.name} to $newName")
      f.renameTo(newName)
    }
  }

  def renamePossible(header: AnalyzedDoc): Boolean = header.title.isDefined && header.date.isDefined

  def nameFromHeader(header: AnalyzedDoc): String = {
    val title = sluggify(header.title.get) + ".scim"
    header.date.get.date.full + " " + title
  }

  def sluggify(str: String): String =
    str
    .replaceAll("""[<>":;%/\?\[\]\\\*\|]""", "-")
    .replaceAll("\\s+", " ")
    .replaceAll("-+", "-")
    .trim
    .replaceAll("^-|-$", "")

}
