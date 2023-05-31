package scitzen.cli

import scitzen.compat.Logging
import scitzen.generic.{DocumentDirectory, Project}
import scitzen.outputs.{Includes, SastToTextConverter}

object ConvertTemplate:
  def fillTemplate(
      project: Project,
      directory: DocumentDirectory,
      templatePath: String,
      templateSettings: Map[String, String]
  ): String =
    project.resolve(project.root, templatePath) match
      case None =>
        Logging.scribe.warn(s"could not find template file »$templatePath«")
        ""
      case Some(templateFile) =>
        val templateSast = directory.byPath(templateFile).sast
        val documentString = SastToTextConverter(
          templateSettings,
          Some(Includes(project, templateFile, directory))
        ).convert(templateSast).mkString("\n")
        documentString
