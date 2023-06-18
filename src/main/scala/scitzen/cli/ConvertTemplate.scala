package scitzen.cli

import scitzen.compat.Logging
import scitzen.generic.{ArticleDirectory, Project}
import scitzen.outputs.SastToTextConverter

object ConvertTemplate:
  def fillTemplate(
      project: Project,
      directory: ArticleDirectory,
      templatePath: String,
      templateSettings: Map[String, String]
  ): String =
    project.resolve(project.root, templatePath) match
      case None =>
        Logging.scribe.warn(s"could not find template file »$templatePath«")
        ""
      case Some(templateFile) =>
        val templateArticle = directory.byPath(templateFile).head
        val documentString = SastToTextConverter(
          templateSettings,
          directory,
          Some(templateArticle.doc),
        ).convert(templateArticle.sast).mkString("\n")
        documentString
