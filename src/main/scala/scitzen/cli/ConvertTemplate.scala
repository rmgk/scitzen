package scitzen.cli

import scitzen.generic.{DocumentDirectory, Project}
import scitzen.outputs.{Includes, SastToTextConverter}

object ConvertTemplate:
  def fillTemplate(
      project: Project,
      directory: DocumentDirectory,
      template: String,
      templateSettings: Map[String, String]
  ): String =
    val templateFile = project.resolve(project.root, template).get
    val templateSast = directory.byPath(templateFile).sast
    val documentString = SastToTextConverter(
      templateSettings,
      Some(Includes(project, templateFile, directory))
    ).convert(templateSast).mkString("\n")
    documentString
