package scitzen.cli

import scitzen.bibliography.BibDB
import scitzen.compat.Logging
import scitzen.contexts.ConversionContext
import scitzen.extern.{BlockConversions}
import scitzen.generic.{ArticleDirectory, Project, ProjectPath}
import scitzen.outputs.SastToTextConverter
import scitzen.sast.Attributes

object ConvertTemplate:
  def fillTemplate(
      project: Project,
      directory: ArticleDirectory,
      templatePath: Option[ProjectPath],
      templateSettings: Attributes
  ): String =
    templatePath match
      case None =>
        Logging.cli.warn(s"could not find template file")
        templateSettings.plain("template content").getOrElse("")
      case Some(templateFile) =>
        val templateArticle = directory.byPath(templateFile).head
        val documentString = SastToTextConverter(
          ::(templateArticle.ref, Nil),
          ConversionAnalysis(
            project,
            Nil,
            directory,
            BlockConversions(Map.empty),
            BibDB.empty,
            None,
          ),
          templateSettings,
          ProjectPath(project, project.cacheDir.resolve("templates"))
        ).convertSastSeq(ConversionContext(()), templateArticle.sast)
        documentString.data.mkString("")
