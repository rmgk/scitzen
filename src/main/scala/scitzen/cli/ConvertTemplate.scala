package scitzen.cli

import scitzen.bibliography.BibDB
import scitzen.compat.Logging
import scitzen.contexts.ConversionContext
import scitzen.extern.{BlockConversions, ImageConversions}
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
        Logging.cli.warn(s"could not find template file »$templatePath«")
        ""
      case Some(templateFile) =>
        val templateArticle = directory.byPath(templateFile).head
        val documentString = SastToTextConverter(
          templateArticle.ref,
          ConversionAnalysis(
            project,
            Nil,
            directory,
            BlockConversions(Map.empty),
            ImageConversions(Map.empty),
            BibDB.empty,
            None,
          ),
          templateSettings
        ).convertSastSeq(ConversionContext(()), templateArticle.sast)
        documentString.data.mkString("")
