package scitzen.blockconverters

import scitzen.contexts.ConversionContext
import scitzen.project.{Article, ArticleDirectory, Project}
import scitzen.sast.{Attribute, Attributes, Block, Fenced}

case class ConverterParams(
    project: Project,
    articleDirectory: ArticleDirectory,
    article: Article,
    block: Fenced,
    content: String,
    attribute: Attribute,
    resctx: Option[ConversionContext[?]]
)
