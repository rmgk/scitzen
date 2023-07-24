package scitzen.blockconverters

import scitzen.cli.ConvertTemplate
import scitzen.sast.{Attribute, Attributes, Fenced, Sast}

object TemplateModule extends BlockConverterModule {

  override def handles: String = "template"
  def convert(converterParams: ConverterParams): List[Sast] =
    import converterParams.*
    val pathString = attributes.target
    val resolved = ConvertTemplate.fillTemplate(
      project,
      articleDirectory,
      article.doc.resolve(pathString),
      Attributes(project.config.attrs.raw ++ attributes.raw :+ Attribute("template content", content))
    )
    List(block.copy(content = Fenced(resolved))(block.prov))

}
