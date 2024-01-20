package scitzen.blockconverters

import scitzen.cli.ConvertTemplate
import scitzen.cli.ImageReferences.Reference
import scitzen.project.References
import scitzen.sast.{Attribute, Attributes, Fenced, Sast}

object TemplateModule extends BlockConverterModule {

  override def handles: String = "template"
  def convert(converterParams: ConverterParams): List[Sast] =
    import converterParams.*
    val pathString = attribute.asTarget
    val resolved = ConvertTemplate.fillTemplate(
      project,
      articleDirectory,
      References.resolveResource(project, article.doc, pathString).headOption,
      Attributes(project.config.attrs.raw :+ Attribute("template content", content))
    )
    List(Fenced(block.command, block.attributes, resolved, block.meta))

}
