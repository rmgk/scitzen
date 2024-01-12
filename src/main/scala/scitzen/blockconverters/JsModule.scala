package scitzen.blockconverters

import scitzen.sast.{Attributes, BCommand, Block, Fenced, Sast}

object JsModule extends BlockConverterModule {

  override def handles: String = "js"
  def convert(converterParams: ConverterParams): List[Sast] =
    import converterParams.{attributes, block, content}
    val res = scitzen.extern.JsRunner().run(content, attributes)
    List(Fenced(BCommand.Code, Attributes.empty, res, block.meta))
}
