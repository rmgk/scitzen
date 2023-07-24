package scitzen.blockconverters

import scitzen.sast.Sast

trait BlockConverterModule {
  def handles: String
  def convert(converterParams: ConverterParams): List[Sast]
}
