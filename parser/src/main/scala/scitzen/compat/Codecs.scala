package scitzen.compat

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import scitzen.sast.Sast

object Codecs {

  implicit val SastEncoder: JsonValueCodec[List[Sast]] =
    JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))

  val mapCodec: JsonValueCodec[Map[String, String]] = JsonCodecMaker.make

  case class Reference(file: String, start: Int, end: Int)

  implicit val rferenceRW: JsonValueCodec[Map[String, List[Reference]]] = JsonCodecMaker.make

}
