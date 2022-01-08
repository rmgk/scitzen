package scitzen.compat

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import scitzen.sast.{Attributes, Macro, MacroCommand, Prov, Sast, Section, Text}

object Codecs {

  val macroTupleCodec: JsonValueCodec[(MacroCommand, Attributes, Prov)] = JsonCodecMaker.make[(MacroCommand, Attributes, Prov)](CodecMakerConfig.withAllowRecursiveTypes(true))

  implicit val MacroEncoder: JsonValueCodec[Macro] = new JsonValueCodec[Macro] {
    override def decodeValue(in: JsonReader, default: Macro): Macro = {
      val res = macroTupleCodec.decodeValue(in, default.toTuple)
      Macro(res._1, res._2)(res._3)
    }
    override def encodeValue(x: Macro, out: JsonWriter): Unit = macroTupleCodec.encodeValue((x.toTuple), out)
    override def nullValue: Macro = null
  }


  val sectionTupleCodec: JsonValueCodec[(Text, String, Attributes, Prov)] = JsonCodecMaker.make[(Text, String, Attributes, Prov)](CodecMakerConfig.withAllowRecursiveTypes(true))
  implicit val SectionEncoder: JsonValueCodec[Section] = new JsonValueCodec[Section] {
    override def decodeValue(in: JsonReader, default: Section): Section = {
      val res = sectionTupleCodec.decodeValue(in, default.toTuple)
      Section(res._1, res._2,res._3)(res._4)
    }
    override def encodeValue(x: Section, out: JsonWriter): Unit = sectionTupleCodec.encodeValue(x.toTuple, out)
    override def nullValue: Section = null
  }


  implicit val SastEncoder: JsonValueCodec[List[Sast]] =
    JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))

  val mapCodec: JsonValueCodec[Map[String, String]] = JsonCodecMaker.make

  case class Reference(file: String, start: Int, end: Int)

  implicit val rferenceRW: JsonValueCodec[Map[String, List[Reference]]] = JsonCodecMaker.make

}
