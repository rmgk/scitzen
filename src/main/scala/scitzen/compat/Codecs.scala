package scitzen.compat

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import scitzen.sast.{Attribute, Attributes, DCommand, Directive, Prov, Sast, Section, Text}

object Codecs {

  //implicit def dcc: JsonValueCodec[DCommand] = JsonCodecMaker.make[DCommand]
  //implicit def anc: JsonValueCodec[Attribute.Nested] = JsonCodecMaker.make[Attribute.Nested]
  //implicit def atc: JsonValueCodec[Attribute] = JsonCodecMaker.make[Attribute]
  //implicit def asc: JsonValueCodec[Attributes] = JsonCodecMaker.make[Attributes]
  //
  //val macroTupleCodec: JsonValueCodec[(DCommand, Attributes, Prov)] =
  //  JsonCodecMaker.make[(DCommand, Attributes, Prov)](CodecMakerConfig.withAllowRecursiveTypes(true))
  //
  //implicit val MacroEncoder: JsonValueCodec[Directive] = new JsonValueCodec[Directive] {
  //  override def decodeValue(in: JsonReader, default: Directive): Directive = {
  //    val res = macroTupleCodec.decodeValue(in, default.toTuple)
  //    Directive(res._1, res._2)(res._3)
  //  }
  //  override def encodeValue(x: Directive, out: JsonWriter): Unit = macroTupleCodec.encodeValue((x.toTuple), out)
  //  override def nullValue: Directive                             = null
  //}
  //
  //val sectionTupleCodec: JsonValueCodec[(Text, String, Attributes, Prov)] =
  //  JsonCodecMaker.make[(Text, String, Attributes, Prov)](CodecMakerConfig.withAllowRecursiveTypes(true))
  //implicit val SectionEncoder: JsonValueCodec[Section] = new JsonValueCodec[Section] {
  //  override def decodeValue(in: JsonReader, default: Section): Section = {
  //    val res = sectionTupleCodec.decodeValue(in, default.toTuple)
  //    Section(res._1, res._2, res._3)(res._4)
  //  }
  //  override def encodeValue(x: Section, out: JsonWriter): Unit = sectionTupleCodec.encodeValue(x.toTuple, out)
  //  override def nullValue: Section                             = null
  //}
  //
  //implicit val SastEncoder: JsonValueCodec[List[Sast]] =
  //  JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))

  val mapCodec: JsonValueCodec[Map[String, String]] = JsonCodecMaker.make

  case class Reference(file: String, start: Int, end: Int)

  implicit val rferenceRW: JsonValueCodec[Map[String, List[Reference]]] = JsonCodecMaker.make

}
