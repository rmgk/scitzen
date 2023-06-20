package scitzen.sast

import scitzen.sast.Attribute.{Nested, Plain, Positional}

case class Attributes(raw: Seq[Attribute]) {

  lazy val positional: Seq[Text] = raw.collect { case Positional(text, _) => text }
  lazy val argumentsT: Seq[Text] = positional.dropRight(1)
  lazy val targetT: Text         = positional.last
  lazy val named: Map[String, String] = raw.collect {
    case Plain(id, value) => (id, value)
  }.toMap
  lazy val nested: Seq[(String, Attributes)] = raw.collect {
    case Nested(id, attr) => (id, attr)
  }
  lazy val nestedMap: Map[String, Attributes] = nested.toMap
  lazy val attrMap: Map[String, Attribute]    = raw.iterator.filter(_.id.nonEmpty).map(a => Tuple2(a.id, a)).toMap

  lazy val legacyPositional: Seq[String] = positional.map(_.plainString)
  lazy val arguments: Seq[String]        = legacyPositional.dropRight(1)
  lazy val target: String                = legacyPositional.last
  lazy val text: Text                    = positional.head

  def definition(name: String): Option[String] =
    nestedMap.get("definitions").flatMap(_.named.get(name))

  def append(other: Seq[Attribute]): Attributes  = Attributes(raw ++ other)
  def prepend(other: Seq[Attribute]): Attributes = Attributes(other ++ raw)
  def remove(key: String): Attributes = Attributes(raw.filterNot {
    case Plain(`key`, _)  => true
    case Nested(`key`, _) => true
    case other            => false
  })
  def updated(key: String, value: String) = {
    remove(key).append(List(Attribute.apply(key, value)))
  }
}

object Attributes {
  def target(string: String): Attributes = Attribute("", string).toAttributes
  val empty                              = Attributes(Nil)
}

sealed trait Attribute {
  def toAttributes: Attributes = Attributes(List(this))
  def id: String
}

object Attribute {
  def apply(id: String, value: String): Attribute =
    if (id.isBlank) Positional(Text(List(InlineText(value))), value) else Plain(id, value)

  case class Plain(id: String, value: String)      extends Attribute
  case class Nested(id: String, inner: Attributes) extends Attribute
  case class Positional(text: Text, string: String) extends Attribute {
    override def id = ""
  }
  object Positional {
    def apply(string: String): Positional =
      Positional(Text.of(string), string)
  }

}
