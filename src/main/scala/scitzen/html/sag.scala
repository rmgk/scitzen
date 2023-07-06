package scitzen.html

import de.rmgk.{Chain, delay}
import de.rmgk.delay.{Sync, SyncCompanion}

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import scala.annotation.implicitNotFound
import scala.quoted.*
import scala.language.dynamics
import scala.compiletime.summonInline
import scala.compiletime.*

object sag {

  inline def Recipe: SyncCompanion[SagContext] = new SyncCompanion[SagContext] {}

  class SagContext(val baos: ByteArrayOutputStream = new ByteArrayOutputStream(8096)) {
    def resultString: String = baos.toString(StandardCharsets.UTF_8)
  }

  object write:
    inline def apply(using inline sc: SagContext)(inline bytes: Array[Byte]) = sc.baos.write(bytes)
    inline def apply(using inline sc: SagContext)(inline str: String) =
      sc.baos.write(str.getBytes(StandardCharsets.UTF_8))
    inline def apply(using inline sc: SagContext)(inline byte: Int) = sc.baos.write(byte)
    inline def encodedString(using inline sc: SagContext)(inline value: String): Unit =
      Escaping.escape(value.getBytes(StandardCharsets.UTF_8), sc.baos)
    inline def attribute(using inline sc: SagContext)(inline name: Array[Byte], inline value: Recipe): Unit =
      write(' ')
      write(name)
      write('=')
      write('"')
      value.run
      write('"')
    inline def stringAttribute(using inline sc: SagContext)(inline name: Array[Byte], inline value: String): Unit =
      attribute(using sc)(name, Sag.String(value))
    inline def openTag(using inline sc: SagContext)(tag: Array[Byte]) =
      write('<')
      write(tag)
    inline def closeTag(using inline sc: SagContext)() =
      write('>')
    inline def closingTag(using inline sc: SagContext)(tag: Array[Byte]) =
      write('<')
      write('/')
      write(tag)
      write('>')

  @implicitNotFound("Do not know how to use ${T} as content")
  trait SagContentWriter[-T] {
    def convert(value: T): Recipe
  }

  object SagContentWriter {
    given recipeWriter: SagContentWriter[Recipe] with {
      override inline def convert(value: Recipe): Recipe = value
    }

    given stringWriter: SagContentWriter[String] with {
      override inline def convert(value: String): Recipe = Recipe(write.encodedString(value))
    }

    inline given seqSagWriter[T](using sw: SagContentWriter[T]): SagContentWriter[Seq[T]] with
      override inline def convert(value: Seq[T]): Recipe = Sync:
        value.foreach(v => sw.convert(v).run)

    given chainSagWriter[T](using sw: SagContentWriter[T]): SagContentWriter[Chain[T]] with
      override inline def convert(value: Chain[T]): Recipe = Sync:
        value.foreach(v => sw.convert(v).run)

    given optionSagWriter[T](using sw: SagContentWriter[T]): SagContentWriter[Option[T]] with
      override inline def convert(value: Option[T]): Recipe = Sync:
        value.foreach(v => sw.convert(v).run)

  }

  @implicitNotFound("Do not know how to use ${T} as an attribute value")
  trait SagAttributeValueWriter[-T] {
    def convert(attr: Array[Byte], value: T): Recipe
  }

  object SagAttributeValueWriter {

    given stringAw: SagAttributeValueWriter[String] with {
      override def convert(attr: Array[Byte], value: String): Recipe = Recipe(write.stringAttribute(attr, value))
    }

    given optAw[T](using sw: SagAttributeValueWriter[T]): SagAttributeValueWriter[Option[T]] with {
      override inline def convert(name: Array[Byte], value: Option[T]): Recipe = Sync:
        value match
          case Some(v) => sw.convert(name, v).run
          case None    => ()
    }

    given booleanAw: SagAttributeValueWriter[Boolean] with {
      override inline def convert(name: Array[Byte], value: Boolean): Recipe = Sync:
        inline constValueOpt[value.type] match
          case Some(v) => inline if v then write(name)
          case other   => if value then write(name)
    }
  }

  type Recipe = de.rmgk.delay.Sync[SagContext, Unit]

  object Sag extends Dynamic {
    inline def applyDynamicNamed(inline name: String)(inline args: (String, Any)*): Recipe =
      ${ applyDynamicImpl('{ name }, '{ args }) }

    inline def applyDynamic(inline name: String)(inline args: Any*): Recipe =
      ${ applyDynamicImpl('{ name }, '{ args }) }

    inline def Raw(inline content: String): Recipe = Sync:
      write(content)

    inline def Nothing: Recipe = Sync { () }

    inline def String(inline other: String): Recipe = Recipe(write.encodedString(other))
  }

  def applyDynamicImpl(name: Expr[String], args: Expr[Seq[Any]])(using quotes: Quotes): Expr[Recipe] =
    import quotes.*
    import quotes.reflect.report

    val tagname = name.valueOrAbort

    if !validHtml5Tags.contains(tagname) then report.errorAndAbort(s"not a html5 tag $tagname", name)

    val attributes = args match
      case Varargs(args) =>
        args.map:
          case '{ new Tuple2[String, Any](${ Expr(y1) }, ${ y2 }: τ) } => y1 -> y2
          case '{ Tuple2[String, Any](${ Expr(y1) }, $y2) }            => y1 -> y2
          case '{ (${ Expr(y1) }: String) -> $y2 }                     => y1 -> y2
          case other                                                   => "" -> other
      case other =>
        report.errorAndAbort(s"not varargs ${other.show}", other)

    '{
      val tag = ${ Expr(tagname) }.getBytes(StandardCharsets.UTF_8)
      Sync { ctx ?=>
        write.openTag(tag)

        // write attributes
        ${
          Expr.block(
            attributes.filter(_._1.nonEmpty).toList.map: a =>
              if
                val list = attributeScopes.getOrElse(a._1, List("+ Nothing"))
                !(list.isEmpty || list.contains(tagname) || a._1.startsWith("data-"))
              then
                report.errorAndAbort(s"not a valid attribute <$tagname ${a._1}>")

              val attrName = '{ ${ Expr(a._1) }.getBytes(StandardCharsets.UTF_8) }

              a._2 match
                case '{ $v: String } =>
                  '{
                    write.stringAttribute(
                      $attrName,
                      ${ v }
                    )
                  }
                case '{ $v: τ } =>
                  '{
                    summonInline[scitzen.html.sag.SagAttributeValueWriter[τ]].convert(
                      $attrName,
                      ${ v }
                    ).run
                  }
            ,
            '{}
          )
        }
        write.closeTag()

        // write children
        ${
          tagname match
            // void tag
            case "!DOCTYPE html" | "area" | "base" | "br" | "col" | "embed" | "hr" | "img" | "input" | "link" | "meta" | "source" | "track" | "wbr" =>
              if attributes.filter(_._1 == "").nonEmpty then
                report.errorAndAbort(s"may not have children $tagname", args)
              '{}
            case other =>
              '{
                ${
                  Expr.block(
                    attributes.filter(_._1 == "").toList.map: attr =>
                      attr._2 match
                        case '{ $v: String } =>
                          '{
                            write.encodedString($v)
                          }
                        case '{ $v: Recipe } =>
                          '{
                            ${ v }.run
                          }
                        case '{ $v: τ } =>
                          '{
                            summonInline[SagContentWriter[τ]].convert($v).run
                          }
                    ,
                    '{}
                  )
                }
                write.closingTag(tag)
              }
        }
      }

    }

}
