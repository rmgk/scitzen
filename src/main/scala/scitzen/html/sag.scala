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
    def append(bytes: Array[Byte], offset: Int, length: Int) =
      baos.write(bytes, offset, length)

    def append(bytes: Array[Byte]): Unit =
      baos.writeBytes(bytes)
    def resultString: String = baos.toString(StandardCharsets.UTF_8)
  }

  @implicitNotFound("Do not know how to use ${T} as content")
  trait SagContentWriter[-T] {
    def convert(value: T): Recipe
  }

  object SagContentWriter {
    given recipeWriter: SagContentWriter[Recipe] with {
      override inline def convert(value: Recipe): Recipe = value
    }

    inline def writeEncodedString(inline value: String): Recipe = Sync: ctx ?=>
      Escaping.escape(value.getBytes(StandardCharsets.UTF_8), ctx.baos)

    given stringWriter: SagContentWriter[String] with {
      override inline def convert(value: String): Recipe = writeEncodedString(value)
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

    inline def writeAttr(inline name: Array[Byte], inline value: Recipe): Recipe = Sync:
      write(' ')
      write(name)
      write('=')
      write('"')
      value.run
      write('"')

    inline def writeStringAttr(inline name: Array[Byte], inline value: String): Recipe =
      writeAttr(name, Sag.String(value))

    given stringAw: SagAttributeValueWriter[String] with {
      override def convert(attr: Array[Byte], value: String): Recipe = Sync:
        writeAttr(attr, Sag.String(value)).run
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

  inline def write(using inline sc: SagContext)(inline bytes: Array[Byte]) = sc.baos.write(bytes)
  inline def write(using inline sc: SagContext)(inline str: String) = sc.baos.write(str.getBytes(StandardCharsets.UTF_8))
  inline def write(using inline sc: SagContext)(inline byte: Int)   = sc.baos.write(byte)

  type Recipe = de.rmgk.delay.Sync[SagContext, Unit]

  object Sag extends Dynamic {
    inline def applyDynamicNamed(inline name: String)(inline args: (String, Any)*): Recipe =
      ${ applyDynamicImpl('{ name }, '{ args }) }

    inline def applyDynamic(inline name: String)(inline args: Any*): Recipe =
      ${ applyDynamicImpl('{ name }, '{ args }) }

    inline def Raw(inline content: String): Recipe = Sync:
      write(content)

    inline def Nothing: Recipe = Sync { () }

    inline def String(inline other: String): Recipe = SagContentWriter.writeEncodedString(other)
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
      Sync {
        write('<')
        write(tag)

        // write attributes
        ${
          Expr.ofSeq(
            attributes.filter(_._1.nonEmpty).map: a =>
              if
                val list = attributeScopes.getOrElse(a._1, List("+ Nothing"))
                !(list.isEmpty || list.contains(tagname) || a._1.startsWith("data-"))
              then
                report.errorAndAbort(s"not a valid attribute <$tagname ${a._1}>")

              val attrName = '{ ${ Expr(a._1) }.getBytes(StandardCharsets.UTF_8) }

              a._2 match
                case '{ $v: String } =>
                  '{
                    SagAttributeValueWriter.writeStringAttr(
                      $attrName,
                      ${ v }
                    ).run
                  }
                case '{ $v: τ } =>
                  '{
                    summonInline[scitzen.html.sag.SagAttributeValueWriter[τ]].convert(
                      $attrName,
                      ${ v }
                    ).run
                  }
          )
        }
        write('>')

        // write children
        ${
          tagname match
            // void tag
            case "!DOCTYPE html" | "area" | "base" | "br" | "col" | "embed" | "hr" | "img" | "input" | "link" | "meta" | "source" | "track" | "wbr" =>
              if attributes.filter(_._1 == "").nonEmpty then
                report.errorAndAbort(s"may not have children $tagname", args)
              '{ () }
            case other =>
              '{
                ${
                  Expr.ofSeq(attributes.filter(_._1 == "").map: attr =>
                    attr._2 match
                      case '{ $v: String } =>
                        '{
                          SagContentWriter.writeEncodedString($v).run
                        }
                      case '{ $v: Recipe } =>
                        '{
                          ${v}.run
                        }
                      case '{ $v: τ } =>
                        '{
                          summonInline[SagContentWriter[τ]].convert($v).run
                        }
                  )
                }
                write('<')
                write('/')
                write(tag)
                write('>')
              }
        }
      }

    }

}
