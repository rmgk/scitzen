package scitzen.generic

import java.nio.file.Path

import better.files.File
import cats.data.Chain

case class SastRef(file: File, sast: Sast)

/** The conversion context, used to keep state of in the conversion. */
case class ConversionContext[T]
(data: T,
 scope: Scope = new Scope(1),
 katexMap: Map[String, String] = Map.empty,
 resourceMap: Map[File, Path] = Map.empty,
 tasks: List[ConvertTask] = Nil,
 labelledSections: Map[String, List[SastRef]] = Map.empty,
 uniquectr: Int = 0,
 stack: List[Sast] = Nil
) {


  def resolveTarget(target: String): List[SastRef] =
    labelledSections.getOrElse(target, Nil)


  def requireInOutput(source: File, relative: Path): ConversionContext[T] = {
    copy(resourceMap = resourceMap.updated(source, relative))
  }

  def nextId: ConversionContext[Int] = copy(uniquectr = uniquectr + 1, data = uniquectr)

  def addSection(ref: String, secref: SastRef): ConversionContext[SastRef] = {
    val old = labelledSections.getOrElse(ref, Nil)
    copy(labelledSections = labelledSections.updated(ref, secref :: old), data = secref)
  }

  def push(sast: Sast): ConversionContext[T] = copy(stack = sast :: stack)
  def pop(): ConversionContext[T] = copy(stack = stack.tail)


  def ret[U](d: U): ConversionContext[U] = copy(data = d)
  def retc[U](d: U): ConversionContext[Chain[U]] = copy(data = Chain(d))
  def map[U](f: T => U): ConversionContext[U] = ret(f(data))

  def +:[I](value: I)(implicit ev: T <:< Chain[I]): ConversionContext[Chain[I]] =
    map(data => value +: data)
  def :+[I](value: I)(implicit ev: T <:< Chain[I]): ConversionContext[Chain[I]] =
    map(data => data :+ value)
  def ++:[I](value: Chain[I])(implicit ev: T <:< Chain[I]): ConversionContext[Chain[I]] =
    map(data => value ++ data)

  def :++[I](value: Chain[I])(implicit ev: T <:< Chain[I]): ConversionContext[Chain[I]] =
    map(data => data ++ value)

  def empty[U]: ConversionContext[Chain[U]] = ret(Chain.empty[U])
  def single: ConversionContext[Chain[T]] = ret(Chain.one(data))

  def withScope[U](scope: Scope)(f: ConversionContext[T] => ConversionContext[U]): ConversionContext[U] =
    f(copy(scope = scope)).copy(scope = this.scope)
  def incScope[U](f: ConversionContext[T] => ConversionContext[U]): ConversionContext[U] =
    withScope(scope.inc)(f)

  def fold[U, V](seq: Seq[U])
                (f: (ConversionContext[Chain[V]], U) => ConversionContext[Chain[V]])
  : ConversionContext[Chain[V]] =
    seq.foldLeft(ret(Chain.empty[V])) { (ctx, elem) =>
      val nctx = f(ctx, elem)
      nctx.map(data => ctx.data ++ data)
    }

  def katex(key: String, default: => String): (String, ConversionContext[T]) = {
    katexMap.get(key) match {
      case None        =>
        val computed = default
        (computed, copy(katexMap = katexMap.updated(key, computed)))
      case Some(value) => (value, this)
    }
  }


}

class Scope(val level: Int) extends AnyVal {
  def inc: Scope = {
    new Scope(level + 1)
  }
}
