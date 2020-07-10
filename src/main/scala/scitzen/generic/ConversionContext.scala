package scitzen.generic

import java.nio.file.Path

import better.files.File
import cats.data.Chain
import scitzen.extern.ConvertTask
import scitzen.generic.Sast.Section

case class SastRef(file: File, sast: Sast)

/** The conversion context, used to keep state of in the conversion. */
case class ConversionContext[T](
    data: T,
    katexMap: Map[String, String] = Map.empty,
    resourceMap: Map[File, Path] = Map.empty,
    tasks: List[ConvertTask] = Nil,
    labelledThings: Map[String, List[SastRef]] = Map.empty,
    uniquectr: Int = 0,
    stack: List[Sast] = Nil,
    includes: List[File] = Nil
) {

  def resolveRef(ref: String): List[SastRef] =
    labelledThings.getOrElse(ref, Nil)

  def requireInOutput(source: File, relative: Path): ConversionContext[T] = {
    copy(resourceMap = resourceMap.updated(source, relative))
  }

  def nextId: ConversionContext[Int] = copy(uniquectr = uniquectr + 1, data = uniquectr)

  def addRefTarget(ref: String, secref: SastRef): ConversionContext[SastRef] = {
    val old = labelledThings.getOrElse(ref, Nil)
    copy(labelledThings = labelledThings.updated(ref, secref :: old), data = secref)
  }

  def push(sast: Sast): ConversionContext[T] = {
    val droppedStack = sast match {
      case Section(_, level, _) =>
        stack.dropWhile {
          case Section(_, l, _) => l >= level
          case other            => false
        }
      case other => stack
    }
    copy(stack = sast :: droppedStack)
  }
  def pop(): ConversionContext[T] = copy(stack = stack.tail)

  lazy val stacklevel: Int = stack.dropWhile(_.isInstanceOf[Section])
    .collectFirst { case Section(_, level, _) => level.size }
    .getOrElse(0)

  def ret[U](d: U): ConversionContext[U]         = copy(data = d)
  def retc[U](d: U): ConversionContext[Chain[U]] = copy(data = Chain(d))
  def map[U](f: T => U): ConversionContext[U]    = ret(f(data))

  def +:[I](value: I)(implicit ev: T <:< Chain[I]): ConversionContext[Chain[I]] =
    map(data => value +: data)
  def :+[I](value: I)(implicit ev: T <:< Chain[I]): ConversionContext[Chain[I]] =
    map(data => data :+ value)
  def ++:[I](value: Chain[I])(implicit ev: T <:< Chain[I]): ConversionContext[Chain[I]] =
    map(data => value ++ data)

  def :++[I](value: Chain[I])(implicit ev: T <:< Chain[I]): ConversionContext[Chain[I]] =
    map(data => data ++ value)

  def empty[U]: ConversionContext[Chain[U]] = ret(Chain.empty[U])
  def single: ConversionContext[Chain[T]]   = ret(Chain.one(data))

  def fold[U, V](seq: Seq[U])(f: (ConversionContext[Chain[V]], U) => ConversionContext[Chain[V]])
      : ConversionContext[Chain[V]] =
    seq.foldLeft(ret(Chain.empty[V])) { (ctx, elem) =>
      val nctx = f(ctx, elem)
      nctx.map(data => ctx.data ++ data)
    }

  def katex(key: String, default: => String): (String, ConversionContext[T]) = {
    katexMap.get(key) match {
      case None =>
        val computed = default
        (computed, copy(katexMap = katexMap.updated(key, computed)))
      case Some(value) => (value, this)
    }
  }

  def execTasks(): ConversionContext[T] = {
    import scala.jdk.CollectionConverters._
    tasks.asJava.parallelStream().forEach { ct => ct.run() }
    copy(tasks = Nil)
  }

}

class Scope(val level: Int) extends AnyVal {
  def inc: Scope = {
    new Scope(level + 1)
  }
}
