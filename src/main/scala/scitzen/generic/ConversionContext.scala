package scitzen.generic

import cats.data.Chain

/** The conversion context, used to keep state of in the conversion. */
case class ConversionContext[T](data: T, scope: Scope = new Scope(1)) {
  def ret[U](d: U): ConversionContext[U] = copy(data = d)
  def map[U](f: T => U): ConversionContext[U] = ret(f(data))
  def prepend[I](values: I*)(implicit ev: T <:< Chain[I]): ConversionContext[Chain[I]] =
    map(data => Chain.fromSeq(values) ++ data)

  def +: [I](value: I)(implicit ev: T <:< Chain[I]): ConversionContext[Chain[I]] =
    map(data => value +: data)
    def :+ [I](value: I)(implicit ev: T <:< Chain[I]): ConversionContext[Chain[I]] =
    map(data => data :+ value)
  def empty: ConversionContext[Chain[String]] = ret(Chain.empty[String])
  def single: ConversionContext[Chain[T]] = ret(Chain.one(data))
}

class Scope(val level: Int) extends AnyVal {
  def inc: Scope = {
    new Scope(level + 1)
  }
}
