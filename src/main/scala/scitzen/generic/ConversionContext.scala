package scitzen.generic

import better.files.File
import cats.data.Chain

/** The conversion context, used to keep state of in the conversion. */
case class ConversionContext[T](data: T, scope: Scope = new Scope(1), externalContentResolver: ExternalContentResolver2) {


  def ret[U](d: U): ConversionContext[U] = copy(data = d)
  def retc[U](d: U): ConversionContext[Chain[U]] = copy(data = Chain(d))
  def map[U](f: T => U): ConversionContext[U] = ret(f(data))

  def +:[I](value: I)(implicit ev: T <:< Chain[I]): ConversionContext[Chain[I]] =
    map(data => value +: data)
  def :+[I](value: I)(implicit ev: T <:< Chain[I]): ConversionContext[Chain[I]] =
    map(data => data :+ value)
  def ++:[I](value: Chain[I])(implicit ev: T <:< Chain[I]): ConversionContext[Chain[I]] =
    map(data => value ++ data)

  def empty[U]: ConversionContext[Chain[U]] = ret(Chain.empty[U])
  def single: ConversionContext[Chain[T]] = ret(Chain.one(data))

  def image(currentFile: File, target: String): ConversionContext[Option[String]] =
    externalContentResolver.resolve(currentFile, target) match {
      case None              => ret(None)
      case Some((ecr, path)) => copy(externalContentResolver = ecr, data = Some(path))
    }

  def convert(tlblock: Sast.TLBlock, value: "pdf"): Seq[Sast] = {
    externalContentResolver.convert(tlblock, value)
  }

  def fold[U, V](seq: Seq[U])
                (f: (ConversionContext[Chain[V]], U) => ConversionContext[Chain[V]])
  : ConversionContext[Chain[V]] =
    seq.foldLeft(ret(Chain.empty[V])) { (ctx, elem) =>
      val nctx = f(ctx, elem)
      nctx.map(data => ctx.data ++ data)
    }


}

class Scope(val level: Int) extends AnyVal {
  def inc: Scope = {
    new Scope(level + 1)
  }
}
