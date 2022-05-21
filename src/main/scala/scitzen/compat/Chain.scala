package scitzen.compat

import scitzen.compat.Chain.*

import scala.annotation.tailrec
import scala.collection.immutable.Iterable
import scala.collection.{IterableFactory, IterableFactoryDefaults, IterableOps, mutable}

enum Chain[+A] {

  case nil                                        extends Chain[Nothing]
  case one[A](elem: A)                            extends Chain[A]
  case Wrap[A](underlying: Iterable[A])           extends Chain[A]
  case Append[A](left: Chain[A], right: Chain[A]) extends Chain[A]

  def iterator: Iterator[A] = this match
    case `nil`     => Iterator.empty
    case one(a)    => Iterator.single(a)
    case Wrap(seq) => seq.iterator
    case Append(left, right) => left.iterator ++ right.iterator

  def flatMap[B](f: A => Chain[B]): Chain[B] = iterator.foldLeft(Chain.empty[B])((c, a) => Append[B](c, f(a)))
  def map[B](f: A => B): Chain[B]            = Chain.fromSeq(iterator.map(f).toSeq)

  def +:[B >: A](other: B): Chain[B]        = Append(one(other), this)
  def :+[B >: A](other: B): Chain[B]        = Append(this, one(other))
  def ++[B >: A](other: Chain[B]): Chain[B] = Append(this, other)
  def toList: List[A]                       = iterator.toList

}

object Chain {

  def empty[A]: Chain[A]                = nil
  def apply[A](elem: A*): Chain[A]      = Wrap(elem)
  def fromSeq[A](seq: Seq[A]): Chain[A] = if seq.isEmpty then nil else Wrap(seq)

}
