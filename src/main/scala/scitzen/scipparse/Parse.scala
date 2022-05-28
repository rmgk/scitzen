package scitzen.scipparse

import scitzen.sast.{Inline, Prov, Sast}

import de.rmgk.scip.*

//case class ParsingAnnotation(content: String, failure: TracedFailure) extends Exception

object Parse {

  // type Result[T] = Either[ParsingAnnotation, T]

  // def parseResult[T](content: String, parser: P[_] => P[T], prov: Prov): Result[T] = {
  //  val parsed = fastparse.parse(
  //  content,
  //  { (p: P[_]) =>
  //    p.misc("provenanceOffset") = prov
  //    parser(p)
  //  }
  //  )
  //  parsed match {
  //    case Success(value, _) => Right(value)
  //    case f: Failure =>
  //      val traced = f.trace()
  //      println(
  //        s"failed to parse ${traced.longMsg}\nwhile parsing: ${content.substring(traced.index, math.min(traced.index + 80, content.length))}..."
  //        )
  //      Left(ParsingAnnotation(content, traced))
  //  }
  // }

  def parserDocument: Scip[Seq[Sast]] = BlockParsers.alternatives.list(Scip {}) <~ CompatParsers.End

  def documentUnwrap(blockContent: Array[Byte], prov: Prov): Seq[Sast] = {
    parserDocument.run0(Scx(blockContent, 0, 0, -1, "", false))
  }

  val allInlines = InlineParsers(Scip {false}, CompatParsers.End)

  def inlineUnwrap(paragraphString: String, prov: Prov): Seq[Inline] = {
    allInlines.full.run0(Scx(paragraphString).copy(tracing = false))._1
  }
}
