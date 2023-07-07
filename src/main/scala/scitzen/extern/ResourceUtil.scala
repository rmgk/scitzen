package scitzen.extern

import java.io.ByteArrayOutputStream
import scala.util.Using

object ResourceUtil {
  def load(path: String): Array[Byte] =
    Option(getClass.getClassLoader.getResourceAsStream(s"de.rmgk.scitzen/$path")).map { rs =>
      Using.resource(rs) { ars =>
        val bo = new ByteArrayOutputStream(14000) // size estimate from file snapshot
        ars.transferTo(bo)
        bo.toByteArray
      }
    }.getOrElse(Array.emptyByteArray)
}
