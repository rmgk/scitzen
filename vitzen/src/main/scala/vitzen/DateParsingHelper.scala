package vitzen

import java.time.LocalDateTime
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}

object DateParsingHelper {
  val timeFormatter: DateTimeFormatter = new DateTimeFormatterBuilder()
  .parseCaseInsensitive()
  .append(DateTimeFormatter.ISO_LOCAL_DATE)
  .optionalStart()
  .optionalStart().appendLiteral('T').optionalEnd()
  .optionalStart().appendLiteral(' ').optionalEnd()
  .append(DateTimeFormatter.ISO_LOCAL_TIME)
  .optionalEnd()
  .optionalStart().appendOffsetId().optionalEnd()
  .toFormatter()

  def parseDate(dateString: String): LocalDateTime = {
    if (dateString == null) return LocalDateTime.MIN
    val temporal = DateParsingHelper.timeFormatter.parse(dateString)
    LocalDateTime.from(temporal)
  }

}
