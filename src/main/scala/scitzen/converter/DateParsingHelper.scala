package scitzen.converter

import java.time.LocalDateTime
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}

object DateParsingHelper {
  val relaxedISODateTimeParser: DateTimeFormatter = new DateTimeFormatterBuilder()
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
    val temporal = DateParsingHelper.relaxedISODateTimeParser.parse(dateString)
    LocalDateTime.from(temporal)
  }

  val dateOnlyOutput: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE
  val monthDayTime: DateTimeFormatter = DateTimeFormatter.ofPattern("MM-dd HH:mm")

}
