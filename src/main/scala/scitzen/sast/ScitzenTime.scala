package scitzen.sast

case class ScitzenDate(year: String, month: Option[String], day: Option[String]) {
  require(if day.isDefined then month.isDefined else true)
  def full = Iterator(Some(year), month, day).flatten.mkString("-")
}
case class ScitzenTime(hour: String, minute: Option[String], second: Option[String]) {
  require(if second.isDefined then minute.isDefined else true)
  def short = hour + minute.map(m => s":$m").getOrElse("")
  def iso   = Iterator(Some(hour), minute, second).flatten.mkString("", ":", "Z")
}
case class ScitzenDateTime(date: ScitzenDate, timeO: Option[ScitzenTime]) {
  def timeAppend: String   = timeO.fold("")(st => s" ${st.short}")
  def full: String         = s"${date.full}$timeAppend"
  def dayTime: String      = s"${date.day}$timeAppend"
  def monthDayTime: String = s"${date.month}-$dayTime"
  def year: String         = date.year
  def iso: String          = s"${date.full}${timeO.fold("")(_.iso)}"
}
object ScitzenDateTime {
  implicit val ord: Ordering[ScitzenDateTime] = Ordering.by(_.full)
}
