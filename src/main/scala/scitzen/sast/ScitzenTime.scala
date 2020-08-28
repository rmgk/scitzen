package scitzen.sast

case class ScitzenDate(year: String, month: String, day: String) {
  def full = s"$year-$month-$day"
}
case class ScitzenTime(hour: String, minute: String, second: String) {
  def short = s"$hour:$minute"
  def iso   = s"$hour:$minute:${second}Z"
}
case class ScitzenDateTime(date: ScitzenDate, timeO: Option[ScitzenTime]) {
  def timeAppend: String   = timeO.fold("")(st => s" ${st.short}")
  def full: String         = s"${date.full}$timeAppend"
  def monthDayTime: String = s"${date.month}-${date.day}$timeAppend"
  def year: String         = date.year
  def iso: String          = s"${date.full}${timeO.fold("")(_.iso)}"
}
object ScitzenDateTime {
  implicit val ord: Ordering[ScitzenDateTime] = Ordering.by(_.full)
}
