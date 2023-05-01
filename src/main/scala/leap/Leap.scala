package leap

object Leap {
  def leapYear(year: Int): Boolean = {
    year match {
      case year if year % 400 == 0 => true
      case year if year % 100 == 0 => false
      case year if year % 4 == 0   => true
      case _                       => false
    }
  }
}
