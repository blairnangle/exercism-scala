package gigasecond

import java.time.{LocalDate, LocalDateTime, LocalTime}

object Gigasecond {

  def add(startDate: LocalDate): LocalDateTime = {
    addOneGigasecond(
      LocalDateTime
        .of(startDate, LocalTime.of(0, 0))
    )
  }

  def add(startDateTime: LocalDateTime): LocalDateTime = {
    addOneGigasecond(startDateTime)
  }

  private def addOneGigasecond(startDateTime: LocalDateTime): LocalDateTime = {
    startDateTime.plusSeconds(math.pow(10, 9).toLong)
  }

}
