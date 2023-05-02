package beersong

object BeerSong {

  def recite(initial: Int, iterations: Int): String = {
    Range
      .inclusive(initial, initial - iterations + 1, -1)
      .map(n => recite(n))
      .reduce(_ + "\n" + _)
  }

  private def recite(n: Int): String = {
    s"${bottles(n, startOfSentence = true)} of beer on the wall, ${bottles(n, startOfSentence = false)} of beer.\n${action(
        n
      )}, ${bottles(newBottleNumber(n), startOfSentence = false)} of beer on the wall.\n"
  }

  private def bottles(n: Int, startOfSentence: Boolean): String = {
    n match {
      case n if n == 0 => s"${if (startOfSentence) 'N' else 'n'}o more bottles"
      case n if n == 1 => s"${n} bottle"
      case _           => s"${n} bottles"
    }
  }

  private def action(n: Int): String = {
    n match {
      case n if n == 0 => "Go to the store and buy some more"
      case _ =>
        s"""Take ${if (n == 1) "it" else "one"} down and pass it around"""
    }
  }

  private def newBottleNumber(n: Int): Int = {
    if (n == 0) 99 else n - 1
  }

}
