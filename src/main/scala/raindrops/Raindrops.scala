package raindrops

object Raindrops {

  def convert(n: Int): String = {
    val sounds = List(3, 5, 7).filter(f => n % f == 0).map(f => chooseSound(f))
    if (sounds.isEmpty) n.toString else sounds.reduce(_ + _)
  }

  private def chooseSound(f: Int): String = {
    f match {
      case f if f == 3 => "Pling"
      case f if f == 5 => "Plang"
      case f if f == 7 => "Plong"
    }
  }

}
