package hamming

object Hamming {
  def distance(dnaStrandOne: String, dnaStrandTwo: String): Option[Int] = {
    (dnaStrandOne, dnaStrandTwo) match {
      case (dnaStrandOne, dnaStrandTwo) if dnaStrandOne == dnaStrandTwo =>
        Option(0)
      case (dnaStrandOne, dnaStrandTwo)
          if dnaStrandOne.length != dnaStrandTwo.length =>
        None
      case _ =>
        Option((dnaStrandOne zip dnaStrandTwo).count { case (one, two) =>
          one != two
        })
    }
  }
}
