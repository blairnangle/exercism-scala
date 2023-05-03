package darts

object Darts {

  def score(x: Double, y: Double): Int = {
    val r = Math.sqrt(scala.math.pow(x, 2) + scala.math.pow(y, 2))
    r match {
      case r if r <= 1  => 10
      case r if r <= 5  => 5
      case r if r <= 10 => 1
      case _            => 0
    }
  }

}
