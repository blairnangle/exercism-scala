package grains

object Grains {

  private val grainsOnFirstSquare = 1
  private val factor: BigInt = 2
  private val nSquares: Int = 64

  def square(n: Int): Option[BigInt] = if (0 < n && n <= nSquares) {
    Option(grainsOnFirstSquare * factor.pow(n - 1))
  } else {
    None
  }

  def total: BigInt = {
    (1 to nSquares).map(n => square(n).get).sum
  }

}
