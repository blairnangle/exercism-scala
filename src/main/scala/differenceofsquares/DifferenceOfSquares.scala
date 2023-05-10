package differenceofsquares

object DifferenceOfSquares {

  def sumOfSquares(n: Int): Int = {
    generateRange(n).map(i => math.pow(i, 2)).sum.toInt
  }

  def squareOfSum(n: Int): Int = {
    math.pow(generateRange(n).sum, 2).toInt
  }

  def differenceOfSquares(n: Int): Int = {
    squareOfSum(n) - sumOfSquares(n)
  }

  private def generateRange(n: Int): Range.Inclusive = {
    Range.inclusive(1, n)
  }

}
