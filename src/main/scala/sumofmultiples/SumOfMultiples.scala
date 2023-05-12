package sumofmultiples

object SumOfMultiples {

  def sum(factors: Set[Int], limit: Int): Int = {
    val allMultiples = factors.flatMap(f => computeMultiples(f, limit))
    allMultiples.filter(m => allMultiples.count(i => i == m) == 1).sum
  }

  private def computeMultiples(n: Int, l: Int): Seq[Int] = {
    0 until l by n
  }

}
