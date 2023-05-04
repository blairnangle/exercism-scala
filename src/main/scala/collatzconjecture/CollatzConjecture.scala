package collatzconjecture

import scala.annotation.tailrec

object CollatzConjecture {

  def steps(n: Int): Option[Int] = {
    collatz(n, 0)
  }

  @tailrec
  private def collatz(n: Int, nSteps: Int): Option[Int] = {
    n match {
      case n if n <= 0     => None
      case n if n == 1     => Option(nSteps)
      case n if n % 2 == 0 => collatz(n / 2, nSteps + 1)
      case _               => collatz(3 * n + 1, nSteps + 1)
    }
  }
}
