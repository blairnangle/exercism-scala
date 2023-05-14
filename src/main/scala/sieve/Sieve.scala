package sieve

import scala.annotation.tailrec

object Sieve {

  def primes(n: Int): List[Int] = {
    @tailrec
    def loop(
        spread: IndexedSeq[Int],
        start: Int,
        acc: IndexedSeq[Int]
    ): List[Int] = {
      val filteredForStart = spread.filter(i => i == start || i % start != 0)
      if (start == n - 1 || filteredForStart.length < 1) acc.toList
      else
        loop(
          filteredForStart.tail,
          start + 1,
          acc.appended(filteredForStart.head)
        )
    }

    loop(Range.inclusive(2, n), 2, IndexedSeq())
  }

}
