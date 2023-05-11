package accumulate

import scala.annotation.tailrec

class Accumulate {
  def accumulate[A, B](f: (A) => B, list: List[A]): List[B] = {
    @tailrec
    def loop[C, D](
        g: (C) => D,
        initialList: List[C],
        accumulatedList: List[D]
    ): List[D] = {
      if (initialList.nonEmpty)
        loop[C, D](g, initialList.tail, accumulatedList :+ g(initialList.head))
      else accumulatedList
    }
    loop[A, B](f, list, List())
  }

}
