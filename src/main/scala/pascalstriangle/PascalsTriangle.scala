package pascalstriangle

import scala.annotation.tailrec

object PascalsTriangle {

  def rows(n: Int): List[List[Int]] = {
    @tailrec
    def loop(
        currentRowNumber: Int,
        terminatingRowNumber: Int,
        acc: List[List[Int]]
    ): List[List[Int]] = {
      if (currentRowNumber == terminatingRowNumber)
        acc
      else {
        loop(
          currentRowNumber + 1,
          terminatingRowNumber,
          if (acc.isEmpty) acc.appended(List(1))
          else acc.appended(buildNextRow(acc.last))
        )
      }
    }

    if (n < 0) List()
    else loop(0, n, List())
  }

  private def buildNextRow(row: List[Int]): List[Int] = {
    if (row.length == 1) List(1, 1)
    else {
      row.sliding(2).map(l => l.sum).toList.prepended(1).appended(1)
    }
  }

}
