package binarysearch

import scala.annotation.tailrec

object BinarySearch {

  def find(list: List[Int], elem: Int): Option[Int] = {
    binarySearch(list, elem, 0, list.length)
  }

  @tailrec
  private def binarySearch(
      list: List[Int],
      elem: Int,
      startIdx: Int,
      endIdx: Int
  ): Option[Int] = {
    val middleIdx = (startIdx + endIdx) / 2
    if (startIdx == endIdx) None
    else if (list(middleIdx) > elem)
      binarySearch(list, elem, startIdx, middleIdx)
    else if (list(middleIdx) < elem)
      binarySearch(list, elem, middleIdx + 1, endIdx)
    else Option(middleIdx)
  }
}
