package isogram

import scala.annotation.tailrec

object Isogram {

  def isIsogram(word: String): Boolean = {
    isIsogram(word, Set())
  }

  @tailrec
  private def isIsogram(word: String, seen: Set[Char]): Boolean = {
    if (word.length <= 1) true
    else {
      if (seen.contains(word.head)) false
      else
        isIsogram(
          word.tail,
          if (word.head.isLetter) seen + word.head.toLower else seen
        )
    }
  }

}
