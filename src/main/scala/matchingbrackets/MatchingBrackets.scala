package matchingbrackets

import scala.annotation.tailrec

object MatchingBrackets {

  def isPaired(brackets: String): Boolean = {
    @tailrec
    def loop(
        b: String,
        stack: List[Char]
    ): Boolean = {
      val next = b.head
      if (b.length == 1) {
        if (opening.contains(next)) false
        else matching(stack.head, next)
      } else {
        if (opening.contains(next)) loop(b.tail, stack.appended(next))
        else {
          if (stack.isEmpty) false
          else {
            if (matching(stack.last, next)) loop(b.tail, stack.dropRight(1))
            else false
          }
        }
      }
    }

    val bracketsOnly = brackets.filter(c => (opening ++ closing).contains(c))
    if (bracketsOnly.isEmpty) true else loop(bracketsOnly, List())
  }

  private def matching(last: Char, next: Char): Boolean = {
    last match {
      case last if last == '(' => next == ')'
      case last if last == '[' => next == ']'
      case last if last == '{' => next == '}'
    }
  }

  private val opening: List[Char] = List('(', '[', '{')
  private val closing: List[Char] = List(')', ']', '}')

}
