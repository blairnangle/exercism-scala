package scrabblescore

object ScrabbleScore {

  def score(word: String): Int = {
    word.map(l => lettersToScore(l)).sum
  }

  private def lettersToScore(letter: Char): Int = {
    val letterUpper = letter.toUpper
    letterUpper match {
      case letterUpper
          if List('A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T').contains(
            letterUpper
          ) =>
        1
      case upperLetter if List('D', 'G').contains(upperLetter)           => 2
      case upperLetter if List('B', 'C', 'M', 'P').contains(upperLetter) => 3
      case upperLetter if List('F', 'H', 'V', 'W', 'Y').contains(upperLetter) =>
        4
      case upperLetter if upperLetter == 'K'                   => 5
      case upperLetter if List('J', 'X').contains(upperLetter) => 8
      case upperLetter if List('Q', 'Z').contains(upperLetter) => 10
    }
  }

}
