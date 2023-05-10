package pangram

object Pangrams {

  def isPangram(input: String): Boolean = {
    val inputLower = input.toLowerCase
    Range
      .inclusive(97, 97 + 26)
      .map(i => i.toChar)
      .count(l => inputLower.contains(l.toLower)) == 26
  }

}
