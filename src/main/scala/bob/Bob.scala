package bob

object Bob {

  private def hasAllUpperLetters(s: String): Boolean = {
    val letters = s.filter(c => c.isLetter)
    letters.nonEmpty & letters.forall(c => c.isUpper)
  }

  def response(statement: String): String = {
    statement match {
      case statement if statement.trim().endsWith("?") =>
        statement match {
          case statement if hasAllUpperLetters(statement) =>
            "Calm down, I know what I'm doing!"
          case _ => "Sure."
        }
      case statement if hasAllUpperLetters(statement) => "Whoa, chill out!"
      case statement if statement.forall(c => c.isWhitespace) =>
        "Fine. Be that way!"
      case _ => "Whatever."
    }
  }
}
