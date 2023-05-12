package bob

object Bob {

  def response(s: String): String = {
    val t = s.trim
    if (t.isEmpty) "Fine. Be that way!"
    else
      t match {
        case t if isUpper(t) && t.endsWith("?") =>
          "Calm down, I know what I'm doing!"
        case t if t.endsWith("?") => "Sure."
        case t if t.isEmpty       => "Fine. Be that way!"
        case t if isUpper(t)      => "Whoa, chill out!"
        case _                    => "Whatever."
      }
  }

  private def isUpper(s: String): Boolean = {
    val letters = s.filter(c => c.isLetter)
    letters.nonEmpty & letters.forall(c => c.isUpper)
  }

}
