package acronym

object Acronym {

  def abbreviate(phrase: String): String = {
    phrase
      .split("[ \\-]+")
      .map(w => w.head)
      .map(c => c.toUpper)
      .mkString
  }

}
