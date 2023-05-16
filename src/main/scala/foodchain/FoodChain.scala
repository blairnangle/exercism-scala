package foodchain

import scala.annotation.tailrec

object FoodChain {

  def recite(start: Int, end: Int): String = {
    (start to end)
      .map(v => construct(v, List(), first = true).reduce(_ + _))
      .reduce(_ + "\n\n" + _) + "\n\n"
  }

  @tailrec
  private def construct(
      verse: Int,
      acc: List[String],
      first: Boolean = false
  ): List[String] = {
    val animalAndComment = getAnimalAndComment(verse)
    val animal = animalAndComment._1
    val comment = animalAndComment._2

    if (first) {
      if (verse == 8)
        acc.appended(s"I know an old lady who swallowed a $animal.\n$comment")
      else
        construct(
          verse,
          acc.prepended(
            s"I know an old lady who swallowed a $animal.\n${if (verse == 1) ""
              else comment + '\n'}"
          )
        )
    } else {
      verse match {
        case verse if verse == 1 =>
          acc.appended(
            "I don't know why she swallowed the fly. Perhaps she'll die."
          )
        case _ =>
          val previousAnimalAndComment = getAnimalAndComment(verse - 1)
          val previousAnimal = previousAnimalAndComment._1
          construct(
            verse - 1,
            acc.appended(
              s"She swallowed the $animal to catch the $previousAnimal${if (verse == 3) " that wriggled and jiggled and tickled inside her"
                else ""}.\n"
            )
          )
      }
    }
  }

  private def getAnimalAndComment(verse: Int): (String, String) = {
    verse match {
      case verse if verse == 1 =>
        ("fly", "I don't know why she swallowed the fly.")
      case verse if verse == 2 =>
        ("spider", "It wriggled and jiggled and tickled inside her.")
      case verse if verse == 3 =>
        ("bird", "How absurd to swallow a bird!")
      case verse if verse == 4 =>
        ("cat", "Imagine that, to swallow a cat!")
      case verse if verse == 5 =>
        ("dog", "What a hog, to swallow a dog!")
      case verse if verse == 6 =>
        ("goat", "Just opened her throat and swallowed a goat!")
      case verse if verse == 7 =>
        ("cow", "I don't know how she swallowed a cow!")
      case verse if verse == 8 =>
        ("horse", "She's dead, of course!")
    }
  }

}
