package house

import scala.annotation.tailrec

object House {

  def recite(start: Int, end: Int): String = {
    val recitals = (start to end)
      .map {
        case start if start == 1 => s"${firstLine}"
        case v                   => construct(v, List(), first = true)
      }

    recitals.reduce(_ + "\n" + _) + "\n\n"
  }

  @tailrec
  private def construct(
      verse: Int,
      acc: List[String],
      first: Boolean
  ): String = {
    if (verse == 1)
      acc.appended(" in the house that Jack built.").reduce(_ + _)
    else
      construct(verse - 1, acc.appended(buildLine(verse, first)), first = false)
  }

  private def buildLine(verse: Int, first: Boolean): String = {
    val subjectAndVerb = getSubjectAndVerb(verse)
    s"${if (first) "This is " else " "}the ${subjectAndVerb._1} that ${subjectAndVerb._2}"
  }

  private def firstLine: String = {
    "This is the house that Jack built."
  }

  private def getSubjectAndVerb(verse: Int): (String, String) = {
    verse match {
      case verse if verse == 2  => ("malt", "lay")
      case verse if verse == 3  => ("rat", "ate")
      case verse if verse == 4  => ("cat", "killed")
      case verse if verse == 5  => ("dog", "worried")
      case verse if verse == 6  => ("cow with the crumpled horn", "tossed")
      case verse if verse == 7  => ("maiden all forlorn", "milked")
      case verse if verse == 8  => ("man all tattered and torn", "kissed")
      case verse if verse == 9  => ("priest all shaven and shorn", "married")
      case verse if verse == 10 => ("rooster that crowed in the morn", "woke")
      case verse if verse == 11 => ("farmer sowing his corn", "kept")
      case verse if verse == 12 =>
        ("horse and the hound and the horn", "belonged to")
    }
  }

}
