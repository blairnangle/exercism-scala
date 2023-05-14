package say

import scala.annotation.tailrec

object Say {

  def inEnglish(n: Long): Option[String] = {
    if (n < 20) below20(n.toInt)
    else {
      val nString = n.toString
      val nDigits = nString.length
      if (1 < nDigits && nDigits < 13) {
        Option(
          buildWordList(nString, nDigits - 1, List())
            .filter(o => o.nonEmpty)
            .map(o => o.get)
            .reduce((l, r) => combineWords(l, r))
        )
      } else None
    }
  }

  @tailrec
  private def buildWordList(
      nString: String,
      powerOfTen: Int,
      acc: List[Option[String]]
  ): List[Option[String]] = {
    if (nString.length == 1) {
      acc.appended(
        mapPowerOfTenAndDigitToWord(powerOfTen, nString.head.asDigit)
      )
    } else {
      buildWordList(
        nString.tail,
        powerOfTen - 1,
        acc.appended(
          mapPowerOfTenAndDigitToWord(powerOfTen, nString.head.asDigit)
        )
      )
    }
  }

  private def combineWords(l: String, r: String): String = {
    if (multiplesOfTenBelow100.contains(l.split(" ").last)) s"$l-$r"
    else s"$l $r"
  }

  private def mapPowerOfTenAndDigitToWord(p: Int, d: Int): Option[String] = {
    if (d == 0) None
    else
      p match {
        case p if p == 0 => singles(d)
        case p if p == 1 => tens(d)
        case p if p == 2 => Option(s"${singles(d).get} hundred")
        case p if p == 3 => Option(s"${singles(d).get} thousand")
        case p if p == 4 =>
          Option(s"${mapPowerOfTenAndDigitToWord(1, d).getOrElse("")}")
        case p if p == 5 =>
          Option(s"${mapPowerOfTenAndDigitToWord(2, d).getOrElse("")}")
        case p if p == 6 => Option(s"${singles(d).get} million")
        case p if p == 7 =>
          Option(s"${mapPowerOfTenAndDigitToWord(1, d).getOrElse("")}")
        case p if p == 8 =>
          Option(s"${mapPowerOfTenAndDigitToWord(2, d).getOrElse("")}")
        case p if p == 9 => Option(s"${singles(d).get} billion")
        case p if p == 10 =>
          Option(s"${mapPowerOfTenAndDigitToWord(1, d).getOrElse("")}")
        case p if p == 11 =>
          Option(s"${mapPowerOfTenAndDigitToWord(2, d).getOrElse("")}")
      }
  }

  private def below20(n: Int): Option[String] = {
    n match {
      case n if n == 0 => Option("zero")
      case n if n < 10 => singles(n)
      case n if n < 20 => teens(n)
    }
  }

  private def singles(n: Int): Option[String] = {
    Map(
      1 -> "one",
      2 -> "two",
      3 -> "three",
      4 -> "four",
      5 -> "five",
      6 -> "six",
      7 -> "seven",
      8 -> "eight",
      9 -> "nine"
    ).get(n)
  }

  private def teens(n: Int): Option[String] = {
    Map(
      10 -> "ten",
      11 -> "eleven",
      12 -> "twelve",
      13 -> "thirteen",
      14 -> "fourteen",
      15 -> "fifteen",
      16 -> "sixteen",
      17 -> "seventeen",
      18 -> "eighteen",
      19 -> "nineteen"
    ).get(n)
  }

  private val multiplesOfTenBelow100 = List(
    "twenty",
    "thirty",
    "forty",
    "fifty",
    "sixty",
    "seventy",
    "eighty",
    "ninety"
  )

  private def tens(n: Int): Option[String] = {
    Map(
      1 -> "ten",
      2 -> "twenty",
      3 -> "thirty",
      4 -> "forty",
      5 -> "fifty",
      6 -> "sixty",
      7 -> "seventy",
      8 -> "eighty",
      9 -> "ninety"
    ).get(n)
  }

}
