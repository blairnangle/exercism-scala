package rnatranscription

object RnaTranscription {

  def toRna(nucleotide: String): Option[String] = {
    nucleotide
      .flatMap(c => mapNucleotideToComplement(c))
      .foldLeft(Option(""))((acc, c) => acc.map(_ + c))
  }

  private def mapNucleotideToComplement(n: Char): Option[String] = {
    Map('C' -> "G", 'G' -> "C", 'T' -> "A", 'A' -> "U").get(n)
  }

}
