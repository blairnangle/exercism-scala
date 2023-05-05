package proteintranslation

object ProteinTranslation {

  def proteins(sequence: String): Seq[String] = {
    sequence
      .grouped(3)
      .takeWhile(s => !List("UAA", "UAG", "UGA").contains(s))
      .map(s => mapSequenceToProtein(s))
      .toSeq
  }

  private def mapSequenceToProtein(sequence: String): String = {
    sequence match {
      case sequence if List("AUG").contains(sequence)        => "Methionine"
      case sequence if List("UUU", "UUC").contains(sequence) => "Phenylalanine"
      case sequence if List("UUA", "UUG").contains(sequence) => "Leucine"
      case sequence if List("UCU", "UCC", "UCA", "UCG").contains(sequence) =>
        "Serine"
      case sequence if List("UAU", "UAC").contains(sequence) => "Tyrosine"
      case sequence if List("UGU", "UGC").contains(sequence) => "Cysteine"
      case sequence if List("UGG").contains(sequence)        => "Tryptophan"
    }
  }
}
