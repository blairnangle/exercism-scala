package perfectnumbers

import perfectnumbers.NumberType.NumberType

object NumberType extends Enumeration {
  type NumberType = Value
  val Abundant, Deficient, Perfect = Value
}

object PerfectNumbers {

  def classify(n: Int): Either[String, NumberType] = {
    if (n <= 0) {
      Left("Classification is only possible for natural numbers.")
    } else {
      val aliquotSum = calculateAliquotSum(n)
      (n, aliquotSum) match {
        case (n, aliquotSum) if n == aliquotSum => Right(NumberType.Perfect)
        case (n, aliquotSum) if n < aliquotSum  => Right(NumberType.Abundant)
        case (n, aliquotSum) if n > aliquotSum  => Right(NumberType.Deficient)
      }
    }
  }

  private def calculateAliquotSum(n: Int): Int = {
    Range.inclusive(1, n / 2).filter(f => n % f == 0).sum
  }

}
