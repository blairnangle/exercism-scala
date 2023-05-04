package armstrongnumbers

object ArmstrongNumbers {

  def isArmstrongNumber(n: Int): Boolean = {
    val nString = n.toString
    val nDigits = nString.length

    n == nString.map(c => math.pow(c.asDigit, nDigits)).sum
  }
}
