package triangle

case class Triangle(s1: Double, s2: Double, s3: Double) {

  def equilateral: Boolean = {
    !hasAZeroSide(s1, s2, s3) && s1 == s2 && s2 == s3
  }

  def isosceles: Boolean = {
    !hasAZeroSide(s1, s2, s3) && isValidTriangle(
      s1,
      s2,
      s3
    ) && hasTwoEqualSides(s1, s2, s3)
  }

  def scalene: Boolean = {
    !hasAZeroSide(s1, s2, s3) && isValidTriangle(s1, s2, s3) && hasNoEqualSides(
      s1,
      s2,
      s3
    )
  }

  private def hasAZeroSide(a: Double, b: Double, c: Double): Boolean = {
    List(a, b, c).contains(0)
  }

  private def isValidTriangle(a: Double, b: Double, c: Double): Boolean = {
    2 * List(a, b, c).max < a + b + c
  }

  private def hasTwoEqualSides(a: Double, b: Double, c: Double): Boolean = {
    a == b || a == c || b == c
  }

  private def hasNoEqualSides(a: Double, b: Double, c: Double): Boolean = {
    a != b && a != c && b != c
  }

}
