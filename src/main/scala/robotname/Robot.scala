package robotname

import scala.util.Random

class Robot() {
  var name: String = generateName()

  private def generateName(): String = {

    s"${Random.alphanumeric.filter(_.isUpper).take(2).mkString}${Random.alphanumeric.filter(_.isDigit).take(3).mkString}"
  }

  def reset(): Unit = {
    this.name = generateName()
  }
}
