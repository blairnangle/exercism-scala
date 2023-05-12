package robotsimulator

object Bearing extends Enumeration {
  val North: Bearing.Value = Value(0)
  val East: Bearing.Value = Value(1)
  val South: Bearing.Value = Value(2)
  val West: Bearing.Value = Value(3)
}

case class Robot(bearing: Bearing.Value, coordinates: (Int, Int)) {

  def turnRight: Robot = {
    Robot(Bearing((bearing.id + 1) % 4), coordinates)
  }

  def turnLeft: Robot = {
    Robot(Bearing((4 + (bearing.id - 1)) % 4), coordinates)
  }

  def advance: Robot = {
    bearing match {
      case bearing if bearing == Bearing.North =>
        Robot(bearing, (coordinates._1, coordinates._2 + 1))
      case bearing if bearing == Bearing.East =>
        Robot(bearing, (coordinates._1 + 1, coordinates._2))
      case bearing if bearing == Bearing.South =>
        Robot(bearing, (coordinates._1, coordinates._2 - 1))
      case bearing if bearing == Bearing.West =>
        Robot(bearing, (coordinates._1 - 1, coordinates._2))
      case _ => Robot(bearing, coordinates)
    }
  }

  private def mapInstructionToMethod(i: Char): Robot = {
    i match {
      case i if i == 'R' => this.turnRight
      case i if i == 'L' => this.turnLeft
      case i if i == 'A' => this.advance
    }
  }

  def simulate(instructions: String): Robot = {
    if (instructions.length > 1) {
      mapInstructionToMethod(instructions.charAt(0))
        .simulate(instructions.drop(1))
    } else {
      mapInstructionToMethod(instructions.charAt(0))
    }
  }
}
