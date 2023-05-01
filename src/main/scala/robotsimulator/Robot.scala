package robotsimulator

import robotsimulator.Bearing.Bearing

object Bearing extends Enumeration {
  type Bearing = Value
  val North, East, South, West = Value
}

case class Robot(bearing: Bearing, coordinates: (Int, Int)) {

  def turnRight: Robot = {
    bearing match {
      case bearing if bearing == Bearing.North =>
        Robot(Bearing.East, coordinates)
      case bearing if bearing == Bearing.East =>
        Robot(Bearing.South, coordinates)
      case bearing if bearing == Bearing.South =>
        Robot(Bearing.West, coordinates)
      case bearing if bearing == Bearing.West =>
        Robot(Bearing.North, coordinates)
      case _ => Robot(bearing, coordinates)
    }
  }

  def turnLeft: Robot = {
    bearing match {
      case bearing if bearing == Bearing.North =>
        Robot(Bearing.West, coordinates)
      case bearing if bearing == Bearing.West =>
        Robot(Bearing.South, coordinates)
      case bearing if bearing == Bearing.South =>
        Robot(Bearing.East, coordinates)
      case bearing if bearing == Bearing.East =>
        Robot(Bearing.North, coordinates)
      case _ => Robot(bearing, coordinates)
    }
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
