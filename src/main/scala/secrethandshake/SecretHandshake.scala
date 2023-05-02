package secrethandshake

import scala.collection.mutable

object SecretHandshake {

  def commands(input: Int): mutable.ArrayBuffer[String] = {
    val reversed = input.toBinaryString.reverse
    var actions = mutable.ArrayBuffer[String]()
    for ((x, i) <- reversed.view.zipWithIndex) {
      if (x == '1') {
        if (i == 4) {
          actions = actions.reverse
        } else {
          actions += mapBinaryToAction(i)
        }
      }
    }
    actions
  }

  private def mapBinaryToAction(idx: Int): String = {
    idx match {
      case idx if idx == 0 => "wink"
      case idx if idx == 1 => "double blink"
      case idx if idx == 2 => "close your eyes"
      case idx if idx == 3 => "jump"
    }
  }
}
