package highscores

object HighScores {

  def latest(scores: List[Int]): Int = {
    scores.takeRight(1).head
  }

  def personalBest(scores: List[Int]): Int = {
    scores.max
  }

  def personalTop(scores: List[Int]): List[Int] = {
    scores.sortWith((s1, s2) => s1 > s2).take(3)
  }

  def report(scores: List[Int]): String = {
    val l = latest(scores)
    val pb = personalBest(scores)
    val personalBestMessage =
      if (l >= pb) "That's your personal best!"
      else s"That's ${pb - l} short of your personal best!"

    s"Your latest score was ${l}. ${personalBestMessage}"
  }

}
