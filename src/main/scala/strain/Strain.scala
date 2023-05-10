package strain

import scala.annotation.tailrec

object Strain {

  def keep[A](seq: Seq[A], predicate: (A) => Boolean): Seq[A] = {
    @tailrec
    def loop(s: Seq[A], kept: Seq[A], p: (A) => Boolean): Seq[A] = {
      s match {
        case s if s.isEmpty     => kept
        case s if s.length == 1 => if (p(s.head)) kept :+ s.head else kept
        case _ =>
          if (p(s.head)) loop(s.tail, kept :+ s.head, p)
          else loop(s.tail, kept, p)
      }
    }
    loop(seq, Seq(), predicate)
  }

  def discard[A](seq: Seq[A], predicate: (A) => Boolean): Seq[A] = {
    @tailrec
    def loop(s: Seq[A], kept: Seq[A], p: (A) => Boolean): Seq[A] = {
      s match {
        case s if s.isEmpty     => kept
        case s if s.length == 1 => if (!p(s.head)) kept :+ s.head else kept
        case _ =>
          if (!p(s.head)) loop(s.tail, kept :+ s.head, p)
          else loop(s.tail, kept, p)
      }
    }
    loop(seq, Seq(), predicate)
  }

}
