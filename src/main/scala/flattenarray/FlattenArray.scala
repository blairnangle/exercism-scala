package flattenarray

object FlattenArray {

  def flatten(list: List[_]): List[_] = list flatMap {
    case l: List[_] => flatten(l.filter(e => e != null))
    case otherwise  => if (otherwise != null) List(otherwise) else List()
  }
}
