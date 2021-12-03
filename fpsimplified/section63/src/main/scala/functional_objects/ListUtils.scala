package functional_objects

object ListUtils {

  def dropFirstMatch[A](ls: Seq[A], value: A): Seq[A] = {
    val index = ls.indexOf(value)
    if (index < 0) {
      ls
    }
    else if (index == 0) {
      ls.tail
    }
    else {
      val (a, b) = ls.splitAt(index)
      a ++ b.tail
    }
  }

}
