package utils

object ListUtils {
  def dropFirstMatch[A](ls: Seq[A], value: A): Seq[A] = {
    val index = ls.indexOf(value)
    if (index < 0){
      ls
    }
    else if (index == 0){
      ls.tail
    }
    else {
      //splitAt keeps the matching element in second group
      val (a, b) = ls.splitAt(index)
      a ++ b.tail
    }
  }

  private def dropAllButFirst[A](list: List[A], dropee : A, foundCount: Int): List[A] = list match {
    case Nil => Nil
    case x :: xs if x == dropee => {
      if (foundCount == 0) {
        x::dropAllButFirst(xs, dropee, foundCount + 1)
      }
      else {
        dropAllButFirst(xs, dropee, foundCount + 1)
      }
    }
    case x :: xs => {
      x::dropAllButFirst(xs, dropee, foundCount)
    }

  }

  def dropAllButFirst[A](list: List[A], dropee: A) : List[A] = {
    dropAllButFirst(list, dropee, 0)
  }
}
