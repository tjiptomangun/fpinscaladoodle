package org.learningconcurrency

object exerciseCombination extends App {

//  def combination[T](req: Int, xs: Seq[T]) : Iterator[Seq[T]] = {
//
//  }
  def innerComb[T](requiredDepth: Int, currDepth: Int, list: List[T], accum: (List[List[T]], List[T])) : List[List[T]] = {
    if (currDepth == requiredDepth) {
      if (accum._2.length > 0) {
        accum._1 :+ accum._2
      }
      else {
        accum._1
      }
    }
    else {
      val zipped = list.zipWithIndex

      val split = zipped.map((x) => {
        val rems = (list splitAt (x._2))._2
        (rems.head, rems.tail)
      })
      split.flatMap((x) => {
        if ((x._2.length  + 1)< (requiredDepth - currDepth)) {
          List()
        }
        else {
          val newAcc = x._1 +: accum._2
          innerComb(requiredDepth, currDepth + 1, x._2, (accum._1, newAcc))
        }
      })
    }
  }

  def combinationX[T](required: Int, list: List[T]) : List[List[T]] = {
    val unique = list.toSet.toList
    innerComb(required, 0, unique, (List.empty:List[List[T]], List.empty:List[T]))
  }

  def combination[T](required: Int, list: List[T]) : Iterator[Seq[T]] = {
    combinationX(required, list).map((x) => x.toIndexedSeq).iterator
  }

  val it = combination(3, List("A", "B", "C", "D", "E", "F"))
  while(it.hasNext)
    println(it.next())
}
