package org.learningconcurrency
object ExersicePermutations extends App {
  def inPermute [T](accum: (List[List[T]],  List[T]), list:List[T]): List[List[T]]  = {
    list match {
      case h :: t => {
        list.zipWithIndex.flatMap ((x) => {
          val (l1, l2) = list splitAt (x._2)
          val toPush = l2.head
          val nl = l1 ++ l2.tail
          inPermute((accum._1, accum._2 :+ toPush), nl)
        })
      }
      case _ =>
        accum._2 match {
          case h::t =>
                accum._1 :+ accum._2
          case _ =>
            accum._1
        }
    }
  }

  def permute[T](input : List[T]) : List[List[T]] = {
    inPermute((List.empty:List[List[T]], List.empty:List[T]), input).toSet.toList
  }

  def stringPermutation(input: String): Seq[String] = {
    val toPerm = input.toList
    permute(toPerm).map((x) =>  x.mkString )
  }

  print(stringPermutation("ABBA"))
}