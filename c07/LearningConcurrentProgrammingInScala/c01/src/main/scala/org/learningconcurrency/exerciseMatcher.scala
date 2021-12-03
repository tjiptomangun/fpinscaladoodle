package org.learningconcurrency

object exerciseMatcher extends App {
  def matcher(reqex: String): PartialFunction[String, List[String]] = {
    case x if reqex.r.findAllIn(x).toList.length > 0 => reqex.r.findAllIn(x).toList
  }

  val y = matcher("\\d+")
  val ret = y("A 1 Ddsds 3 swwewe ee2e2")
  println(ret)
  // val ret2 = y("A Ddsds  swwewe") will cause exception because partial function not defined if no match
  // println(ret2)
}

