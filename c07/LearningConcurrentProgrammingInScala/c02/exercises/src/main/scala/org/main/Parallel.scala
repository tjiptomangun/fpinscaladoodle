package org.main

object Parallel extends  WrapperMain {
  def parallel[A, B] (a: => A, b: => B): (A, B) = {
    var ares :A = null.asInstanceOf[A];
    var bres :B = null.asInstanceOf[B];
    val p1 = thread{
      ares = a
    }
    val p2 = thread{
      bres = b
    }
    p1.join()
    p2.join()
    (ares, bres)
  }
  val(x, y) = parallel(10, 11)
  println(s"x : $x")
  println(s"y : $y")

}
