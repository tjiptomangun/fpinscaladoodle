package org.main

object ThreadSharedStateAccessReordering extends WrapperMain {
  for (i  <- 0 until 100000) {
    var a = false
    var b = false
    var x = -1
    var y = -1
    val t1 = thread {
      //if  a is true then x should be 0
      a = true
      y = if (b) 0 else 1
    }
    val t2 = thread{
      //if  b is true then y should be 0
      b = true
      x = if (a) 0 else 1
    }
    t1.join()
    t2.join()
    assert(!(x == 1 && y == 1), s"x = $x, y = $y, i = $i")
  }

}
