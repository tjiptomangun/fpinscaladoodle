package org.main

object Periodic extends  WrapperMain {
  def periodically(duration: Long) (b: => Unit): Unit = {
    while(true) {

      val x = thread{
        b
        Thread.sleep(duration)
      }
      x.join()
    }
  }
  def toRun = {
    val a = 10;
    val b = 12;
    log(s"${a + b}")
  }

  periodically(1000)(toRun)

}
