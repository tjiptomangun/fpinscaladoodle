package org.main

import org.main.SynchronizedNesting.Account

object SynchronizedDeadlock extends WrapperMain {
  def send(a: Account, b: Account, n: Int) = {
    val thName = Thread.currentThread().getName()
    log(s"$thName wait ${a.name} lock")
    a.synchronized{
      log(s"$thName wait ${b.name} lock")
      b.synchronized{
        a.money -= n
        b.money += n
        log(s"$thName release ${b.name} lock")
      }
      log(s"$thName release ${a.name} lock")
    }
  }


  val a = new Account("Jack", 1000)
  val b = new Account("Jill", 2000)
  val t1 = thread {
    for (i <- 0 until 100) send(a, b, 1)
  }
  val t2 = thread {
    for (i <- 0 until 100) send(b, a, 1)
  }
  t1.join(); t2.join()
  log(s"a = ${a.money} b=${b.money}")

}
