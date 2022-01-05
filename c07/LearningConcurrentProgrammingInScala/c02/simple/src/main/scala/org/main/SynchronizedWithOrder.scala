package org.main

object SynchronizedWithOrder extends WrapperMain {
  class Account(val name: String, var money: Int) {
    import ThreadsUnprotectedUid.getUniqueId
    val uid = getUniqueId()
  }
  def send(a: Account, b: Account, n: Int) = {
    val thName = Thread.currentThread().getName()
    def adjust (): Unit = {
      a.money -= n
      b.money += n
    }
    def logWait (acc: Account): Unit = {
      log(s"$thName wait ${acc.name} lock")
    }
    def logRelease(acc: Account): Unit = {
      log(s"$thName release ${acc.name} lock")
    }
    if (a.uid < b.uid) {
      logWait(a)
      a.synchronized{
        logWait(b)
        b.synchronized{
          adjust()
          logRelease(b)
        }
        logRelease(a)
      }
    }
    else {
      logWait(b)
      b.synchronized{
        logWait(a)
        a.synchronized{
          adjust()
          logRelease(a)
        }
        logRelease(b)
      }
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
