package org.main

object SendAllApp extends  WrapperMain {

  object uiders {
    var uidCount = 0L
    def getUniqueId() = this.synchronized{
      val freshUid = uidCount + 1
      uidCount = freshUid
      freshUid
    }

    def printUniqueIds(n: Int): Unit = {
      val uids = for(i <- 0 until n) yield  getUniqueId()
      log(s"Generated uids: $uids")
    }
  }
  class Account(val name: String, var money: Int) {
    val uid = uiders.getUniqueId()
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
  def sendAll(accounts: Set[Account], target: Account) : Unit = {
    accounts.foreach((x:Account) => {
      send(x, target, 1)
    })
  }

  val a = new Account("Jack", 1000)
  val b = new Account("Jill", 2000)
  val c = new Account("John", 2000)
  val t = new Account("Jim", 2000)
  sendAll(List(a, b, c).toSet, t)

}
