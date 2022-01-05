package org.main

object ThreadsSynchronizedUidDuo extends WrapperMain {
  var uidCount = 0L

  def getUniqueId() = this.synchronized{
    val freshUid = uidCount + 1
    uidCount = freshUid
    freshUid
  }

  def getUniqueIdToo() = this.synchronized{
    val freshUid = uidCount + 1
    uidCount = freshUid
    freshUid
  }

  def printUniqueIds(n: Int): Unit = {
    val uids = for(i <- 0 until n) yield  getUniqueId()
    log(s"Generated uids: $uids")
  }

  def printUniqueIdsToo(n: Int): Unit = {
    val uids = for(i <- 0 until n) yield  getUniqueId()
    log(s"Generated uidsToo: $uids")
  }

  val t = thread{
    printUniqueIds(5)
    printUniqueIdsToo(5)
  }
  printUniqueIds(5)
  t.join()



}
