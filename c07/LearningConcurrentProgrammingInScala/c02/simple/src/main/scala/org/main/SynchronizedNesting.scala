package org.main

import scala.collection._

object SynchronizedNesting extends  WrapperMain {
  private val transfers = mutable.ArrayBuffer[String]()
  def logTranfer(name: String, n: Int) = transfers.synchronized{
    transfers += s"transfer to account '$name ' = $n"
  }
  class Account(val name: String, var money: Int)
  def add(account: Account, n: Int) = account.synchronized {
    account.money += n
    if (n  > 10) logTranfer(account.name, n)
  }

  val jane = new Account("Jane", 100)
  val josh = new Account("Josh", 200)
  val t1 = thread{add(jane, 5)}
  val t2 = thread{add(josh, 50)}
  val t3 = thread{add(jane, 70)}
  t1.join(); t2.join(); t3.join()
  log(s"transefs ---- \n$transfers")

}
