package org.main

object AnonymousThread extends App with Logging {
  def log(s: String) = println(s)

  val t = thread {
    Thread.sleep(1000)
    log("New Thread running")
    Thread.sleep(1000)
    log("Still running")
    Thread.sleep(1000)
    log("Completed")
  }
  t.join()
  log("New Thread joined")

}
