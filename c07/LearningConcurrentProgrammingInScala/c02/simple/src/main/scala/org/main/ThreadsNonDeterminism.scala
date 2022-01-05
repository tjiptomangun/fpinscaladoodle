package org.main

object ThreadsNonDeterminism extends App with Logging {
  def log(s: String) = println(s)
  val t = thread{
    log("New thread running.")
  }
  log("...")
  log("...")
  t.join()
  log("New thread joined")
}
