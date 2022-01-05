package org.main

object SimpleApp extends App {
  val t: Thread = Thread.currentThread()
  val name = t.getName
  println(s"I am the thread $name")

}
