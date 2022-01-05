package org.main

object ThreadCreation extends App{
  class MyThread extends Thread {
    override def run(): Unit = {
      println("New Thread running")
    }
  }
  val ct: Thread = Thread.currentThread()
  val name = ct.getName
  println(s"I am the thread $name")
  val t = new MyThread
  t.start()
  t.join()
  println("New thread joined")

}
