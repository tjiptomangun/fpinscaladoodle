package org.main

import scala.collection.mutable

object SynchronizedPool extends  WrapperMain {
  private val tasks = mutable.Queue[() => Unit] ()

  val worker = new Thread {
    setDaemon(true)
    def poll(): () => Unit = tasks.synchronized {
      while(tasks.isEmpty) tasks.wait()
      tasks.dequeue()
    }
    override def run() = while(true) {
      val task = poll()
      task()
    }
  }
  worker.setName("Worker")

  // on setDaemon https://stackoverflow.com/a/19421083/8642222
  worker.start()

  def asynchronous(body: => Unit) = tasks.synchronized {
    tasks.enqueue {() => body}
    tasks.notify()
  }

  asynchronous(log("Hello"))
  asynchronous(log("World"))
  asynchronous(log("Dune"))
  asynchronous(log("Buttlerian Jihad"))
  Thread.sleep(5000)
}
