package org.main

import scala.collection.mutable

object SynchronizedBadPool extends  WrapperMain {
  private val tasks = mutable.Queue[() => Unit] ()

  val worker = new Thread {
    def poll(): Option[() => Unit] = tasks.synchronized {
      if(tasks.nonEmpty) Some(tasks.dequeue()) else None
    }
    override def run() = while(true) {
      poll() match {
        case Some(task) => task()
        case None =>
      }
    }
  }
  worker.setName("Worker")

  // on setDaemon https://stackoverflow.com/a/19421083/8642222
  worker.setDaemon(true)
  worker.start()

  def asynchronouse(body: => Unit) = tasks.synchronized {
    tasks.enqueue {() => body}
  }

  asynchronouse(log("Hello"))
  asynchronouse(log("World"))
  Thread.sleep(5000)
}
