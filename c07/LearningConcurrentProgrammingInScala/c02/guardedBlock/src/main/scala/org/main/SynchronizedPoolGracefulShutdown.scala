package org.main

import scala.annotation.tailrec
import scala.collection.mutable

//using daemon is good for handling shutdown.
//but  despite using 0 cpu because of `wait` `notify` method
//it still use memory space until application
//exits.
//We can use Thread.interrupt to force release
//thread release. But there is a better way.
object SynchronizedPoolGracefulShutdown extends  WrapperMain {
  private val tasks = mutable.Queue[() => Unit] ()
  object WorkerThread extends  Thread {
    var terminated = false
    def poll(): Option[() => Unit] = tasks.synchronized{
      while(tasks.isEmpty && !terminated) tasks.wait()
      if (!terminated) {
        Some(tasks.dequeue())
      }
      else {
        None
      }
    }
    @tailrec
    override def run() = poll() match {
      case Some(task) =>
        task()
        run()
      case None =>
    }

    def shutdown() = tasks.synchronized{
      terminated = true
      tasks.notify()
    }
  }

  def asynchronous(body: => Unit) = tasks.synchronized {
    tasks.enqueue {() => body}
    tasks.notify()
  }
  WorkerThread.start()

  asynchronous(log("Hello"))
  asynchronous(log("World"))
  asynchronous(log("Dune"))
  asynchronous(log("Buttlerian Jihad"))
  Thread.sleep(5000)
  WorkerThread.shutdown()
}
