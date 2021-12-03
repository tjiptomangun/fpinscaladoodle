package example

import scala.concurrent.{ExecutionContext, Future}

trait FuturesExample {
  def runExample() : Future[Unit]

  def slowIOBoundTask(name: String)(implicit ec: ExecutionContext): Future[Unit] = Future {
    val thread = Thread.currentThread()
    println(s"Starting io task $name Thread ${thread.getName}")
    Thread.sleep(1000)
    println(s"ending")
  }

  def taskComplete(): Unit = {
    val thread = Thread.currentThread()
    println(s"All tasks complete on Thread ${thread.getName}")
  }

}
