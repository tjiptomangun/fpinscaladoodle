package org.traditional
import java.util.concurrent.ForkJoinPool

object ExecutorCreate extends WrapperMain {
  val executor = new ForkJoinPool()
  executor.execute(new Runnable {
    override def run(): Unit = log("This task run asynchronously")
  })
  Thread.sleep(500)
}