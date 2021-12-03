package example

import java.util.concurrent.Executors

import scala.concurrent.{ExecutionContext, Future}

class ConcurrentSingleThread extends FuturesExample {

  val singleThreadPool = Executors.newFixedThreadPool(1)

  implicit val ec = ExecutionContext.fromExecutorService(singleThreadPool)

  /**
    * This method will execute on the main thread , post 5 slowIOBoundTasks to the
    * singleThreadPool and print the taskComplete message when all of the futures
    * have completed
    */

  override def runExample(): Future[Unit] = {
    println(s"Running example on Thread ${Thread.currentThread().getName}")

    val slowTasks = Seq (
      slowIOBoundTask("A"), slowIOBoundTask("B"),
      slowIOBoundTask("C"), slowIOBoundTask("D"),
      slowIOBoundTask("E")
    )
    Future.sequence(slowTasks).map(_ => taskComplete())
  }
}
