package example
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class ParallelOnGlobalWithFutureSequence extends FuturesExample {
  override def runExample(): Future[Unit] = {
    println(s"Running example on thread ${Thread.currentThread().getName}")
    val slowTasks = Seq (
      slowIOBoundTask("A"),
      slowIOBoundTask("B"),
      slowIOBoundTask("C"),
      slowIOBoundTask("D"),
      slowIOBoundTask("E")
    )
    Future.sequence(slowTasks).map(_ => taskComplete())
  }

}
