//https://medium.com/@jstnlef/a-journey-into-concurrent-programming-in-scala-c1c3e7df0c4f

import scala.concurrent.Future

/**object Main {
	def main(args: Array[String]): Unit = {
		Future { println("I'm in a future") }
	}
}
error: Cannot find an implicit ExecutionContext
**/ 

import java.util.concurrent.{ExecutorService, Executors}

import scala.concurrent.{ExecutionContext, Await}

import scala.concurrent.duration._

object Main {
	val singleThreadPool: ExecutorService = Executors.newSingleThreadExecutor();
	def main(args: Array[String]): Unit = {
		val ec = ExecutionContext.fromExecutorService(singleThreadPool)
		val f = Future { println("I'm in a future") }(ec)
		Await.result(f, 10.seconds)
		//System.exit(0)
		ec.shutdown()
	}
}
