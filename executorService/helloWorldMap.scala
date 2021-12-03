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
		val f = Future { 
			val result = 42
			println(s"Result is $result") 
			result
		} (ec)
		f.map(result => println(s"I was passed the result of first future! Result:$result"))(ec)
		Await.result(f, 10.seconds)
		ec.shutdown()
	}
}
