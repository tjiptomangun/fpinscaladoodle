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

import scala.concurrent.ExecutionContext.Implicits.global

object Main {
	val singleThreadPool: ExecutorService = Executors.newSingleThreadExecutor();
	def main(args: Array[String]): Unit = {
		val f = Future { println("I'm in a future") }
		Await.result(f, 10.seconds)
		System.exit(0)
	}
}

/*
The caveat here is that the global context is defined to be a thread pool with N threads where N is equal to the number of available processor on the machine. So be aware that code exec...
 */
