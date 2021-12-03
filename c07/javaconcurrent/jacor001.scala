//https://www.baeldung.com/java-future
import java.util.concurrent._
val executor = Executors.newSingleThreadExecutor()

def calculate(input: Int) : Future[Int] = {
	executor.submit(() => {
		Thread.sleep(1000);
		input * input
	})
}
val x = calculate(100)
x.isDone()
Thread.sleep(1000)
x.isDone()
x.get()
