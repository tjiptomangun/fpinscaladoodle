//https://howtodoinjava.com/java/multi-threading/callable-future-example/

import java.util.concurrent._
val executor : ExecutorService = Executors.newFixedThreadPool(2);
val number :Int = 10;
val x:Future[Int] = executor.submit(new Callable[Int] {
	def call =  {
		println(s"called ${number}")
		if (number == 0 || number == 1) {
			println(number)
			1
		}
		else {
			println(number)
			var ret = 1;
			for (i <- 2 to number) {
				Thread.sleep(100);
				println(ret)
				ret*=i;
			}
			ret
		}
	}
})
x
x.isDone()
Thread.sleep(1000)
x.isDone()
val y = x.get
println(s"y = ${y}")

