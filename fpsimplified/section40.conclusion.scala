import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}


val xs = Seq(1, 2, 3)
val ys = Seq(7, 11, 13)
for {
	x <- xs
	y <- ys
} yield  x + y

val f1 = Future {Thread.sleep(1*1000); 1}
val f2 = Future {Thread.sleep(2*1000); 2}
val f3 = Future {Thread.sleep(3*1000); 3}

val result =  for {
	r1 <- f1
	r2 <- f2
	r3 <- f3
}yield(r1 + r2 + r3)

result.onComplete {
	case Success(x) => println(s"$x")
	case Failure(e) => e.printStackTrace
}
