//changing "new Wrapper" to "Wrapper"
//use :paste section46.scala instead of :load section46.scala

class Wrapper[A] private (value: A) {
	def map[B](f: A => B) : Wrapper[B] =  {
		new Wrapper(f(value));
	}
	def flatMap[B](f: A=> Wrapper[B]) : Wrapper[B] = {
		f(value);
	}
	override def toString = value.toString
}
object Wrapper {
	def apply [A](x: A): Wrapper[A] =  {
		new Wrapper(x);
	}
}

//import Wrapper._
val result: Wrapper[Int] = for {
	a <- Wrapper(1)
	b <- Wrapper(2)
	c <- Wrapper(3)
}	yield a + b + c
val x = Wrapper(1)
x.map(_ * 2)
x.map(_ * 2)

val res: Wrapper[String] = for {
	a <- Wrapper("1")
	b <- Wrapper(2)
	c <- Wrapper("3")
}	yield a + b + c

