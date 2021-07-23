//Making wrapper more generic

class Wrapper[A](value: A) {
	def map[B](f: A => B) : Wrapper[B] =  {
		new Wrapper(f(value));
	}
	def flatMap[B](f: A=> Wrapper[B]) : Wrapper[B] = {
		f(value);
	}
	override def toString = value.toString
}
val result: Wrapper[Int] = for {
	a <- new Wrapper(1)
	b <- new Wrapper(2)
	c <- new Wrapper(3)
}	yield a + b + c
val x = new Wrapper(1)
x.map(_ * 2)
x.map(_ * 2)

val res: Wrapper[String] = for {
	a <- new Wrapper("1")
	b <- new Wrapper(2)
	c <- new Wrapper("3")
}	yield a + b + c

