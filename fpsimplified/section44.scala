//Using a "wrapper" class in a for expression

class Wrapper[Int](value: Int) {
	def map(f: Int => Int) : Wrapper[Int] =  {
		new Wrapper(f(value));
	}
	def flatMap(f: Int => Wrapper[Int]) : Wrapper[Int] = {
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

