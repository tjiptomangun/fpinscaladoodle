//using bind in a for expression
case class Debuggable (value: Int, message: String) {
	def map (f: Int => Int): Debuggable = {
		val nextValue = f(this.value);
		Debuggable(nextValue, this.message);
	}

	def flatMap[B](f: Int => Debuggable): Debuggable = {
		val nextValue = f(this.value);
		Debuggable(nextValue.value, this.message + "\n"+ nextValue.message);
	}
}

def f(a: Int) : Debuggable =  {
	val result = a * 2
	val message = s"f: a ($a) * 2 = $result"
	Debuggable(result, message)
}
def g(a: Int) : Debuggable = {
	val result = a * 3
	val message = s"f: a ($a) * 3 = $result"
	Debuggable(result, message)
}
def h(a: Int) : Debuggable = {
	val result = a * 4
	val message = s"f: a ($a) * 4 = $result"
	Debuggable(result, message)
}


val finalResult = for {
	fResult <- f(100)
	gResult <- g(fResult)
	hResult <- h(gResult)	
} yield hResult

println(s"value:  ${finalResult.value}\n")
println(s"message:  \n${finalResult.message}")
