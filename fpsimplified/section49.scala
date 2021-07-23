//using bind in a for expression
case class Debuggable [A](value: A, message: String) {
	def map [B](f: A => B): Debuggable[B] = {
		val nextValue:B = f(this.value);
		Debuggable(nextValue, this.message);
	}

	def flatMap[B](f: A => Debuggable[B]): Debuggable[B] = {
		val nextValue = f(this.value);
		Debuggable(nextValue.value, this.message + "\n"+ nextValue.message);
	}
}

def f(a: Int) : Debuggable[Int] =  {
    val result = a * 2
    val message = s"f: a ($a) * 2 = $result"
    Debuggable(result, message)
}
def g(a: Int) : Debuggable[Int] = {
    val result = a * 3
    val message = s"f: a ($a) * 3 = $result"
    Debuggable(result, message)
}
def h(a: Int) : Debuggable[Int] = {
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
