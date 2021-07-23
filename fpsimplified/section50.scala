//One last debuggable: Using list instead of string
case class Debuggable [A](value: A, log: List[String]) {
	def map [B](f: A => B): Debuggable[B] = {
		val nextValue:B = f(this.value);
		Debuggable(nextValue, this.log);
	}

	def flatMap[B](f: A => Debuggable[B]): Debuggable[B] = {
		val nextValue = f(this.value);
		Debuggable(nextValue.value, this.log :::  nextValue.log);
	}
}

def f(a: Int) : Debuggable[Int] =  {
    val result = a * 2
    val message = s"f: a ($a) * 2 = $result"
    Debuggable(result, List(message))
}
def g(a: Int) : Debuggable[Int] = {
    val result = a * 3
    val message = s"f: a ($a) * 3 = $result"
    Debuggable(result, List(message))
}
def h(a: Int) : Debuggable[Int] = {
    val result = a * 4
    val message = s"f: a ($a) * 4 = $result"
    Debuggable(result, List(message))
}


val finalResult = for {
    fResult <- f(100)
    gResult <- g(fResult)
    hResult <- h(gResult)
} yield hResult

println(s"value:  ${finalResult.value}\n")
println(s"log:  \n${finalResult.log}")
