//How Debuggable, f, g, h work
//scala -Xprint:parse section48.scalac.scala
object section48{
	case class Debuggable (value: Int, message: String) {
		def map (f: Int => Int): Debuggable = {
			println("\n>>> entered map >>>")
			println(s"map: value: ${value}")
			println(s"map: msg: ${message}")
			val nextValue = f(this.value);
			println(s"map: nextValue: ${nextValue}")
			println("<<< leaving map <<<\n")
			Debuggable(nextValue, this.message);
		}
	
		def flatMap[B](f: Int => Debuggable): Debuggable = {
			println("\n>>> entered fmap >>>")
			println(s"fmap: value: ${value}")
			println(s"fmap: msg: ${message}")
			val nextValue = f(this.value);
			println(s"fmap: msg: ${message}")
			println(s"fmap: next val: ${nextValue.value}")
			println(s"fmap: next msg: ${nextValue.message}")
			println("<<< leaving fmap <<<\n")
			Debuggable(nextValue.value, this.message + "\n"+ nextValue.message);
		}
	}
	
	def f(a: Int) : Debuggable =  {
		println(s"\n[f: a = $a]")
		val result = a * 2
		val message = s"f: input $a, result: $result"
		Debuggable(result, message)
	}
	def g(a: Int) : Debuggable = {
		println(s"\n[g: a = $a]")
		val result = a * 3
		val message = s"g: input $a, result: $result"
		Debuggable(result, message)
	}
	def h(a: Int) : Debuggable = {
		println(s"\n[h: a = $a]")
		val result = a * 4
		val message = s"h: input $a, result: $result"
		Debuggable(result, message)
	}
	
	
	val finalResult = for {
		fResult <- f(100)
		gResult <- g(fResult)
		hResult <- h(gResult)	
	} yield hResult
	
	println(s"value:  ${finalResult.value}\n")
	println(s"message:  \n${finalResult.message}")
}
