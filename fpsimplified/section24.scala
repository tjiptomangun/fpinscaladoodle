//How to use by-name parameters
//by name syntax
//def fn(blockOfCode: => returnType)
//:load section24.scala

def fnSketch[A](blockOfCode: =>A) = ???

def timer[A](blockOfCode: => A) = {
	val startTime = System.nanoTime	
	val result = blockOfCode
	val stopTime = System.nanoTime	
	val delta = stopTime - startTime
	(result,   delta/1000000d)
}

val (result, time) = timer(println("Hello"));

val (r, t) = timer (((i:Int) => i+1)(10))
val assertionEnabled = true

def byValueAssertion(p: () => Boolean) = {
	if (assertionEnabled && !p()){
		println("byValueAssertion hit!!!")
		throw new AssertionError
	}
}

byValueAssertion(() => 3>5);

def byNameAssertion(p:  => Boolean) = {
	if (assertionEnabled && !p){
		throw new AssertionError
	}
}

byNameAssertion( 3>5);
//https://stackoverflow.com/questions/4543228/whats-the-difference-between-and-unit
def f (x: => Int) = x * x
val y = 10;
f (y+2);
//res20: Int = 144
val x = 12;
f (x /2)

//case class Scheduled(val time: Int, val callback: => Unit)
//error: `val` parameters may not be call-by-name
