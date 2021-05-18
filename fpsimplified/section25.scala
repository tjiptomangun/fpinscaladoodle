//functions can have multiple parameter group
def sum (a: Int)(b: Int)(c: Int) = a + b + c;
sum(1)(2)(3)

def whilst(testCondition: => Boolean)(codeblock: => Unit): Unit = {
	while(testCondition) {
		codeblock;
	}
}

var y = 0;
whilst(y < 5) {
	println(y);
	y+=1
}

def ifBothTrue(test1: => Boolean)
			  (test2: => Boolean)
			  (codeblock: => Unit)(caseFalse: => Unit) : Unit = {
	if (test1 && test2) {
		codeblock
	}
	else {
		caseFalse
	}
}

val x= 10;
val y = 11
ifBothTrue(x == 10)(y == 11) {
	println("Discount!!!");
}{
	println("Try again")
}

ifBothTrue(x > 0)(y<11) {
	println("sale");
}{

	println("Try harder")
}

def printIntIfTrue(a: Int)(implicit b: Boolean) = if (b) println(a)

printIntIfTrue(42)(true)

// printIntIfTrue(52)
// error: could not find implicit value for parameter b: Boolean

// val boo = true;
// printIntIfTrue(52)
// error: could not find implicit value for parameter b: Boolean`

implicit val boo = true;
printIntIfTrue(52)
// 52

// implicit val coo =false
// printIntIfTrue(52)
// section25.scala:1: error: ambiguous implicit values:

def f2(a: Int = 1)(b: Int = 2) = { a + b }
// f2
// error: missing argument list for method f2
f2()() //3
f2(10)() //12
f2()(10) //11
// f2(10)
// error: missing argument list for method f2
def f3(a: Int = 1)(b: Int = a) = { a + b }
f3()() //2

// https://alvinalexander.com/scala/using-control-structure-beginning-scala-david-pollak/ 
import scala.language.reflectiveCalls
/**
 * From the book, Beginning Scala, by David Pollak.
 */
object Control  {
	def using[A <: {def close(): Unit}], B] (param: A)(f: A=>B): B = {
		try {
			f(param)
		}
		finally {
			param.close();
		}
	}
}
