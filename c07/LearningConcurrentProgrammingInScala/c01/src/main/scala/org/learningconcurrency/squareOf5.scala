package org.learningconcurrency

trait Logging {
	def log(s: String): Unit
	def warn(s: String)= log("WARN: " + s)
	def error(s: String)= log("ERROR: " + s)
}

class PrintLogging extends Logging{
	def log(s: String) = println(s)
}

class Position(val x: Int, val y: Int) {
	def +(that: Position) = new Position(x + that.x, y + that.y)
	override def toString =  s"Position:($x, $y)"
}

object SquareOf5 extends App {
	def square(x: Int): Int = x * x
	val s = square(5)
	println(s"Result: $s")
	val pl = new PrintLogging()
	pl.warn("Approaching")
	pl.error("Crash")
	for (i <- 0 until 10) println(i)
	val untilYield = for (i <- 0 until 10) yield -i
	println(untilYield)
	val successor = Map(1 -> 2, 2 -> 3, 3 -> 4)
	successor.get(5) match {
		case Some(n) => println(s"Successor is: $n")
		case None => println("Could not find succcessor")
	}
	val p1 = new Position(3, 4)
	val p2 = new Position(-5, 11)
	val p3 = p1 + p2
	println(p3.toString)

}
