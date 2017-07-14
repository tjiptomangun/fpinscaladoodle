import scala.util.{Either => _,  _}
sealed trait Either[+E, +A] {
	def map[B] (f: A => B): Either[E, B] = {
		this match {
			case Right(x) =>
				Right(f(x))
			case Left(y) => 
				this.asInstanceOf[Either[E, Nothing]]
					
		}
	}

	def flatMap[EE >: E, B] (f: A => Either[EE, B]): Either[EE, B] = {
		this match {
			case Right(x) =>
				f(x)
			case Left(y) => 
				Left(y)
				//this:Either[EE, Nothing]
				//this.asInstanceOf[Either[EE, Nothing]]
				
		}
	}

	def orElse[EE>: E, B>: A] (b: => Either[EE, B]): Either[EE, B] = {
		this match {
			case Right(x) =>
				this.asInstanceOf[Either[EE, B]]
			case Left(x) =>
				b
		}
	}

	def map2[EE >: E, B, C] (b: Either[EE, B]) (f: (A, B) => C):Either[EE, C] = {
		flatMap{ta => b map{tb => f(ta, tb)}}
	}
}
case class Right[+A](value: A) extends Either[Nothing, A]
case class Left[+E](value: E) extends Either[E, Nothing]

object Either{
}

def insuranceRateQuote(age:Int, numberOfSpeedingTicket: Int): Double = {
        age * 0.01 + numberOfSpeedingTicket * 0.015
}

def Try[A](a: => A): Either[Exception, A] = {
        try Right(a)
        catch {case e: Exception => Left(e)}
}

def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Either[Exception, Double] = {
	val a = Try(age.toInt)
	val b = Try(numberOfSpeedingTickets.toInt)
	a.map2(b){(ta, tb) => insuranceRateQuote(ta, tb)}
}

def parseInsuranceRateQuote_for(age: String, numberOfSpeedingTickets: String): Either[Exception, Double] = {
	for {
		a <- Try(age.toInt)
		b <- Try(numberOfSpeedingTickets.toInt)
		
	} yield insuranceRateQuote(a, b)
}



val b = Left("Error")
val a = Right(30)

val aa = a map (_ * 4)
val bb = b map {x:Int =>x * 3}

val aaa = a map (x => x)
val bbb = b map (x => x)

val aaaa = a flatMap(y => Right(y + 2))
val bbbb = b flatMap{y:Int => Right(y + 2)}

val aaaaa = a orElse(Right(0))
val bbbbb = b orElse(Right(0))

val aaaaaa = a.map2{a}{(ta:Int, tb:Int) => ta + 5}
val bbbbbb = a.map2{b}{(ta:Int, tb:Int) => ta + 5}
val cccccc = b.map2(a){(ta:Int, tb:Int) => ta + 5}
val dddddd = b.map2(b){(ta:Int, tb:Int) => ta + 5}

val e     = parseInsuranceRateQuote("100", "100")
val ee    = parseInsuranceRateQuote("1x0", "100")
val eee   = parseInsuranceRateQuote("100", "10x")
val eeee  = parseInsuranceRateQuote("1x0", "10x")

val f     = parseInsuranceRateQuote_for("100", "100")
val ff    = parseInsuranceRateQuote_for("1x0", "100")
val fff   = parseInsuranceRateQuote_for("100", "10x")
val ffff  = parseInsuranceRateQuote_for("1x0", "10x")

