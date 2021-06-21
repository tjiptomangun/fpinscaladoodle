//Functional Error Handling
import scala.util.{Try, Success, Failure}
def makeInt(s: String): Try[Int] = Try(s.trim.toInt);

val result = makeInt("1")
val rezult = makeInt("hi")

val rexult = for {
	x <- makeInt("3")
	y <- makeInt("4")
	z <- makeInt("5")
} yield x * y * z

val reyult = for {
	x <- makeInt("3")
	y <- makeInt("hi")
	z <- makeInt("babe")
} yield x * y * z

def createInt(s:String): Either[String, Int] = {
	try {
		Right(s.trim.toInt)
	}catch {
		case e: Exception => Left(e.toString);
	}
}
createInt("10");
createInt("Woi");

val answer = for {
	a <- makeInt("10")
	b <- makeInt("11")
} yield a * b

import org.scalactic._
def convToInt(s: String): Int Or ErrorMessage = {
	try {
		Good(s.trim.toInt)
	}
	catch {
		case e: Exception => Bad(e.toString);
	}
}

