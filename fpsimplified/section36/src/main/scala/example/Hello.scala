package example
import org.scalactic._
object Hello extends Greeting with App {
  println(greeting)
  def makeInt(s: String) : Int Or ErrorMessage = {
    try {
      Good(s.trim.toInt)
    }
    catch {
      case e: Exception => Bad(e.toString)
    }
  }
  val e = makeInt("avc")
  println("e is", e)
  val f = makeInt("10")
  println("f is", f)
  makeInt("100") match {
    case Good(i) => println("Answer : " + i)
    case Bad(msg) => println("Error: " + msg)
  }

  val result = for {
    a <- makeInt("10")
    b <- makeInt("11")
  } yield a + b
  println(result)
}

trait Greeting {

  lazy val greeting: String = "hello"
}
