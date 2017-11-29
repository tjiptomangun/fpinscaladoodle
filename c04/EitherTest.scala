import scala.io.StdIn._
val in = readLine ("Type Either a string or an Int:")
val result: Either[String, Int] = {
	try Right(in.toInt)
	catch{
		case e:NumberFormatException => Left(in)
	}
}

result match {
	case Right(x) => s"You passed me the Int: $x, which I will increment. $x + 1 = ${x+1}"
	case Left(x) => s"You passed me the String: $x"
}

def doubled(i: Int) = i * 2
Right(42).map(doubled)
Left(42).map(doubled)