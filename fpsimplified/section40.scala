//Recap: Option -> flatMap -> for
//scalac -Xprint:parse section40.scala
class Test40 {
	def makeInt(s: String): Option[Int] = {
		try {
			Some(s.trim.toInt)
		}
		catch {
			case e: Exception => None
		}
	}

	val sum = for {
		a <- makeInt("1")
		b <- makeInt("2");
		c <- makeInt("3")
		d <- makeInt("4")
	} yield ((a + b)*c)^d
}
