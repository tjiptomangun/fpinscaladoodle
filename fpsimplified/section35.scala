//Pure Function Tell No Lies
def makeInt(s: String): Option[Int] = {
	try{
		Some(s.trim.toInt)
	}catch {
		case e: Exception => None
	}
}

val result = for {
	x <- makeInt("1")
	y <- makeInt("2")
	z <- makeInt("3")
}yield x + y + z



val rezult= for {
	x <- makeInt("1")
	y <- makeInt("hello")
	z <- makeInt("4")
}yield x + y + z
