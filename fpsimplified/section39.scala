//Option Naturally Leads to for

def makeInt(s: String): Option[Int] = {
	try {
		Some(s.trim.toInt)
	}
	catch {
		case e: Exception => None
	}
}

val x = makeInt("1")

val y = makeInt("2")

val sum = x.getOrElse(0) + y.getOrElse(0)

x.flatMap((a) => y.map((b) => a+b))

for {
	a <- x
	b <- y
}yield(a * b)



for {
	a <- makeInt("4")
	b <- makeInt("6");
	c <- makeInt("dsdsa")
}yield(a * b * c)


makeInt("102").map(_ * 2)
makeInt("").map(_ * 2)
