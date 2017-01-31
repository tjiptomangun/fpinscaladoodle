
val raptors = List("Golden Eagle", "Bald Eagle", "Prairie Falcon", "Peregrine Falcon", "Harpy Eagle", "Red Kite")



val kinds = raptors.groupBy {
	case bird if bird.contains("Eagle") => "eagle"
	case bird if bird.contains("Falcon") => "falcon"
	case _ => "unknown"
}

val birds = List("Golden Eagle", "Grayfalcon", "American Robin", "Mountain Bluebird", "Mountain-Hawk Eagle")

val groupByFirstLetter = birds.groupBy(_.charAt(0))

val cats = List("Tiger", "Lion", "Puma", "Leopard", "Jaguar", "Cheetah", "Bobcat")
val groupedByLength = cats.groupBy(_.length)

val words = List("one", "two", "one", "three", "four", "two", "one")
val counts = words.groupBy(w => w).mapValues(_.size)

val a = List(
	(1, 1, 3, 13), 
	(1, 1, 3, 43), 
	(1, 1, 3, 23), 
	(1, 1, 5, 32), 
	(1, 1, 5, 35), 
	(1, 2, 4, 17), 
	(1, 2, 4, 23), 
	(1, 2, 4, 17), 
	(1, 2, 7, 21), 
	(1, 2, 7, 22), 
	(1, 2, 7, 23), 
	(1, 2, 7, 24), 
	(2, 1, 3, 44), 
	(2, 1, 3, 45), 
	(2, 1, 3, 46), 
	(2, 1, 3, 47), 
	(2, 1, 5, 27),
	(2, 1, 5, 25),
	(2, 2, 4, 32), 
	(2, 2, 4, 33), 
	(2, 2, 4, 36), 
	(2, 2, 7, 52),
	(2, 2, 7, 62),
	(2, 2, 7, 72),
	(2, 2, 7, 82),
	(2, 2, 7, 92)
)
val b = a.groupBy {
	case t4 if t4._1 == 1 && t4._2 == 1 =>  "f11"
	case t4 if t4._1 == 1 && t4._2 == 2 =>  "f12"
	case t4 if t4._1 == 2 && t4._2 == 1 =>  "f21"
	case t4 if t4._1 == 2 && t4._2 == 2 =>  "f22"
	case _ => "unknown"
}

b.mapValues(_.size)

val c = a.groupBy {
	case t4 if t4._1 == 1 && t4._2 == 1 && t4._3 > 0 =>  ("f11", t4._3)
	case t4 if t4._1 == 1 && t4._2 == 2 && t4._3 > 0 =>  ("f12", t4._3)
	case t4 if t4._1 == 2 && t4._2 == 1 && t4._3 > 0 =>  ("f21", t4._3)
	case t4 if t4._1 == 2 && t4._2 == 2 && t4._3 > 0 =>  ("f22", t4._3)
	case _ => "unknown"
}

val d = c.toList


