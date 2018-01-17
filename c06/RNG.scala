trait RNG {
	def nextInt: (Int, RNG) 
	
}


object RNG {
	case class SimpleRNG(seed: Long) extends RNG{
		def nextInt: (Int, RNG) = {
			val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
			val nextRNG = SimpleRNG(newSeed)
			val n = (newSeed >>> 16).toInt
			(n, nextRNG)
		}
	}

	def nonNegativeInt(curr: RNG) : (Int, RNG) = {
		val (cand, next) = curr.nextInt	
		(if (cand < 0 ) - (cand + 1) else cand, next)
	}

	def double(curr: RNG): (Double, RNG) = {
		val (cand, next) = curr.nextInt
		val j = if (cand < 0) (cand + 1).toDouble else cand.toDouble
		(j/Int.MaxValue.toDouble, next)
	}

	def intDouble(curr: RNG): ((Int, Double), RNG) = {
		val (i, rP) = nonNegativeInt(curr)
		val (d, rN) = double(rP)
		((i, d), rN)
	}

	def doubleInt(curr: RNG): ((Double, Int), RNG) = {
		val ((i, d), r) = intDouble(curr)
		((d, i), r)
	}

	def double3(curr: RNG) : ((Double, Double, Double), RNG) = {
		val r1 = double(curr)
		val r2 = double(r1._2)
		val r3 = double(r2._2)
		((r1._1, r2._1, r3._1), r2._2)
	}

	def intsMe(count: Int)(curr: RNG): (List[Int], RNG) = {
		val l = 0 to count toList
		val i = curr.nextInt
		val rz = l.foldLeft((List(i._1), i._2)) {(b, a) =>
			val x = b._2
			val y = b._1
			val added = x.nextInt	
			(added._1 :: y, added._2)
		}
		rz
			
		
	}

	def ints(count: Int)(curr: RNG): (List[Int], RNG) = {
		val x0 = curr.nextInt
		if (count == 0)
			(List.empty, curr)
		else{
			val z = ints(count - 1)(x0._2)
			(x0._1 :: z._1, z._2)
		}
		
	}

	@annotation.tailrec
	def intsTailRec(count: Int)(curr: RNG): (List[Int], RNG) = {
		if (count == 0)
			List.empty, x
	}
}

val maxVal = 10

val rng00 = RNG.SimpleRNG(100L)
val (r001, rng001) = rng00.nextInt
val (r002, rng002) = RNG.nonNegativeInt(rng001)
val (r003, rng003) = RNG.nonNegativeInt(rng002)
val (r004, rng004) = RNG.double(rng003)
val (r005, rng005) = RNG.double(rng004)
val (r006, rng006) = RNG.intDouble(rng005)
val (r007, rng007) = RNG.intDouble(rng006)
val (r008, rng008) = RNG.doubleInt(rng007)
val (r009, rng009) = RNG.doubleInt(rng008)
val (r010, rng010) = RNG.double3(rng009)
val (r011, rng011) = RNG.double3(rng010)
val (r012, rng012) = RNG.intsMe(5)(rng011)
val (r013, rng013) = RNG.intsMe(5)(rng012)
val (r014, rng014) = RNG.intsMe(5)(rng013)
val (r015, rng015) = RNG.ints(5)(rng014)
val (r016, rng016) = RNG.ints(5)(rng015)

