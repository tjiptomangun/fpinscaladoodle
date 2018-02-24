trait RNG {
	def nextInt: (Int, RNG) 
}


object RNG {
	/**
	 * implementation of RNG trait
	 **/
	case class SimpleRNG(seed: Long) extends RNG{
		/**
		 * returns tuple2 of integer and new instance of this class
		 * with a new seed
		 **/
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
		val (i, rP) = curr.nextInt
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

	def intsTailRec(count: Int)(curr: RNG): (List[Int], RNG) = {
		@annotation.tailrec
		def inner (a: Int, c: RNG, l: List[Int]) : (List[Int], RNG) = {
			if (a == 0)
				(l, c)
			else{
				val x0 = c.nextInt
				inner(a-1, x0._2, x0._1 :: l)
			}
				
		}
		inner(count, curr, List.empty)
	}
	type Rand[+A] = RNG => (A, RNG)

	val int: Rand[Int] = _.nextInt

	def unit[A](a: A): Rand[A] = 
		rng => (a, rng)

	//try to implement this, see nonNegative above
	def nonNegative2: Rand[Int] = {
		rng => //rng is implicitly assumed by compiler as
			//parameter of this function since
			//type of this definition requires one parameter
			//Lesson: if you stuck and do not understand, see
			//what you already understand. See previous
			//examples
			val (cand, next) = rng.nextInt
			(if(cand < 0) - (cand + 1) else cand, next)
	}

	def double2: Rand[Int] = {
		rng =>
			val (cand, next) = rng.nextInt
			val j = if (cand < 0) (cand + 1).toDouble else cand.toDouble
				(j/Int.MaxValue.toDouble, next)
	}

	def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
		rng => {
			val (a, rng2) = s(rng)
			(f(a), rng2)
		}
	}

	def nonNegativeEven: Rand[Int] = 
		map(nonNegativeInt)(i => i - i%2)

	def doubleWithMap = {
		map(rng => rng.nextInt){
			x => (if(x<0)(x + 1).toDouble else x.toDouble)/Int.MaxValue.toDouble
		} 
	}

	def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
		rng => val (a, rng2) = ra(rng)
			val (b, rng3) = rb(rng2)
			(f(a, b), rng3) 
	}

	def intDouble2: Rand[(Int, Double)] = {
		map2(rng => rng.nextInt, ang => RNG.double(ang))((a, b) => (a, b))
	}

	def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = 
		map2(ra, rb)((_, _))

	val randIntDouble: Rand[(Int, Double)] = {
		//evaluate int value, then double function
		both(int, double)
	}

	val randDoubleInt: Rand[(Double, Int)] =
		//evaluate double function, then int value
		both(double, int)

	//type Rand[+A] = RNG => (A, RNG)
	//List(RNG => (A, RNG), RNG => (A, RNG), RNG => (A, RNG) ...)
	//becomes
	//RNG => (List[A], RNG)
	//eq
	//List(RNG => (Int, RNG), RNG => (Int, RNG), RNG => (Int, RNG) ...)
	//RNG => (List[Int], RNG)
	//def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 

	def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
		fs foldLeft (Rand[List.empty[A]]) {
			(acc, a) =>
				val d = 	
		}
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
val (r017, rng017) = RNG.intsTailRec(5)(rng016)
val (r018, rng018) = RNG.intsTailRec(5)(rng017)
val r019  = RNG.int
val r020 = RNG.unit(0)
val (r021, rng021) = RNG.nonNegative2(rng002)
val (r022, rng022) = RNG.nonNegativeEven(rng002)
val (r023, rng023) = RNG.doubleWithMap(rng003)
val (r024, rng024) = RNG.intDouble2(rng005)
