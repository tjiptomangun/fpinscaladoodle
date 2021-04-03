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
		import scala.language.postfixOps
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
	val abc : Rand[Int] = _.nextInt

	/**
	 * This unit function accepts one parameter  `a` of class A
	 * and returns a function that accepts a parameter
	 * of class RNG and returns tuple2 of (`a`, RNG)
	 **/
	def unit[A](a: A): Rand[A] = 
		rng => (a, rng)

	/**
		* same as unit, just to show that
		* this function will use pattern recognition
		*/
	def unit2[A](a: A): Rand[A] =
		s => (a, s)


	//try to implement this, see nonNegative above
	def nonNegative2: Rand[Int] = {
		rng =>  //this function will return a function which take
			//a parameter of type RNG and returns tuple2 of (Int, RNG)
			//rng here is our implementation of returned 
			//function parameter
			//Lesson: if you stuck and do not understand, see
			//what you already understand. See previous
			//examples
			val (cand, next) = rng.nextInt
			(if(cand < 0) - (cand + 1) else cand, next)
	}

	def double2: Rand[Double] = {
		rng =>  //this function will return a function which take
			//a parameter of type RNG and returns tuple2 of (Double, RNG)
			val (cand, next) = rng.nextInt
			val j = if (cand < 0) (cand + 1).toDouble else cand.toDouble
				(j/Int.MaxValue.toDouble, next)
	}

	def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
		rng => {//this function will return a function which takes
			//a parameter of type RNG and returns tuple2 of (B, RNG)
			//s is a function that takes RNG and returns tuple2 of (A, RNG)
			val (a, rng2) = s(rng)//value a extracted by applying 
						//s to this function param
			(f(a), rng2)
		}
	}

	def nonNegativeEven: Rand[Int] = 
		map(nonNegativeInt)(i => i - i%2)

	def doubleWithMap = {
		//map(rng => rng.nextInt){
		//that line above can be simplified
		map(_.nextInt){
			x => (if(x<0)(x + 1).toDouble else x.toDouble)/Int.MaxValue.toDouble
		} 
	}

	def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
		//this function will return a function that takes a parameter of class RNG
		//and returns tuple of (C, RNG)
		//ra is a function that takes a parameter of class RNG
		//and returns tuple of class (A, RNG)
		//rb is a function that takes a parameter of class RNG
		//and returns tuple of class (B, RNG)
		rng => val (a, rng2) = ra(rng)//value 'a' extracted by applying ra to function parameter
			val (b, rng3) = rb(rng2)//value 'b' extracted by applying rb to rng2
			(f(a, b), rng3) 
	}

	def intDouble2: Rand[(Int, Double)] = {
    //map2(rng => rng.nextInt, ang => RNG.double(ang))((a, b) => (a, b))
    //that line above can be simplified using line below
    map2(_.nextInt, RNG.double)((a, b) => (a, b))
	}

	// combine the value of type A and the value of type B using combinators and map2
	def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = 
		map2(ra, rb)((_, _))

	val randIntDouble: Rand[(Int, Double)] = {
		//evaluate int value, then double function
		both(int, double)
	}

	val randDoubleInt: Rand[(Double, Int)] =
		//evaluate double function, then int value
		both(double, int)


  //Implement sequence for combining a List of transitions into a single
  //transition
  //
  // type Rand[+A] = RNG => (A, RNG)
	//List(RNG => (A, RNG), RNG => (A, RNG), RNG => (A, RNG) ...)
	//becomes
	//RNG => (List[A], RNG)
	//eq
	//List(RNG => (Int, RNG), RNG => (Int, RNG), RNG => (Int, RNG) ...)
	//RNG => (List[Int], RNG)
	//def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
	//fs is a set of functions which each of them takes RNG as its parameter 
	//and returns tuple (A, RNG)
	//

	def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
		fs.foldRight(unit(List.empty:List[A]))	{
      //foldRight first partial param is initial value with type is return type
			(a, b) => map2(a, b)((x, y) => x :: y)
		} 
	}

//	def sequenceBreakDown[A](fs: List[Rand[A]]): Rand[List[A]] = {
//		fs.foldRight(rng:RNG => (List.empty:List[A], rng))	{
//			(a, b) => map2(a, b)((x, y) => x :: y)
//		}
//	}
	def nonNegativeLessThan(n: Int): Rand[Int] = {
		map(nonNegativeInt) ( _ % n )
	}

	def nonNegativeLessThan2(n: Int): Rand[Int] = {
		/**
		 * `i` is next non negative
		 * `n` is max exclusive
		 * `mod` is `i` % `n`
		 * (`n` - 1) is max value inclusive
		 * `i` + (`n` - `1`) - `mod` < 0 happens if
		 * `mod` is greater than `i` + `n` - `1`.
		 * which is when?
		 * Int.MaxValue = 2147483647
		 * `i` will never greater than Int.MaxValue
		 * but if `i` + (`n` - `1`) greater than Int.MaxValue
		 * they become negative. It is a java limitation
		 * and this if is just a workaround.
		 * Why do not just return mod? Because author
		 * of this book thought that it is not fair
		 * that if nonNegativeInt returns any module value
		 * in the last partition befor maxInt, it will not returns
		 * every valid value. If it ever touch this
		 * partition some module will appear more
		 * frequent than other modulo
		 */
		rng =>
			val (i, rng2) = nonNegativeInt(rng)
			val mod = i % n
			if (i + (n - 1) - mod >= 0)
				(mod, rng2)
			else
				nonNegativeLessThan2(n)(rng)
	}

	/**
	 * This function accepts RNG => (A, RNG)
	 * and function A => RNG  => (B, RNG)
	 * Returns RNG => (B, RNG)
	 *
	 **/
	def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
		rng =>
			val (a, b) = f(rng)
			g(a)(b)
	}

	def nonNegativeLessThan3(n: Int): Rand[Int] = {
		flatMap(nonNegativeInt){ 
			i => rng =>
					val mod = i % n
					if (i + (n - 1) - mod >= 0)
						(mod, rng)
					else 
						nonNegativeLessThan3(n)(rng)	
		}	
	}

	def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
		flatMap(s){ i => rng => (f(i), rng) }
	}

	def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
		flatMap(ra){
			i => {rng => flatMap(rb){
				j =>
					rng2 => (f(i, j), rng2)
			}(rng)} 
		}
	}

	def nonNegativeEven2: Rand[Int] = 
		mapViaFlatMap(nonNegativeInt)(i => i - i%2)
	
	def intDouble2ViaFlatMap: Rand[(Int, Double)] = {
		map2ViaFlatMap(rng => rng.nextInt, ang => RNG.double(ang))((a, b) => (a, b))
	}

	def rollDie: Rand[Int]  = map(nonNegativeLessThan(6)) (_ + 1);
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
r023
val (r024, rng024) = RNG.intDouble2(rng005)
val r025 = RNG.sequence(List(RNG.unit(1), RNG.unit(2), RNG.unit(3)))
val (r026, rng026) = r025(rng024)
val (r027, rng027) = RNG.nonNegativeEven2(rng002)
val (r028, rng028) = RNG.intDouble2ViaFlatMap(rng005)
RNG.nonNegativeLessThan(10)(RNG.SimpleRNG(10))
//test what is abc
RNG.abc
//test what is int
RNG.int
//trait RNGR {
//	def nextInt: (Int, RNGR)
//}
RNG.rollDie;
type Rand[+A] = RNG => (A, RNG) 
val int : Rand[Int] = _.nextInt

val flgn :(String) => Int = _.length
val flgn2 :(String) => (String, Int) = (x => (x, x.length))

//val int : Rand[Int] = _.nextTop //10: error: value nextTop is not a member of RNG

