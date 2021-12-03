case class Par[T] (in: T) {
}
object Par {
	/**
	 * taking an unevaluated A and returning a computation
	 * that might evaluate it in separate thread. It is a
	 * unit of parallelism that just wrap single value.
	 * Creates a computation that immediately results in the
	 * value a
	 */
	def unit[A](a: A): Par[A] = ???
	/**
	 * Combines the results of two parallel computations
	 * with a binary function
	 */
	def map2[A, B, C](a: Par[A], b: Par[B])(fn: (A, B)=> C): Par[C] = ???
	/**
	 * Marks a computation for concurrent evaluation by run
	 */
	def fork[A](a: => Par[A]): Par[A] = ???
	/**
	 * Wraps the expression `a` for concurrent evaluation by run
	 */
	def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
	/**
	 * Fully evaluates a given Par, spawning parallel computations
	 * as requested by fork and extracting the result value
	 */
	def run[A](a: Par[A]): A = ???

}

def sum(ints: IndexedSeq[Int]): Par[Int] = {
	if (ints.size <= 1)
		Par(ints.headOption getOrElse 0) 
	else {
		val (l, r) = ints.splitAt(ints.length/2)
		Par.map2(Par.fork(sum(l)),  Par.fork(sum(r)))(_ + _);
	}
}

