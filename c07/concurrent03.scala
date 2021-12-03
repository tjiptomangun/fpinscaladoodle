case class Par[T] (in: T) {
}
object Par {
	/**
	 * taking an unevaluated A and returning a computation
	 * that might evaluate it in separate thread. It is a
	 * unit of parallelism that just wrap single value
	 */
	def unit[A](a: A): Par[A] = ???
	/**
	 * extracting the resulting value from a parellel computation
	 */
	def get[A](a: Par[A]): A = ???
	def map2[T, U](a: Par[T], b: Par[T])(fn: (T, T)=> U): Par[U] = ???
	def fork[A](a: => Par[A]): Par[A] = ???
	def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

}

def sum(ints: IndexedSeq[Int]): Par[Int] = {
	if (ints.size <= 1)
		Par(ints.headOption getOrElse 0) 
	else {
		val (l, r) = ints.splitAt(ints.length/2)
		Par.map2(Par.fork(sum(l)),  Par.fork(sum(r)))(_ + _);
	}
}

