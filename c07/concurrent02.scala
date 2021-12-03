case class Par[T] (in: T) {
}
object Par {
	def map2[T, U](a: Par[T], b: Par[T])(fn: (T, T)=> U): Par[U] = ???

}

def sum(ints: IndexedSeq[Int]): Par[Int] = {
	if (ints.size <= 1)
		Par(ints.headOption getOrElse 0) 
	else {
		val (l, r) = ints.splitAt(ints.length/2)
		Par.map2(sum(l),  sum(r))(_ + _);
	}
}

