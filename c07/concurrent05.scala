import java.util.concurrent._
object Par {
	type Par[A] = ExecutorService => Future[A]
	/**
	 * Fully evaluates a given Par, spawning parallel computations
	 * as requested by fork and extracting the result value
	 */ 
	def run[A](s: ExecutorService)(a: Par[A]): Future[A] = {
		println("run called")
		a(s)
	}
	/**
	 * taking an unevaluated A and returning a computation
	 * that might evaluate it in separate thread. It is a
	 * unit of parallelism that just wrap single value.
	 * Creates a computation that immediately results in the
	 * value a
	 */
	def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

	private case class UnitFuture[A](get: A) extends Future[A] {
		def isDone = true
		def get(timeout: Long, units: TimeUnit) = get
		def isCancelled = false
		def cancel(evenIfRunning: Boolean): Boolean = false
	}
	/**
	 * Combines the results of two parallel computations
	 * with a binary function. The result is still par, which
	 * means will not be evaluated until es provided
	 */
	def map2[A, B, C](a: Par[A], b: Par[B])(fn: (A, B)=> C): Par[C] = { 
		(es: ExecutorService) => {
			println("map2")
			println(s"a ${a}")
			println(s"b ${b}")
			val af = a(es)
			val bf = b(es)
			//get below (af.get, bf.get) is Future get
			UnitFuture(fn(af.get, bf.get))
		}
	}
	/**
	 * Marks a computation for concurrent evaluation by run
	 */
	def fork[A](a: => Par[A]): Par[A] = {
		println("fork");
		es => es.submit(new Callable[A] {
			def call =  {
				println(s"called ${Thread.currentThread().getName}");
				println(s"a is ${a}");
				println(s"es is ${es}");
				val ret = a(es).get
				println(s"ret ${ret}")
				ret
			}
		})
	}
	/**
	 * Wraps the expression `a` for concurrent evaluation by run
	 */
	def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
}
def sumInt (a: Int, b:Int) :Int = {
	a + b
}
def sum(ints: IndexedSeq[Int]): Par.Par[Int] = {
	if (ints.size <= 1){
		Par.unit(ints.headOption getOrElse 0) 
	}
	else {
		val (l, r) = ints.splitAt(ints.length/2)
		Par.map2(Par.fork(sum(l)),  Par.fork(sum(r)))(sumInt);
	}
}
val pool: ExecutorService = Executors.newFixedThreadPool(1)
val j = sum(IndexedSeq(1, 2, 3, 4))
j(pool).get

