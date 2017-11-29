sealed trait Stream[+A] {
	def headOption: Option[A] = this match {
		case Empty =>
			None
		case Cons(h, t) =>
			Some(h())
	}

	def toList: List[A] = {
		def inner(a: Stream[A]): List[A] = {
			a match {
				case Cons(a, b) =>
					a()::inner(b())
				case _ =>
					Nil
			}
		}
		inner(this)
	}

	def take(k: Int):Stream[A] = {
		def inner(str :  Stream[A], n: Int): Stream[A] = {
			if (n > 0){
				str match {
					case Cons(a, b) =>
						Stream.cons(a(), inner(b(), n - 1))
	
					case _ =>
						Stream.empty[A]

				}
			}
			else
				Stream.empty[A]
		}

		inner(this, k);
		
	}
}
case object Empty extends Stream[Nothing]
case class Cons[+A] (h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
	def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
		lazy val head = hd;
		lazy val tail = tl;
		Cons(() => head, () => tail) 
	}

	def cons2[A](hd: => A, tl: => Stream[A]): Stream[A] = {
		val head = hd;
		val tail = tl;
		Cons(() => head, () => tail) 
	}
	
	def empty[A]: Stream[A] = Empty

	def apply[A] (as: A*): Stream[A] =
		if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}

val d = List({println ("zee"); 1}, {println ("wee"); 3}, 5, 6, 2, 4, 11, 32, 12, 7)
val r0 = Stream(d:_*)
val r1 = r0.toList

val c = Cons(()=>{println("Hello"); 1}, ()=>Stream.empty)
c.headOption
c.headOption

val e = Stream.cons({Thread.sleep(3000); 1}, Stream.empty)

//val f = Stream.cons2({Thread.sleep(3000); 1}, Stream.empty)
