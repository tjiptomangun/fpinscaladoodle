import Stream._

sealed trait Stream[+A] {
	def headOption: Option[A] = this match {
		case Empty =>
			None
		case Cons(h, t) =>
			Some(h())
	}

	def toListNotTail: List[A] = { 
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

	def toList: List[A] = {
		@annotation.tailrec
		def inner(a: Stream[A], acc: List[A]): List[A]  = {
			a match {
				case Cons(h, t) => 
					inner(t(), h() :: acc)
				case _ =>
					acc
			}
		}

		inner(this, Nil: List[A]).reverse
	}

	def takeLong(k: Int):Stream[A] = {
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
		
	}//end of takeLong

	def take(k: Int): Stream[A] = {
		this match {
			case Cons(h, t) =>
				k > 1 match {
					case true =>
						Cons(h, () => t().take(k - 1))
					case _ =>
						Cons(h, () => Stream.empty)
				}
			case _ =>
				Stream.empty
		}
	}

	def dropLong(k: Int): Stream[A] = {
		@annotation.tailrec
		def inner(str: Stream[A], n: Int): Stream[A] = {
			if (n <= 0){
				str
			}
			else {
				str match {
					case Cons(a, b) =>
						inner(b(), n - 1)
					case _ =>
						Stream.empty[A]
				}
			}
		}
		inner(this, k)
	}

	@annotation.tailrec
	final def drop(k: Int): Stream[A] = {
		this match {
			case Cons(h, t) =>
				if (k > 0)
					drop(k - 1) 
				else 
					this
				
			case _ =>
				Stream.empty
		}
	}

	def takeWhileLong(p: A => Boolean):Stream[A] = {
		def inner(str :  Stream[A], p: A => Boolean): Stream[A] = { 
			str match {
				case Cons(a, b) =>
					if (p(a()))
						Stream.cons(a(), inner(b(), p))
					else
						Stream.empty[A]

				case _ =>
					Stream.empty[A]

			}
		} 

		inner(this, p);
	}

	def takeWhile(p: A => Boolean): Stream[A] = {
		this match {
			case Cons(h, t) =>
				p(h()) match {
					case true =>
						cons(h(), t().takeWhile(p))
					case _ =>
						Stream.empty
				}
			case _ =>
				Stream.empty
		}
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
		Cons(() => hd, () => tl) 
	}
	
	def empty[A]: Stream[A] = Empty

	def apply[A] (as: A*): Stream[A] =
		if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
		//if (as.isEmpty) empty else cons2(as.head, apply(as.tail: _*))

}

def fn (): Int= {
	println ("Hello World")
	1;	
}

val d = fn _
val ld = List(d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d)

val r0 = Stream(ld:_*)
val r1 = r0.toList

val c = Cons(()=>{println("Hello"); 1}, ()=>Stream.empty)
c.headOption
c.headOption

val e = Stream.cons({println("goodbye cons!"); Thread.sleep(3000); 1}, Stream.empty)
val f = Stream.cons2({println("goodbye cons2!"); Thread.sleep(3000); 1}, Stream.empty)

//val f = Stream.cons2({Thread.sleep(3000); 1}, Stream.empty)

val g0 = List(1, 2, 3, 4, 5, 6)
val g1 = Stream(g0:_*)
val g2 = g1.toList
val g3 = g1.take(2).toList
val g4 = g1.drop(2).toList
val g5 = g1.takeWhile (_ >= 5)

