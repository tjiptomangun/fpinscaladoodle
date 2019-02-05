/**
 * Second try on lazyness
 */



sealed trait Scream[+A] {
	import Scream._
	def headOption: Option[A] = this match {
		case Empty => None
		case Cons(h, t) => Some(h())
	}

	//simple but not tailrec	
	def toListNT: List[A] = {
		this match {
			case Cons(h, t) =>
				h()::t().toListNT
			case _ =>
				List.empty[A]
		}	
	}

	
	def toList: List[A] = {
	
		@annotation.tailrec
		def inner(a:Scream[A], accum:List[A]): List[A] = {
			a match {
				case Cons(h, t) =>
					inner(t(), accum :+ h())
				case _ =>
					accum
			}
		} 
		inner(this, List.empty[A])
	}

	def take(n: Int)  : Scream[A] = {
		n match {
			case 0 =>
				empty
			case _ =>
				this match {
					case Cons(h, t) =>
						cons(h(), t().take(n - 1))
					case _ =>
						empty 
				}
		}
		
	}

	def drop(n: Int) : Scream[A] = {
		n match {
			case 0 =>
				this
			case _ =>
				this match {
					case Cons(h, t) =>
						t().drop(n - 1)
					case _ =>
						empty
				}
		}
	}

	def takeWhile(p: A => Boolean): Scream[A] = {
		this match {
			case Empty =>
				empty
			case Cons(h, t) =>
				p(h()) match {
					case true =>
						cons(h(), t().takeWhile(p))
					case _ =>
						empty
				}
		}
	}

	def exists(p: A => Boolean): Boolean = this match {
		case Empty =>
			false
		case Cons(h, t) =>
			if (p(h()))
				true
			else
				t().exists(p)
	}

	def mapVU[S](f: A => S): Scream[S] = {
		unfold(this) { x => 
			x match {
				case Cons(h, t) =>
					Some((f(h()), t()))
				case _ =>
					None
			}
		}
	}

	def takeVU(n: Int) = {
		unfold((n, this)) { x =>
			x._2 != Empty && x._1 >= 0 match {
				case true =>
					Some((x._2.h(), (n - 1, x._2.t())))
				case _ =>
					None
			}
		}
	}

}
case object Empty extends Scream[Nothing]
case class Cons[+A] (h: () => A, t: () => Scream[A]) extends Scream[A]


object Scream {
	def cons[A] (hd: => A, tl: => Scream[A]): Scream[A] = {
		Cons(() => {lazy val head = hd; head}, () => {lazy val tail = tl; tail})
	}
	def empty[A]: Scream[A] = Empty

	def apply[A](as: A*): Scream[A] = {
		if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
	}
	def fibs: Scream[Int] = {
		def fibgen(n: Int, nm1: Int): Scream[Int] = {
			cons(n + nm1, fibgen(n + nm1, n))
		}

		cons(0, cons(1, fibgen(1, 0)))
	}

	def unfold[A, S](z:S)(f:S => Option[(A, S)]): Scream[A]= {
		f(z) match {
			case Some(x) =>
				cons(x._1, unfold(x._2)(f))
			case None =>
				empty	
		}
	}

	def fibsVU: Scream[Int] = {
		unfold((0, 1)){
			x => Some((x._1, (x._2, x._1 + x._2)))
		}
	}

	def fromVU(n: Int) = unfold(n)(x => Some((x, x+1)))

	def constantVU(n: Int): Scream[Int] = unfold(n){x => Some((x, x))} 
}

val r001 = Scream.fibs
val e001 = r001.take(10).toList

val r002 = Scream.fibsVU
val e002 = r002.take(10).toList

val r003 = Scream.fromVU(5)
val e003 = r003.take(4).toList

val r004 = Scream.constantVU(5)
val e004 = r004.take(4).toList

val r005 = Scream.fromVU(10)
val e005 = r005.mapVU(x => x + 100)
val f005 = e005.take(5).toList

val e006 = e005.takeVU(5).toList
