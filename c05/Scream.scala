/**
 * Second try on lazyness
 */

import Scream._;

sealed trait Scream[+A] {
	def headOption: Option[A] = this match {
		case Empty => None
		case Cons(h, t) => Some(h())
	}

	def toList: List[A] = {
		this match {
			case Cons(h, t) =>
				h()::t().toList
			case _ =>
				List.empty[A]
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
			Scream.cons(n + nm1, fibgen(n + nm1, n))
		}

		cons(0, cons(1, fibgen(1, 0)))
	}
}

val r001 = fibs
r001.headOption
