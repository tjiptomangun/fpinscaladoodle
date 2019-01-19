/**
 * Second try on lazyness
 */

sealed trait Scream[+A] {
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
				Scream.empty
			case _ =>
				this match {
					case Cons(h, t) =>
						Scream.cons(h(), t().take(n - 1))
					case _ =>
						Scream.empty 
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
						Scream.empty
				}
		}
	}

	def takeWhile(p: A => Boolean): Scream[A] = {
		this match {
			case Empty =>
				Scream.empty
			case Cons(h, t) =>
				p(h()) match {
					case true =>
						Scream.cons(h(), t().takeWhile(p))
					case _ =>
						Scream.empty
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
}
