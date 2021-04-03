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

	def foldRight[B](z: => B) (f: (A, =>B) =>B): B = {
		this match {
			case Cons(h, t) => f(h(), t().foldRight(z)(f))
			case _ => z
		} 
	}

	def existsVFR(p: A=> Boolean): Boolean = 
		foldRight(false) ((a, b) => p(a) || b)

	def mapVFR[S](f: A=>S): Scream [S] = {
		this.foldRight(empty[S])((a, b) => cons(f(a), b))
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
			x match {
				case (n:Int, Cons(h, t)) if n > 0 => 
					Some((h(), (n - 1, t())))
				case _ =>
					None
			}
		}
		
	}

	def zipVU[B](p: Scream[B]): Scream[(A, B)] = {
		unfold((this, p)){ x => 
			x match {
				case ((Cons(h, t), Cons(k, e))) =>
					Some((h(), k()), (t(), e()))
				case _ =>
					None
			}
			
		}
	}

	def zipAll[B](p: Scream[B]): Scream[(Option[A], Option[B])] = {
		unfold((this, p)){x=>
			x match {
				case ((Cons(h, t), Cons(k,e))) =>
					Some(((Some(h()), Some(k())), (t(), e())))
				case ((Cons(h,t), _)) =>
					Some(((Some(h()), None), (t(), Scream.empty)))
				case ((_ , Cons(k, e))) =>
					Some(((None, Some(k())), (Scream.empty, e())))
				case(_, _) =>
					None
				}
			}
		
	}

	def startsWith[A](a: Scream[A]): Boolean = {
		(this, a) match {
			case ((Cons(h, t), Cons(k, e))) =>
				if (h() == k())
					t().startsWith(e())
				else
					false
			case (empty, Cons(k, e)) =>
				false
			case (Cons(h, t), empty) =>
				true
			case _ =>
				true
		}
	}
	//use unfold please
	def tails: Scream[Scream[A]] = {
			unfold(this){x =>
				x match {
					case Cons(h, t) =>
						Some(x, t())
					case _ =>
						None
				}
			}
	}

	def hasSubsequence[A](s:Scream[A]): Boolean = {
		tails existsVFR (_ startsWith s)	
	}
	def scanRight[S](s:S)(f:(A, =>S) => S): Scream[S] = {
		tails mapVU (x => x.foldRight(s)(f))
		//here it is clear that tails type parameter is this 
		//Scream type parameter , that is A
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

	/*
     * this function is function with multiple parameter group 
	 * check 
	 * https://alvinalexander.com/scala/fp-book/how-to-write-functions-multiple-parameter-groups/
	 * On result of function f(s:S) is Some 2-tuple ((a:A, s:S))
	 * cons ._1 head with unfold of ._2
	**/
	def unfold[A, S](z:S)(f:S => Option[(A, S)]): Scream[A]= {
		f(z) match {
			case Some(x) =>
				cons(x._1, unfold(x._2)(f))
			case None =>
				empty	
		}
	}
	//VU -> via unfold
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

val r007 = r004.zipVU(r005)
val e007 = r007.takeVU(3).toList

val r008 = r005.zipAll(Scream.empty)
val e008 = r008.take(5).toList

val r009 = Scream(List(1, 2, 3, 4):_*)
val s009 = Scream(List(1, 2):_*)
val e009 = r009.startsWith(s009)
val f009 = s009.startsWith(r009)
val g009 = r009.hasSubsequence(s009) //true
val h009 = r009.hasSubsequence(Scream(List(2, 3):_*)) //true
val i009 = r009.hasSubsequence(Scream(List(1, 3, 4):_*))//false 
val a010 = Scream(1, 2, 3).scanRight(0)(_ + _).toList//List(6, 5, 3)
val a011 = r002.mapVFR(x => x * x).take(7).toList//List(0, 1, 1, 4, 9, 25, 64)

