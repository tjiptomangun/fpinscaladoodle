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
					t().drop(k - 1) 
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

	def existsLong(p: A => Boolean): Boolean = this match{
		case Cons(h, t) => p(h()) || t().exists(p)
		case _ => false
	}

	def foldRight[B](z: => B)(f: (A, =>B) => B): B = {
		this match {
			case Cons(h, t) => 
				f(h(), t().foldRight(z)(f))
			case _ =>
				z
		}
	}

	def exists(p: A => Boolean): Boolean = {
		foldRight(false)((a, b) => p(a) || b )
	}

	def forAll(p: A => Boolean): Boolean = {
		foldRight(true)((a, b) => p(a) && b)
	}

	def takeWhileVFR (p: A => Boolean): Stream [A] = {
		foldRight(Stream.empty: Stream[A]){(a, b) => 
			if (p(a)){
				Cons(() => a, () => b)
			}
			else {
				Stream.empty
			}
		}
	}

	def headOptionViaFoldRight(): Option[A] = {
		foldRight(None: Option[A]){
			(a, b) =>
				Some(a)
				
		}
	}

	def map[B](j: A => B): Stream [B] = {
		foldRight(empty: Stream[B]) {
			(a, b) =>
				cons(j(a), b)
		}
	}

	def filter(j: A => Boolean): Stream[A] = {

		foldRight(empty: Stream[A]){
			(a, b) =>
				if (j(a))
					cons(a, b)
				else
					b
		}
	}

	def append[ B >: A](j: => Stream[B]) : Stream [B] = {
		foldRight(j){
			(a, b) =>
				cons(a, b)
		}
	}

	def append2[B >: A](j: => Stream[B]): Stream [A] = {
		val x:List[A] = j.toList.map(_.asInstanceOf[A])
		val k = apply(x:_*)
		foldRight(k){
			(a, b) =>
				cons(a, b)
		}
	} 

	def flatMap [B](f: A => Stream[B]): Stream[B] = {
		foldRight(empty[B]) {
			(a, b) =>
				f(a).append(b)
		}
	}

	def find (f: A => Boolean): Option[A] =
		filter(f).headOption

	def mapViaUnfold[B](j: A => B): Stream[B] = {
		unfold(this) {
			case Cons(h, t) => 
				Some((j(h()), t()))

			case empty: Stream[A] =>
				None

		}
	}

	def takeViaUnfold(n: Int): Stream[A] = {
		unfold(this) {
			case Cons(h, t) if n > 0 =>
				Some(h(), t().takeViaUnfold(n - 1))
			case _ =>
				None 
		}
	}

	def takeWhileViaUnfold(j: A => Boolean) : Stream[A] = {
		unfold(this) {
			case Cons(h, t) if j(h()) =>
				Some(h(), t().takeWhileViaUnfold(j))
			case _ =>
				None		
		}
	}

	def zipWith[B, C](bs: Stream[B])(f:(A, B) => C) : Stream [C] = {
		unfold(this, bs) { 
			case (Cons(h1, t1), Cons(h2, t2)) =>
				Some((f(h1(), h2()), (t1(),t2())))
	
			case _ =>
				None 
		}
	}

	def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] = {
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

	def constant[A](a: A): Stream[A] = {
		cons(a, constant(a)) 
	}

	val ones : Stream[Int] = Stream.cons(1, ones)

	def from(n: Int): Stream[Int] = {
		Stream.cons(n, from(n + 1))
	}

	def fibs(): Stream[Int] = {
		def _fibs(x: Int, y: Int): Stream[Int] = {
			cons(y, _fibs(y, x + y))
		}
		cons(0, _fibs(0, 1))
	}

	def fact(): Stream[Int] = {
		def _fact(x: Int, y: Int): Stream[Int] = {
			cons(y, _fact(y, ((y/x)+1)*y))
		}
		cons(1, cons(1, _fact(1, 2)))
	}

	def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream [A] = {
		f(z) match {
			case None =>
				Stream.empty
			case Some(x) =>
				cons(x._1, unfold(x._2)(f))
		}
	}

	def fibsViaUnfold(): Stream [Int]  = {
		unfold((0, 1)){
			(x:(Int, Int)) =>
				Some((x._1), (x._2, x._1 + x._2))
				
		}
	}

	def fromViaUnfold(n: Int): Stream[Int] = {
		unfold(n){
			(x: Int) =>
				x match {
					case y if y < 0 =>
						None
					case z =>
						Some((z, z+1))
				}	
		}
	}

	def constantViaUnfold[A](a: A) : Stream[A] = {
		unfold(a){
			(x: A) =>	
				Some((x, x))
		}
	} 

	def onesViaUnfold(): Stream[Int] = {
		unfold(1) {
			(x: Int) =>
				Some((x, x))
		}
	}

	

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
val g5 = g1.takeWhile (_ < 5)
val g6 = g1.exists(_ < 3)
val g7 = g1.exists(_ < 0)
val g8 = g1.forAll(_ > 0)
val g9 = g1.takeWhileVFR(_ < 5)
val gA = g1.map(_ + 4).toList //gA = List(5, 6, 7, 8, 9, 10)
val gB = g1.map(_ + 4).filter(_ > 7).toList //List[Int] = List(8, 9, 10)
val gC = g1.append(g1)
val gD = g1.append2(g1)
val gE = g1.flatMap((x: Int) => Stream.cons(x+0.1, Stream.empty:Stream[Double])).toList
val gF:Stream[Int] = g1.append(Stream.cons(40,Stream.cons(20, Stream.cons(30,Stream.empty:Stream[Int]))))


val gH = Stream.constant("Hello").take(10).drop(5)
val gI = g1.mapViaUnfold(_ + 4).toList
val gJ = g1.takeViaUnfold(4).toList

