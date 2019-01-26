
/* 
 * Notes
 * x: () => A and x: => A are different on calling style and signature but 
 * do exactly the same thing.
 * It is just mean that in first form we need to have () in applying
 * and no need to have () in second form of applying
 *
 * def foldRight[B](z: => B)(f: (A, => B) => B): B =
 * The arrow `=>` in front of the argument type `B` means that 
 * the function `f` takes its second argument by name and may 
 * not choose to evaluate it. 
 *
 * If the evaluation of an expression runs forever or throws an error 
 * instead of returning a definite value, we say that the expression 
 * doesn’t terminate, or that it evaluates to bottom. A function f is 
 * strict if the expression f(x) evaluates to bottom for all x that 
 * evaluate to bottom.
 * As a final bit of terminology, we say that a non-strict function 
 * in Scala takes its arguments by name rather than by value.
 *
 * Unevaluated form of an expression is call thunk.
 * We force the thunk to evaluate and get result. We do so by invoking
 * the function.
 *
 * refence Functional Programming in Scala Ch.05
 *
 * "Evaluates to bottom" is a way to say that an expression doesn't return 
 * normally: it throws an exception, gets stuck in a loop, or halts the 
 * program. The reason that phrase is used is because sometimes it's 
 * convenient to pretend that all expressions evaluate to a value. Once 
 * you pretend that non-returning expressions produce a value called bottom 
 * you can simplify your description of how expressions interact.
 * These uses of the word "bottom" come originally from formal logic and 
 * then into programming via formal language semantics.
 * https://stackoverflow.com/questions/25872075/how-to-understand-the-sentence-or-that-it-evaluates-to-bottom

 * A function f is said to be strict if, when applied to a non-terminating 
 * expression, it also fails to terminate.
 * The entity ⊥ , called bottom, denotes an expression which does not return 
 * a normal value, either because it loops endlessly or because it aborts 
 * due to an error such as division by zero. A function which is not strict 
 * is called non-strict. A strict programming language is one in which 
 * user-defined functions are always strict.
 * Intuitively, non-strict functions correspond to control structures. 
 * Operationally, a strict function is one which always evaluates its argument; 
 * a non-strict function is one which may not evaluate some of its arguments. 
 * Functions having more than one parameter may be strict or non-strict 
 * in each parameter independently, as well as jointly strict in several 
 * parameters simultaneously.  
 * https://en.wikipedia.org/wiki/Strict_function

 * By-name parameters are only evaluated when used.
 * By-name parameters have the advantage that they are not evaluated 
 *   if they aren’t used in the function body.
 * On the other hand, by-value parameters have the advantage that 
 *   they are evaluated only once.
 * ref https://docs.scala-lang.org/tour/by-name-parameters.html
 *
 * A parameter of type A acts like a val (its body is evaluated once, 
 *   when bound) 
 * and 
 * one of type => A acts like a def (its body is evaluated whenever it is used).
 * 
 * see cons and cons2 demo below 
 * cons headOption will evaluate hd once,
 * cons2 headOption will evaluate hd on each call.
 * This is the evidence that hd is evaluated once its value is requested.
 * Not when cons is called since it is by name parameter, not when 
 * hd assigned to hd since it is lazy, not even when Cons is called since 
 * this is also by name parameter.
 * But when head is evaluated in which hd head need its value in which
 * hd need to have its value evaluated.
 *
 * Head and tail cached with lazy.
 * Lazy variable in a class is evaluated when the variable is referenced 
 * in class instances.
 * Strict variable in a class is evaluated as instance of the class is created.
 * See lazyness.scala for more details.
 * 
 * Caching evaluation of non strict parameter value into lazy variable
 * will delay its evaluation until it needs the value.
 */

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
			case Cons(h, t) if (p(h())) =>
				cons(h(), t().takeWhile(p))
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
		unfold(this, bs) {
			case (Cons(h1, t1), Cons(h2, t2)) =>
				Some((Some((h1())), Some(h2())), (t1(),t2()))

			case (Cons(h1, t1), _) =>
				Some((Some(h1()), None:Option[B]), (t1(), empty))

			case (_, Cons(h2, t2)) =>
				Some((None:Option[A], Some(h2())), (empty, t2()))

			case _ =>
				None 
			
		}
	}

	def startsWith [B](bs: Stream[B]): Boolean = {
		zipAll(bs).takeWhile(!_._2.isEmpty) forAll(y => y._1 == y._2)
	}

	def tails : Stream[Stream[A]] = {
		unfold(this) {
			case x@Cons(h1, t1) =>
				Some((x, t1()))
			case y@Empty =>
				Some((y, null))
			case _ =>
				None
		}
	}

	def hasSubsequence [B](bs: Stream[B]): Boolean = {
		tails exists (_ startsWith bs)
	}

	// todo
	// prove your impl using fibs
	// Stream.fibs.scanRight ..... 
	// find the most effective way
	// check answer/answer key
	def scanRightWrong[B](initVal: => B)(f: (A, =>B) => B): Stream[B] = {
		this match {
			case Cons(h, t) =>
				cons(this.foldRight(initVal)(f), t().scanRight(initVal)(f))
			case _ =>
				cons(initVal, Empty)
		} 
	}

	def scanRight[B](initVal: => B)(f: (A, =>B) => B): Stream[B] = {
		foldRight((initVal, Stream(initVal))) {
			(a, passed) =>
				// this lazy val prevent reevaluation
				// why is this necessary ?
				// because passed already calculated in
				// previous iteration and cons-ed (stream-ed)
				lazy val tmp = passed
				val b = f(a, tmp._1)
				(b, cons(b, tmp._2))
		}._2
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

val e = Stream.cons({println("goodbye cons!"); Thread.sleep(5000); 1}, Stream.empty) 
	
val f = Stream.cons2({println("goodbye cons2!"); Thread.sleep(5000); 1}, Stream.empty)

e.headOption //will only print and sleep once
f.headOption //will print and sleep on each call

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
val gK = g1.map(_ + 4).take(4)
val gL = g1.zipWith(gK)((x, y) => x - 5)
val gM = g1.zipAll(gK)
val gN = g1.take(2)
val gO = g1.take(4).drop(1)

val r001 = g1.startsWith(gN)
val r002 = g1.startsWith(gO)
val r003 = g1.hasSubsequence(gO) //true
val r004 = gO.hasSubsequence(gN) //false
val r005 = g1.scanRight(0)(_ + _)


