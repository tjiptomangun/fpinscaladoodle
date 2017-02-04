//package fpinscala.datastructure
import scala.annotation.tailrec

sealed trait List[+A] 

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List{
	def sum(ints: List[Int]): Int = ints match {
		case Nil => 
			0
		case Cons(x, xs) => 
			x + sum(xs)
	}

	def product(ds: List[Double]): Double = ds match {
		case Nil => 
			1.0
		case Cons(0.0, _) => 
			0.0
		case Cons(x,xs) => 
			x * product(xs) 
	}

	def apply[A] (as: A*): List[A] = {
		if (as.isEmpty)
			Nil
		else Cons(as.head, apply(as.tail: _*))
	}

	def tail[A] (as: List[A]): List[A] = {
		as match {
			case Nil =>
				Nil
			case Cons(a, t) =>
				t
		}
	}

	def setHead[A] (as: List[A], a: A): List[A] = {
		Cons(a, tail(as))
	}

	@tailrec
	def drop[A] (l: List[A], n: Int): List[A] = {
		n match {
			case 0 =>
				l
			case x =>
				drop (tail(l), n -1)
		}
	}

	@tailrec
	def dropWhile[A] (l: List[A], f: A => Boolean) :  List[A] = {
		l match {
			case Nil =>
				Nil
			case Cons(h, t) =>
				f.apply(h) match {
					case false =>
						Cons(h, t)	
					case _ =>
						dropWhile(t, f)
				}
				
		}
	}

	def append[A](a1: List[A], a2: List[A]): List[A] = {
		a1 match {
			case Nil => 
				a2
			case Cons(h, t) =>
				Cons(h, append(t, a2))
		}
	}

	def init[A](l: List[A]): List[A] = {
		l match {
			case Cons(h, Nil) =>
				Nil 
			case Cons(h, t) =>
				append(Cons(h, Nil), init(t)) 
		}	
	}

	def foldRight[A, B] (as: List[A], z: B) (f: (A, B) => B): B = {
		as match {
			case Nil => z
			case Cons(x, xs) => f(x, foldRight(xs, z)(f))
		}
	}

	def sum2(ns: List[Int]) = {
		foldRight(ns, 0)((x, y) => x + y)
	}

	def product2(ns: List[Double]) = {
		foldRight(ns, 1.0) (_ * _)
	}

	//ex. 3.7
	//cannot lah, because its function  does not accept list
	def productX(ns: List[Double]) = {
		foldRight(ns, 1.0) (_ * _)
	}

	def reflect [A] (ns: List[A]) = {
		foldRight(ns, Nil: List[A]) (Cons(_, _))
	}

	def length [A] (ns: List[A]): Int = {
		ns match {
			case Nil => 0
			case Cons(x, xs) =>
				foldRight(xs, 1) ((x, y) => 1 + length(xs))
		}
	}

	def length2 [A] (ns: List[A]): Int = {
		foldRight (ns, 0) ((_, acc) => acc + 1)	
	}

	@tailrec
	def foldLeft [A, B] (as: List[A], z: B) (f: (B, A) => B): B = {
		as match {
			case Nil => 
				z
			case Cons(x, xs) =>  
				foldLeft (xs, f(z, x)) (f) 
		}
	}
	
	def sum3(ns: List[Int]) = {
		foldLeft (ns, 0) ((x, y) => x + y)
	}

	def product3 (ns: List[Double]) = {
		foldLeft (ns, 1.0) (_ * _)
	}

	def length3[A](ns: List[A]): Int = {
		foldLeft (ns, 0) ((acc, _) => acc + 1)
	}

	def reverseXX[A](ns: List[A]): List[A] = {
		foldLeft (ns, Nil: List[A]) ((acc, a) => append(Cons(a, Nil), acc))
	} 

	def appendViaFoldRight[A] (a1: List[A], a2: List[A]): List[A] = {
		foldRight (a1, a2) ((x, y) => Cons(x, y)) 
	}

	def appendViaFoldLeft[A] (a1: List[A], a2: List[A]): List[A] = {
		foldLeft (foldLeft(a1, Nil: List[A]) ((x, y) => Cons(y, x)), a2) ((x, y) => Cons(y, x))
	}

	def concatViaFoldRight[A] (a1: List[List[A]]): List[A]  = {
		foldRight (a1, Nil: List[A]) ((x, y) => appendViaFoldRight(x, y))
	}

	def reverse[A] (a1: List[A]): List[A] = {
		foldLeft (a1, Nil: List[A]) ((acc, a) => Cons(a, acc)) 
	}

	def concatViaFoldLeft[A] (a1: List[List[A]]): List[A] = {
		val a2 = reverse (a1)
		foldLeft (a2, Nil: List[A]) ((acc, a) => appendViaFoldLeft(a, acc)) 
	}

	def foldLeftViaFoldRight [A, B] (l: List[A], z: B) (f: (B, A) => B): B = {
		foldRight (l,  (b: B) => b) ((a, g) => b => g(f(b, a))) (z)
		// equals with foldRight (l,  (b: B) => b) ((a, g) => (b => g(f(b, a)))) (z)
	}

	def reverseWithFoldLeftViaFoldRight[A] (a1: List[A]): List[A] = {
		foldLeftViaFoldRight (a1, Nil: List[A]) ((acc, a) => Cons(a, acc))
	}
/*	
	def foldRightViaFoldLeft [A, B] (l: List[A], z: B) (f: (A, B) => B): B = {
		foldLeft(l, ???) ((???, a) => )
	}
*/

	def addOne (l: List[Int]):List[Int]  = {
		foldRight(l, Nil:List[Int]) ((a, acc) => Cons(a+1, acc))
	}

	def doubleToString (l: List[Double]): List[String] = { 
		foldRight(l, Nil:List[String]) ((a, acc) => Cons(a.toString, acc))
	}

	def map [A, B] (as: List[A]) (f: A=>B): List[B] = {
		foldRight(as, Nil:List[B]) ((a, acc) => Cons(f(a), acc))
	}

	def filter[A] (as: List[A]) (f: A=> Boolean): List[A] = {
		foldRight(as, Nil:List[A]) ((a, acc) => 
				if(f(a)) 
					Cons(a, acc)
				else
					acc
		) 
	}

	def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
		foldRight(as, Nil:List[B]) ((a, acc) => append(f(a) , acc))
		
	}
	

	
}

//ex. 3.8
//List.foldRight(List(1, 2, 3, 4, 5), Nil: List[Int]) (Cons(_, _))

val a = List (1, 2, 3, 4, 5, 6)
val b = List (11, 12, 13)
val c = List (25, 26, 27)
val d = List(a, b, c)
val j = List(.3, .6, .89, 1.4, 100.4)

val e = List.concatViaFoldRight(d)
val f = List.reverse(e)
val g = List.concatViaFoldLeft(d)
val h = List.reverseWithFoldLeftViaFoldRight(e)
val i = List.addOne(a)
val k = List.doubleToString(j)
val l = List.map(j)(_.toString)
val m = List.filter(j)(_ <1.0)
val n = List.flatMap(j)(i => List(i, i))
val test = n


