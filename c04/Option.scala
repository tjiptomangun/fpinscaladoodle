//{echo ":paste" & cat Option.scala & echo ":q";}| scala

import scala.{Option => _, Either => _, _}
import scala.annotation.tailrec
case object None extends Option[Nothing]
case class Some[+A](get:A) extends Option[A]

sealed trait Option[+A]{ 
	def map[B](f: A => B): Option[B] = {
		this match {
			case None => None
			case Some(x) => Some(f(x))
		}
	}

	def flatMap[B](f: A => Option[B]): Option[B] = {
		this match {
			case None =>
				None
			case Some(x) =>
				f(x)
		}
	}

	def getOrElse[B >: A](default: => B): B = {
		this match {
			case None =>
				default
			case  Some(x) =>
				x
		}	
	}

	def orElse[B >: A](ob: => Option[B]): Option[B] = {
		this match{
			case None =>
				ob
			case _ =>
				this
		}
	}

	def filter(f: A => Boolean): Option[A] = {
		this match {
			case None =>
				None
			case Some(x) =>
				f(x) match {
					case true =>
						this
					case _ =>
						None
				}
		}
	}
	
}

object Option{
}


val a0 = Some(3)
val a1 = a0.map(_ * 3)
val a2 = a0.flatMap(x => Some(x * 1.5))
val a3 = a0.flatMap{x => 
	x > 3 match {
		case true =>
			Some(x)
		case _ =>
			None
	}
}

val a4:Double = a2.getOrElse(0.0)
val a5:Int = a3.getOrElse(0)
val a6:Option[Double] = a2.orElse(Some(0.0))
val a7:Option[Int] = a3.orElse(Some(0))
val a8 = a2.filter(_ < 4.0)
val a9 = a2.filter(_ > 4.0)

def mean(xs: Seq[Double]): Option[Double] = {
	if (xs.isEmpty)
		None
	else
		Some(xs.sum/xs.length)
}

def variance(xs: Seq[Double]): Option[Double] = {
	mean(xs) flatMap (m => mean(xs.map(x => Math.pow(x-m, 2))))
}

val lDouble = List(1.1, 0.0, 3.5, 2.2, 4.1, 7.1, 9,2)
val m = mean (lDouble)
val v = variance(lDouble)

def lift[A,B](f: A=>B): Option[A] => Option[B] = _ map f

val abs0 = lift(math.abs)

val animals = List("cat", "dog", "snake")

animals.lift(0)
animals.lift(7) 

def insuranceRateQuote(age:Int, numberOfSpeedingTicket: Int): Double = {
	age * 0.01 + numberOfSpeedingTicket * 0.015
}

def Try[A](a: => A): Option[A] = {
	try Some(a)
	catch {case e: Exception => None}
}


def map2Old[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
	(a, b) match {
		case (Some(x), Some(y)) =>
			Some(f(x, y))
		case _ =>
			None
	}
}

def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
	a flatMap (x => b flatMap (y => Some(f(x, y))))
}

def map2_map[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
	a flatMap (x => b map (y => f(x, y)))
}

def parseInsuranceRateQuote (age: String, numberOfSpeedingTickets: String) = {
	val optAge: Option[Int] = Try(age.toInt)
	val optTickets : Option[Int] = Try(numberOfSpeedingTickets.toInt)
	map2(optAge, optTickets)(insuranceRateQuote)
} 

def parseInsuranceRateQuote_map (age: String, numberOfSpeedingTickets: String) = {
	val optAge: Option[Int] = Try(age.toInt)
	val optTickets : Option[Int] = Try(numberOfSpeedingTickets.toInt)
	map2_map(optAge, optTickets)(insuranceRateQuote)
} 

parseInsuranceRateQuote("100", "3y")
parseInsuranceRateQuote("100", "3")
parseInsuranceRateQuote("100x", "3")
parseInsuranceRateQuote("100x", "3y")

parseInsuranceRateQuote_map("100", "3y")
parseInsuranceRateQuote_map("100", "3")
parseInsuranceRateQuote_map("100x", "3")
parseInsuranceRateQuote_map("100x", "3y")

def sequence[A](a: List[Option[A]]): Option[List[A]] = {
	a.foldRight(Some(Nil): Option[List[A]]) {
		(x: Option[A], y:Option[List[A]]) => 
			y flatMap {_y => x flatMap { 
				_x => Some(_x :: _y)
			}
		}
	}
}

def sequence_rec[A](a: List[Option[A]]): Option[List[A]] = {
	a match {
		case (None :: y) =>
			None
		case Nil => Some(Nil)
		case (Some(x) :: y) =>
			sequence_rec(y).flatMap(_y => Some(x :: _y))
	}	
}

def sequence_recmap[A](a: List[Option[A]]): Option[List[A]] = {
	a match {
		case Nil => Some(Nil)
		case h :: t => h flatMap (_h => sequence_recmap(t).flatMap (_t => Some(_h :: _t)))
	}
}

def sequence_foldmap[A](a: List[Option[A]]): Option[List[A]] = {
	a.foldRight(Some(Nil):Option[List[A]])((h, n) => map2(h, n)((x, y) => x :: y)) 
} 

def _traverse[A, B](a: List[A])(f: A=> B): Option[List[B]] = {
	a.foldRight(Some(Nil): Option[List[B]]) ((h, acc) => Try(f(h)) flatMap (_f => acc.map(_acc => _f :: _acc)))
}

def _traverse_rec[A, B](a: List[A])(f: A=> B): Option[List[B]] = {
	a match {
		case  Nil =>
			Some(Nil)
		case h::t =>
			Try(f(h)) flatMap (_f => _traverse_rec(t)(f) map (_t => _f::_t))
	}
}

def _traverse_recmap[A, B](a: List[A])(f: A=> B): Option[List[B]] = {
	a match {
		case Nil =>
			Some(Nil)
		case h::t =>
			map2(Try(f(h)), _traverse_recmap(t)(f))((a, b) => a::b)
	}
}

def sequence_via__traverse[A](a: List[Option[A]]): Option[List[A]] = {
	_traverse(a)(_a => _a match{
			case Some(y) =>
				y
			case _ =>
				throw new Exception("error")
		})
}

def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
	a.foldRight(Some(Nil): Option[List[B]])((_a, acc) => map2(f(_a), acc)((x, y)=> x::y))	
}

def traverse_rec[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
	a match {
		case h :: t =>
			map2(f(h), traverse_rec(t)(f))((u, v) => u :: v)
		 case Nil =>
			Some(Nil)
	}	
}

def sequence_via_traverse[A](a: List[Option[A]]): Option[List[A]] = {
	traverse(a)(_a => _a)
}

val c = List(Some(1), Some(2), Some(3), Some(4))
val d:List[Option[Int]] = List(Some(1), None, Some(3), Some(4))
val e:List[String] = List("1", "2", "3")
val f:List[String] = List("y", "2", "3")
sequence(c)
sequence(d)
sequence_rec(c)
sequence_rec(d)
sequence_recmap(c)
sequence_recmap(d)
sequence_foldmap(c)
sequence_foldmap(d)
_traverse(e)(_.toInt)
_traverse(f)(_.toInt)
_traverse_rec(e)(_.toInt)
_traverse_rec(f)(_.toInt)
_traverse_recmap(e)(_.toInt)
_traverse_recmap(f)(_.toInt) 
sequence_via_traverse(c)
sequence_via_traverse(d)

traverse(e)(_e =>Try(_e.toInt))
traverse(f)(_f =>Try(_f.toInt))

traverse_rec(e)(_e =>Try(_e.toInt))
traverse_rec(f)(_f =>Try(_f.toInt))

