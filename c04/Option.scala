//{ echo ":paste" & cat Option.scala & echo ":q";}| scala

import scala.{Option => _, Either => _, _}
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
	val mean_ = mean(xs)
	xs.flatMap{ x=>
		mean_ match {
			case None =>
				None
			case Some(m) =>
				Some(math.pow(x - m), 2)
		}
	}
}

val lDouble = List(1.1, 0.0, 3.5, 2.2, 4.1, 7.1, 9,2)

val v = variance(lDouble)
