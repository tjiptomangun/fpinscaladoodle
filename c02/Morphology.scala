import scala.annotation.tailrec
object MonoMorph {
	def findFirst(ss: Array[String], key: String): Int = {
		@tailrec
		def loop(n: Int): Int = {
			if (n >= ss.length)
				-1
			else if (ss (n) == key)
				n
			else
				loop(n + 1)
		}
		loop (0)
	}
}

object Polymorph{
	def findFirst[A](as: Array[A], p: A => Boolean): Int = {
		@tailrec
		def loop (n: Int): Int =  {
			if (n >= as.length)
				-1
			else if (p(as(n)))
				n
			else loop (n+1)
		}
		loop (0)
	}
	

	def isSorted[A] (as: Array[A], ordered: (A, A) => Boolean): Boolean = {
		@tailrec
		def loop (a: Boolean, i: Int): Boolean= {
			if (i >= (as.length - 1))
				a
			else {
				val ord = a && ordered (as(i), as(i+1)) 
				loop(ord, i+1)
			}
		} 
		loop (true, 0)
	} 


}
val InSortInt = Array (1, 2, 3, 4)
val UnSortInt = Array (1, 3, 3, 4)

val InSortStr = Array ("a", "ab", "ac")
val UnSortStr = Array ("a", "ac", "ab")
val a = Polymorph.isSorted (InSortInt, (a: Int, b:Int) => a < b)
val b = Polymorph.isSorted (UnSortInt, (a: Int, b:Int) => a < b)

val c = Polymorph.isSorted (InSortStr, (a: String, b:String) => a.compareTo(b)<0)
val d = Polymorph.isSorted (UnSortStr, (a: String, b:String) => a.compareTo(b)<0)

def partial1[A, B, C](a: A, f: (A, B)=>C): B => C = 
	(b: B) => f(a, b)

def curry[A, B, C](f: (A, B) => C): A => (B => C) =
	(a: A) => ((b: B) => f(a,b))

def uncurry[A, B, C](f: A => B => C) : (A, B) => C =
	(a: A, b: B) => f(a).apply(b)

def compose[A, B, C](f: B => C, g: A => B): A => C = 
	(a: A) => f(g(a))

def someType2 (a: Int, b: Double) : String = "A is "+ a + " B is "+b
def someType3 (a: Int) (b: Double) (c: Long) : String = "A is "+ a + " B is "+b + " C is "+c
val a = someType2 _
val b = curry(a)
val c = uncurry(b)
val d = partial1 (3, a)
val e = b(3)
d(1.5)
e(1.5)







