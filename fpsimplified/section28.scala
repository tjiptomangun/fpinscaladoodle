//Tail recursive algorithms
import scala.annotation.tailrec

val empty1: List[Int] = List()
val empty2: List[Int] = Nil

val	 list1 = List(1, 2, 3)
val	 list2 = 1 :: 2 :: 3 :: Nil
val  l = 1 :: 2 :: 3 :: 4 :: Nil

def sum(x:List[Int]) : Int = {
	x match {
		case Nil =>
			0
		case a :: b => {
			a + sum(b)
		}
	}
}

sum(l)


def sum2(x:List[Int]) : Int = {
	x match {
		case List() =>
			0
		case a :: b => {
			a + sum2(b)
		}
	}
}

sum2(l)

def sum3(a:List[Int]) : Int = {

	@tailrec
	def innerSum(x: List[Int], acc: Int) : Int = 
		x match {
			case List() =>
				acc + 0
			case a :: b => {
				innerSum(b, a + acc)
			}
		}

	innerSum(a, 0);
}

sum3(l)

def take[A](x:List[A]) (n:Int): List[A]  = {
	x match {
		case a :: b if (n > 0) =>{
			a :: take(b) (n -1)	
		}
		case _ =>
			Nil
	}
}

take(l)(2);
take(l)(1);
take(l)(0);
take(l)(5);
@tailrec
def take_rec[A](x:List[A]) (n:Int) (out:List[A]): List [A] = {
	x match {
		case a :: b if (n > 0) => {
			take_rec(b)(n - 1) (a :: out)
		}
		case _ => {
			out.reverse
		}
	}
}


take_rec(l)(2)(List.empty: List[Int]);

def product(in: List[Int]) : Int  = {
	in match {
		case a::b => {
			a * product(b);
		}
		case _ => {
			1
		}
	}
}

product(l)
