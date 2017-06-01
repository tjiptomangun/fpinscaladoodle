sealed trait Tree[A]
case class Leaf[A](value:A) extends  Tree[A]
case class Branch[A](left:Tree[A], right:Tree[A]) extends Tree[A]

object Tree{
	def size[A](t:Tree[A]):Int = {
		t match {
			case Branch(l, r) =>
				1 + size(l) + size(r)
			case Leaf(v) =>
				1
		}
	}

	def maximum [A](t:Tree[A])(max : (A, A)=>A):A = {

		def inMax[A](l:Tree[A], r:Tree[A])(max:(A, A)=>A):A = {
			(l, r) match {
				case (Leaf(l), Leaf(r)) =>
					max(l, r)
				case (Branch(ll, lr), Branch(rl, rr)) =>
					max(inMax(ll, lr) (max), inMax(rl, rr)(max))
				case (Leaf(a), Branch(b, c)) =>
					max(a, inMax(b, c)(max))
				case (Branch(a, b), Leaf(c)) =>
					max(inMax(a, b)(max), c)
					
			}
		}
		t match {
			case Leaf(v) =>
				v
			case Branch(l, r) =>
				inMax(l, r)(max)
		}
	}

	def depth[A](t: Tree[A]): Int ={
		def inMax(a: Int, b: Int): Int = {
			a >= b match {
				case true =>
					a
				case _ =>
					b
			}
		}

		t match {
			case Leaf(a) =>
				1
			case Branch (l, r) =>
				1 + inMax(depth(l), depth(r))
			
		}
	}

	def map[A, B](t:Tree[A])(f:(A)=>B):Tree[B] = {
		t match {
			case Leaf(t) =>
				Leaf(f(t))
			case Branch(l, r) =>
				Branch(map(l)(f), map(r)(f))
		}
	}

	def fold[A, B](t:Tree[A],z:(A)=>B)(f:(B, B)=>B):B = {
		t match {
			case Leaf(a) =>
				z(a)

			case Branch(a, b) =>
				f(fold(a, z)(f), fold(b, z)(f))
		}
	}

	def sizeViaFold[A](t: Tree[A]): Int = {
		fold(t, (a:A)=>1)((c, d) => 1 + c + d)
	}

	def maximumViaFold[A](t:Tree[A])(max: (A, A) => A): A = {
		fold(t, (a:A)=>a)((c, d) => max(c, d))	
	}

	def depthViaFold[A] (t:Tree[A]): Int = {
		fold(t, (a:A)=>1)((c, d) => c > d match{
			case true => 1 + c
			case _ => 1 + d
		})
	}

	def mapViaFold[A,B] (t:Tree[A])(f:(A)=>B):Tree[B] = {
		fold(t, (a:A)=>Leaf(f(a)):Tree[B]){(c:Tree[B], d:Tree[B]) =>
			Branch(c, d)
		}
	} 
}

val l1 = Leaf(1)
val l2 = Leaf(2)
val l3 = Leaf(3)
val l4 = Leaf(4)
val l5 = Leaf(5)
val l6 = Leaf(6)
val l7 = Leaf(7)
val l8 = Leaf(8)

val lb1 =Branch(l1, l6)
val rb1 =Branch(l2, l5)
val rb2 =Branch(l3, l4)
val lb2 =Branch(l5, l6)
val t1 = Branch(Branch(lb1, rb1), Branch(Branch(rb2, lb2), Leaf(9)))

val r00 = Tree.size(t1)
val r00vF = Tree.sizeViaFold(t1)
val r01 = Tree.maximum(t1){(a, b) =>
	a >= b match {
		case true =>
			a
		case _ =>
			b
	}
}
val r01vF = Tree.maximumViaFold(t1){(a, b) =>
	a >= b match {
		case true =>
			a
		case _ =>
			b
	}
}

val r02 = Tree.depth(t1)
val r02vF = Tree.depthViaFold(t1)

val r03 = Tree.map(t1){
	p => p + 0.1	
}


val r03vF = Tree.mapViaFold(t1){
	p => p + 0.1	
}
