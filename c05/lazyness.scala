//this test will show what lazyness value is useful for
//seems that lazy variables in class will only be evaluated when the variables are referenced
//while non lazy variables will evaluated once the class is instatiated
//both lazy and strict variables will evaluate once, even if these variables referenced 
//many times
//: => Int is non strict, or lazy parameter
//it is just a common function with zero parameter
//: => is just syntactic alias of : () =>
//() => is syntactic alis for the type Function0[A]
def atest(in1 : => Int) : Int = {
	println(1)			
	1
}

def btest(in1 : Int) : Int = {
	println(1)
	1
}

def ctest = {
	println(3)
	3
}
println("atest")
atest(ctest)
println("btest")
btest(ctest)

def afun = {
	println(1)
	1
}

def bfun = {
	println(2)
	2
}

def cfun = {
	println(3)
	3
}

def dfun = {
	println(4)
	4
}

def dtest(cond: => Boolean, a: => Int, b: => Int, c: => Int, d: => Int): Int = {
	if (cond) 
		if (a > 1)//if cond == true then a will print twice, because it is a function
			if (b > 1) 
				if (c > 1)
					if (d > 1) 
						d
					else
						d + 1
				else
					c + 1
			else
				b + 1
		else
			a + 1
	else
		100
}

def etest(cond:  Boolean, a: Int, b:  Int, c: Int, d:  Int): Int = {
	if (cond) 
		if (a > 1)//if cond == true a will print once, because it is a value
			if (b > 1) 
				if (c > 1)
					if (d > 1) 
						d
					else
						d + 1
				else
					c + 1
			else
				b + 1
		else
			a + 1
	else
		100
}
println("dtest true")
println(dtest(true, afun, bfun, cfun, dfun))

println("etest true")
println(etest(true, afun, bfun, cfun, dfun))


println("dtest false")
println(dtest(false, afun, bfun, cfun, dfun))

println("etest false")
println(etest(false, afun, bfun, cfun, dfun))

def ftest(cond: => Boolean, a: => Int, b: => Int, c: => Int, d: => Int): Int = {
	if (cond) {
		val aval = a//this way a will evaluates once, 
			    //no price except for more curly braces
		if (aval > 1){
			val bval = b
			if (bval > 1){ 
				val cval = c
				if (cval > 1){
					val dval = d
					if (dval > 1){
						dval
					}
					else
						dval + 1
				}
				else
					cval + 1
			}
			else
				bval + 1
		}
		else
			aval + 1
	} 
	else
		100
}


println("ftest true")
println(ftest(true, afun, bfun, cfun, dfun))
println("ftest false")
println(ftest(false, afun, bfun, cfun, dfun))


def gtest(cond: => Boolean, a: => Int, b: => Int, c: => Int, d: => Int): Int = {
	val aval = a //less curly braces but each must be evaluated before called
	val bval = b
	val cval = c
	val dval = d
	if (cond) 
		if (aval > 1)
			if (bval > 1)
				if (cval > 1)
					if (dval > 1)
						dval
					else
						dval + 1
				else
					cval + 1
			else
				bval + 1
		else
			aval + 1
	else
		100
}


println("gtest true")
println(gtest(true, afun, bfun, cfun, dfun))
println("gtest false")
println(gtest(false, afun, bfun, cfun, dfun))


def htest(cond: => Boolean, a: => Int, b: => Int, c: => Int, d: => Int): Int = {
	lazy val aval = a //less curly braces but each evaluated on used
	lazy val bval = b
	lazy val cval = c
	lazy val dval = d
	if (cond) 
		if (aval > 1)
			if (bval > 1)
				if (cval > 1)
					if (dval > 1)
						dval
					else
						dval + 1
				else
					cval + 1
			else
				bval + 1
		else
			aval
	else
		100
}


println("htest true")
println(htest(true, afun, bfun, cfun, dfun))
println("htest false")
println(htest(false, afun, bfun, cfun, dfun))

val a = {
	println("hello")
	println("hello")
	3
}

println(a)
println(a)
println(a)
println(a)

class X {val x = {Thread.sleep(3000); 15}}
class Y {lazy val y = {Thread.sleep(3000); 13}}

val X1 = new X //will delay 
val Y1 = new Y //will not delay 
X1.x //will not delay
Y1.y //will delay on first call
X1.x //still not delay
Y1.y //no delay
X1.x
Y1.y


