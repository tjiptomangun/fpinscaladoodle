def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A) : A = {
	if (cond) onTrue()
	else onFalse()
}
val a= 51

if2 (a < 22, 
	() => println("a"),
	() => println("b"));


def if3[A](cond: Boolean, onTrue: => A, onFalse: => A): A = {
	if (cond) onTrue
		else onFalse
	
}

val res20 = if3 (a < 22, () => println("a"), () => println("b"));

res20.apply()

def nif2[A](cond: Boolean, onTrue: Function1[Int, A], onFalse: Function1[Int, A]): A = {
	if (cond) onTrue(1)
	else onFalse(100)
}

def xnif2[A](cond: Boolean, x: Int, y: Int, onTrue: (Int) => A, onFalse: (Int) => A): A = {
	if (cond) onTrue(x)
	else onFalse(y)
}

xnif2(false, 3, 4, (x:Int) => x*x, (y:Int) => y)
xnif2(true , 3, 4, 
	(x:Int) => {
			println("x= " +x); 
			x*x
		}
			, (y:Int) => {
			println("y = "+y); 
			y
		})

def mayBeTwice(b: Boolean, i: => Int) = if (b) i + i else 0

val t01 = mayBeTwice(true, {println("hi"); 1 + 32})
val t02 = mayBeTwice(false, {println("hi"); 1 + 32})

def xmayBeTwice(b: Boolean, i: (Int) => Int):Int = {
	if (b)
		i(1) + i(1)
	else
		i(3) - 2
}

val t03 = xmayBeTwice(true, (x: Int) => {println("hi"); 1 + 32})
val t04 = xmayBeTwice(false,(x: Int) => {println("hi"); 1 + 32})

def ymayBeTwice(b: Boolean, i: => Int) = {
	lazy val j = i
	if (b) j + j  else 0
}

val t05 = ymayBeTwice(true, {println("hi"); 1 + 32})
val t06 = ymayBeTwice(false, {println("hi"); 1 + 32})

def zmayBeTwice(b: Boolean, i: => Int) = {
	val j = i
	if (b) j + j  else 0
}

val t07 = zmayBeTwice(true, {println("hi"); 1 + 32})
val t08 = zmayBeTwice(false, {println("hi"); 1 + 32})





