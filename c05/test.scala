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

val t01 = mayBeTwice(true, {println("hi"); 1 + 29})
val t02 = mayBeTwice(false, {println("hi"); 1 + 30})

def xmayBeTwice(b: Boolean, i: (Int) => Int):Int = {
	if (b)
		i(1) + i(1)
	else
		i(3) - 2
}

val t03 = xmayBeTwice(true, (x: Int) => {println("hi"); 1 + 31})
val t04 = xmayBeTwice(false,(x: Int) => {println("hi"); 1 + 32})

def yNotTwice(b: Boolean, i: => Int) = {
	lazy val j = i
	if (b) j + j  else 0
}

val t05 = yNotTwice(true, {println("hi"); 1 + 33})
val t06 = yNotTwice(false, {println("hi"); 1 + 34})

def zNotTwice(b: Boolean, i: () => Int) = {
	val j = i()
	if (b) j + j  else 0
}

val t07 = zNotTwice(true, () => {println("hi"); 1 + 35})
val t08 = zNotTwice(false, () => {println("hi"); 1 + 36})

def cccc(f: => Int) = {f}
def dddd(f:()  => Int) = {1}
def eeee(f:()  => Int):Int = {f()}
cccc(4)
dddd(()=>2)
cccc(Math.pow(Math.pow(3, 2.5), 2.545).toInt)
dddd(() => Math.pow(Math.pow(3, 2.5), 2.545).toInt)
eeee(() => Math.pow(Math.pow(3, 2.5), 2.545).toInt)

/* 
 * Notes
 * x: () => A and x: => A are different on calling style and signature but 
 * do exactly the same thing.
 * It is just mean that in first form we need to have () in applying
 * and no need to have () in second form of applying
 */


