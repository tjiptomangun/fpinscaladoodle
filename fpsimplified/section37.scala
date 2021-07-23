//Embrace the idiom
trait Person {
	def name: String
	def age: Int
	override def toString = s"name: $name, age: $age"
}
//1) anonymous class, we do not extend Person as any class explicitly
val mary = new Person {
	val name = "mary"
	val age = 22
}

val jane = new Person {
	def name = "jane"
	def age = 25
}
println(mary);
println(jane);

def timer[A] (blockOfCode: => A) = {
	val startTime = System.nanoTime;
	val result = blockOfCode
	val stopTime = System.nanoTime;
	val delta = stopTime - startTime
	(result, delta/1000000d)
}
//2) function that take by name parameter
val (result, time) = timer{
	Thread.sleep(1000)
	42
}


def timex[A] (blockOfCode: => A) = {
	val startTime = System.nanoTime;
	val result = blockOfCode
	val stopTime = System.nanoTime;
	val delta = stopTime - startTime
	delta/1000000d
}

val  timeres = timex {
	Thread.sleep(1000)
	42
}

def cool (a: Int) : Int = {
	a + 1
}
cool {10}

def coot (a: Int, b: Int) : Int = {
	a + b  +  1
}
//coot (10){11} not working
def coos(a: Int)(b:Int) : Int = {
	a + b + 1
}
coos{10+1}{11+2}
coos(14){11*2}

//3 a class that accept a function parameter
case class StringToInt(run: String => Int);

val sti = StringToInt{
	s: String => {
		s.length
	}
}
sti.run("qqqqqqq")


case class XXX(x: Int)
val d = XXX{10}
d

case class ABCD(x: Int, y: String)
ABCD (10, "Test")
//ABCD {10, "test"} not working


case class StringAndIntToInt(run: (String, Int) => Int);
val saiti = StringAndIntToInt{
	(s, t) => {
		s.length + t
	}
}
saiti.run("hello", 3);

case class Transform2ParamsTo1Param[A, B](fun:(A, A) => B);

val ssToI = Transform2ParamsTo1Param{
	(x:String, y:String) => {
		x.length + y.length
	}
}
ssToI.fun("What", "'s up Doc")

def justRun[A, B](toRun: (A, A) => B)(x: A, y: A): B= { 
	(Transform2ParamsTo1Param(toRun)).fun(x, y)
}
justRun{
	(a: String, b: String) => {
		(a.length - b.length)
	}
}("What", "'s up Doc")


//4 A function that takes a function input parameter
//A Implemented as a function
def s2i (s: String)(f: String => Int) = f(s)
val res = s2i("hello") {s: String => s.length}
res 
//B Implemented as a case class
case class cs2i (s:String)(_fun: String => Int){
	def fun = _fun(s)
}

val res2 = cs2i("hello"){s: String => 
	s.length
}

println(res2.fun);


