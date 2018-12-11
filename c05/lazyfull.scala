//when p1 will be evaluated?

def atest(p1: ()=> Int): Int = {
	lazy val x = p1
	val ret = atest(x)
	ret
}
//stack overflow with no print
atest ({println("p1 evaluated"); 10})

def btest(p1: => Int): Int = {
	val x = p1
	val ret = atest(x)
	ret
}
//causes stack overflow, with p1 evaluated print once at the beginning
//btest ({println("p1 evaluated"); 10})

atest (() => {println("p1 evaluated"); 10})

