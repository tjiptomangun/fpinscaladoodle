//how to write a `map`  function

//https://alvinalexander.com/scala/fpbook/explaining-scala-val-function-syntax-functional-programming/
val sum = (a: Int, b:Int) => a + b					//implicit return type (IRT)
val sum2: (Int, Int) => Int = (a, b) => a + b       //explicit return type (ERT)
def sum3(a: Int, b: Int):Int = a + b                //method

def map[A, B](f: A => B, list: Seq[A]): Seq[B] = {
	for {
		x <- list
	} yield f(x)
}

def filter[A](f: A => Boolean, list:Seq[A]): Seq[A] = {
	for {
		 x <- list
		if f(x)
	} yield x
}
