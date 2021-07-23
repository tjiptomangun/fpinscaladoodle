//A Quick review how flatMap works
List("foo", "baar").map(x => x.split(""))
//List[Array[String]] = List(Array(f, o, o), Array(b, a, a, r)) 
List("foo", "baar").map(x => x.split("")).flatten
//List[String] = List(f, o, o, b, a, a, 

List("foo", "baar").flatMap(x => x.split(""))
//List[String] = List(f, o, o, b, a, a, r)

List(List(1, 2, 3), List(4, 5, 6)).flatten;

List(1, 2, 3).flatMap((x:Int) => {
	val a:List[Int] = List()
	def _intFn(init:List[Int], num:Int): List[Int] =  {
		if (num >= 0) {
			_intFn(num +: init, num - 1)
		}
		else {
			init
		}
	}
	_intFn(a, x);
})

/*
for (a <- 1 until 10){
	println(a)
}
*/
