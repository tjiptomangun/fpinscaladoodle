//Starting to glue functions together

def f(a: Int): (Int, String) = {
	val res = a * 2
	(res, s"\nf result: $res");
}


def g(a: Int): (Int, String) = {
	val res = a * 3
	(res, s"\ng result: $res");
}

def h(a: Int): (Int, String) = {
	val res = a * 4
	(res, s"\nh result: $res");
}


def bind(fun: (Int) => (Int, String), tup: Tuple2[Int, String]): (Int, String) = {
	val (intRes, stringRes) = fun(tup._1);
	(intRes, tup._2 + stringRes)
}

val fResult = f(100);
val gResult = bind(g, fResult);
val hResult = bind(h, gResult);

println(s"result: ${hResult._1}, debug: ${hResult._2}")
