//Functions are variables,  Too
val double = (i: Int) => i*2
val triple = (i: Int) => i*3

val functions = Map(
	"2x" -> double,
	"3x" -> triple
)

val dub = functions("2x");
dub(5)

