//A Quick Review of for Expressions
case class Person(firstName: String, lastName: String)

val people = List(
	Person("barney", "ruble"),
	Person("fred", "flinstone"), 
)

val nameStartingWithB = for {
	p <- people
	fname = p.firstName
	if (fname startsWith "b")
} yield fname.toUpperCase

nameStartingWithB.foreach(println);
