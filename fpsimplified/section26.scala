//Partially Applied Function and currying
def plus(a: Int)(b: Int) = a + b
def plus2 = plus(2) _

plus2(3)
//5

def wrap(prefix: String)(html: String)(suffix: String) = {
	prefix + html + suffix
}
val hello = "Hello, World";
val result = wrap("<div>")(hello)("</div>")

val wrapWithDiv = wrap("<div>")(_:String)("</div>")
wrapWithDiv("<p>Hello World</p>")
//String = <div><p>Hello World</p></div>
wrapWithDiv("<img src=\"images/foo.png\"/>")
//String = <div><img src="images/foo.png"/></div>

//currying common function
def add(x: Int, y: Int) = x + y
val addFunction = add _
(add _).isInstanceOf[Function2	[_, _, _]]

val addCurried = (add _).curried
addCurried(7)(15)

//PAF with single parameter group

def wrap2(prefix: String, html: String, suffix: String) = { 
	prefix + html + suffix
}

val wrap2WithDiv = wrap2("<div>", _: String, "</div>")
wrap2WithDiv("Hello World")

def add (a:Int)(b:Int) = a + b;
List(10, 11, 12).map(add (2) _)
