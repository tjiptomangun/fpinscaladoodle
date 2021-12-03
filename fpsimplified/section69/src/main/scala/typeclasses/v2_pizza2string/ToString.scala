package typeclasses.v2_pizza2string

//(1) the Type Class
trait ToString[A] {
	def toString(a: A): String
}

//(2) type class instance we want
object ToStringInstances {
	implicit val pizzaAsString = new ToString[Pizza] {
		def toString(p: Pizza): String = {
			s"""|Pizza(${p.crustSize}, ${p.crustType}),
                |toppings=${p.toppings}""".stripMargin
		}
	}
}

//(3.a)
object ToString {
	def asString(a: A)(implicit toStringInstance: ToString[A]): String = {
		toStringInstance.toString(a)
	}
}

object ToStringSyntax {
	implicit class ToStringOps[A](value: A) {
		def asString(implicit toStringInstance: ToString[A]): String = {
			toStringInstance.toString(value)
		}
	}
}
