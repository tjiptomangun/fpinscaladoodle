//package typeclasses.v1_humanlike
package typeclasses
package v1_humanlike
sealed trait Animal
final case class Dog(name: String) extends Animal
final case class Cat(name: String) extends Animal
final case class Bird(name: String) extends Animal
import BehavesLikeHumanInstances.dogBehavesLikeHuman

object Test extends App {
	val a = Dog("bow wow")

	BehavesLikeHuman.speak(a)
	BehavesLikeHuman.eatHumanFood(a)

	import BehavesLikeHumanSyntax.BehavesLikeHumanOps
	a.speak
	a.eatHumanFood
}
