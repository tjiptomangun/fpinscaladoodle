package utils
import org.scalacheck.{Gen, Properties}
import org.scalacheck.Gen.const
import org.scalacheck.Prop.forAll
sealed trait Topping
case object BlackOlives extends Topping
case object Cheese extends Topping
case object Mushrooms extends Topping
case object Pepperoni extends Topping
case object Sausage extends Topping

object GetToppingSeq {
  val genBO = const (BlackOlives)
  val genCh = const (Cheese)
  val genMu = const (Mushrooms)
  val genPe = const (Pepperoni)
  val genSa = const (Sausage)

  def genTopping : Gen[Topping] = Gen.oneOf(genBO, genCh, genMu, genPe, genSa)
  val genToppings : Gen[List[Topping]] = Gen.containerOf[List, Topping](genTopping)

}
object DropAllButFirstSpec_PizzaToppings extends Properties("DropAllButFirstSpec_PizzaToppings"){
  /**
    * TODO : please refactor this and DropAppButFirstSpec_IntLists, as the use the same exact algorithm
    * to test two different type of lists
    */
  property("dropAllButFirst_Toppings") = forAll(GetToppingSeq.genToppings) { input: List[Topping] =>
    val TOPPING_TO_MATCH = Pepperoni

    val result = ListUtils.dropAllButFirst(input, TOPPING_TO_MATCH)

    val numMatches = input.count(_ == TOPPING_TO_MATCH)

    if (numMatches == 0) {
      input == result
    }
    else if (numMatches == 1) {
      input == result
    }
    else {
      val element1PositionOriginal = input.indexOf(TOPPING_TO_MATCH)
      val element1PositionFinal = result.indexOf(TOPPING_TO_MATCH)

      val numOccurencesInResult = result.count(_== TOPPING_TO_MATCH)

      val locationOfFirstOccurrenceInInput = input.indexOf(TOPPING_TO_MATCH)
      val (inputBefore, inputAfter) = input.splitAt(locationOfFirstOccurrenceInInput)

      val inputAfterTail = inputAfter.tail

      val inputAfterFiltered = inputAfterTail.filter(_ != TOPPING_TO_MATCH)

      val locationOfFirstOccurrenceInResult = result.indexOf(TOPPING_TO_MATCH)
      val (resultBefore, resultAfter) = result.splitAt(locationOfFirstOccurrenceInResult)
      val resultAfterTail = resultAfter.tail

      (
        element1PositionOriginal == element1PositionFinal &&
        numOccurencesInResult == 1 &&
        inputBefore == resultBefore &&
        inputAfterFiltered == resultAfterTail
      )
    }

  }
}
