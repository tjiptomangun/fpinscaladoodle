package utils

import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.forAll
object GetIntSeq {
  val g1To5: Gen[List[Int]] = Gen.containerOf[List, Int](Gen.choose(1, 5))

  val favorTwos: Gen[Int] = Gen.frequency(
    (1, 1),
    (4, 2),
    (1, 3),
    (1, 4),
    (1, 5),
  )

  val genMostlyTwos : Gen[List[Int]] = Gen.containerOf[List, Int](favorTwos)

  val littleList: List[Int] = scala.util.Random.shuffle(List(1, 2, 3, 4, 5, 6, 2, 7, 8, 9))
  val littleListGen: Gen[List[Int]] = Gen.someOf(littleList).map(_.toList)
}

object DropAllButFirstSpec_IntLists extends Properties("DropAllButFirstSpec"){
  val NUM_TO_DROP = 2

  property("dropAllButFirstIntLists") =
    forAll(GetIntSeq.g1To5) {
      input: List[Int] =>
        val result = ListUtils.dropAllButFirst(input, NUM_TO_DROP)

        val numMatches = input.count(_ == NUM_TO_DROP)
        if (numMatches == 0) {
          /**
            * OBSERVATIONS 1: If the element is not in the input list
            * the result should be equals
            */
          input == result
        }
        else if (numMatches == 1) {
          /**
            * OBSERVATIONS 2: If only one occurrence then result
            * should and input should be equals
            */
          input == result
        }
        else {
          /**
            * OBSERVATION 3: If more then one occurrence
            * (a) first element match should in its original position
            * (b) all other occurrences should be dropped
            * (c) all other elements in the list should be as they are
            */
          // (a) the first match element should remain in its original position
          val element1PositionOriginal = input.indexOf(NUM_TO_DROP)
          val element1PositionFinal = result.indexOf(NUM_TO_DROP)

          // (b) all other ocurrence should be dropped
          val numOccurrencesResult = result.count(_ == NUM_TO_DROP)

          // (c) all other elements in the list should be as they are
          val locationOfFirstOccurenceInput = input.indexOf((NUM_TO_DROP))
          val (inputBefore, inputAfter) = input.splitAt(locationOfFirstOccurenceInput)
          // splitAt retains the split elements as the first element of the after list
          val inputAfterTail = inputAfter.tail
          val inputAfterFiltered = inputAfterTail.filter(_ != NUM_TO_DROP)

          val locationOfFirstOccurenceResult= result.indexOf((NUM_TO_DROP))
          val (resultBefore, resultAfter) = result.splitAt(locationOfFirstOccurenceResult)
          val resultAfterTail = resultAfter.tail

          //run all of the "OBSERVATION 3" property tests
          (
            element1PositionOriginal == element1PositionFinal &&
            numOccurrencesResult == 1 &&
            inputBefore == resultBefore &&
            inputAfterFiltered == resultAfterTail

          )

        }
    }

}
