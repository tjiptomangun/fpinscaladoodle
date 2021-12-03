package utils

import org.scalacheck.Prop.forAll

import org.scalacheck.{Arbitrary, Gen, Properties}

object IncreaseRandomlySpec extends Properties("IncreaseRandomlySpec")
{
  property("increaseRandomly") = forAll {
    input: Int =>
      val result = MathUtils.increaseRandomonly(input)
      result > input
  }
  val littleInts = Gen.choose(0, 99)

}
