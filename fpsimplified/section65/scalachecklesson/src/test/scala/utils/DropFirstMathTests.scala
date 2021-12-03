package utils

import org.scalatest.BeforeAndAfter
import org.scalatest.funsuite.AnyFunSuite

class DropFirstMathTests extends AnyFunSuite with BeforeAndAfter{

  val list1 = List(1)
  val list123 = List(1, 2, 3)
  val list123123 = List(1, 2, 3, 1, 2, 3)

  test("drop 1 from list1") {
    val xs = ListUtils.dropFirstMatch(list1, 1)
    assert(xs.size == 0)
    assert(xs == List())
  }

  test( "drop 1 from list 123") {
    val xs = ListUtils.dropFirstMatch(list123, 1)
    assert(xs.size == 2)
    assert(xs == List(2, 3))
  }

  test("drop 3 from list 123") {
    val xs = ListUtils.dropFirstMatch(list123, 3)
    assert(xs.size == 2)
    assert(xs == List(1, 2))
  }

  test("drop 1 from list123123") {
    val xs = ListUtils.dropFirstMatch(list123123, 1)
    assert(xs.size == 5)
    assert(xs == List(2, 3, 1, 2, 3))
  }


}
