package utils

object MathUtils {
  def increaseRandomonly(i: Int) = {
    val randomNum = getRandomIntFrom1To100()
    i + randomNum.toLong
  }
  private def getRandomIntFrom1To100(): Int = scala.util.Random.nextInt(100) + 1
}
