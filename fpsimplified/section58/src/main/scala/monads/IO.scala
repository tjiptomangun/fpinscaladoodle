package monads

class IO[A]  private (constructorCodeBlock: => A){
  def run = constructorCodeBlock

  def flatMap[B](customAlgorithm: A => IO[B]): IO[B] = {
    val res1: IO[B] = customAlgorithm(run)
    val res2: B     = res1.run
    IO(res2)
  }

  def map[B](f: A => B): IO[B] = flatMap( a => IO(f(a)))
}

object IO {
  def apply[A](a: => A): IO[A] = new IO(a)
}
