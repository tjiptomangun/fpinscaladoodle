package io_examples.v1

class IO[A] private (constructorBlock: => A) {
  def run: A = constructorBlock
  def flatMap[B](customAlgorithm: A => IO[B]): IO[B] = {
    val result1: IO[B] = customAlgorithm(run)
    val result2: B = result1.run
    IO(result2)
  }

  def map[B](f: A => B): IO[B] = flatMap((a:A) => IO(f(a)))
}

object IO {
  def apply[A](constructorBlock: => A): IO[A] = new IO(constructorBlock)
}
