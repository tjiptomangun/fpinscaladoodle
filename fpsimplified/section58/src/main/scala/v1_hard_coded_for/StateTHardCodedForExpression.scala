package v1_hard_coded_for

import monads.{IO, Monad, StateT}

object StateTHardCodedForExpression extends App{
  implicit val IOMonad = new Monad[IO] {
    def lift[A](a: => A): IO[A] = {
      IO(a)
    }

    override def flatMap[A, B](ma: IO[A])(f: A => IO[B]): IO[B] = ma.flatMap(f)
  }

  case class IntState(i: Int)

  def add (i: Int)= StateT[IO, IntState, Int] {oldState =>
    val newValue = i + oldState.i
    val newState = oldState.copy(i = newValue)
    IO(newState, newValue)

  }

  def multiply(i: Int) = StateT[IO, IntState, Int] { oldState =>
    val newValue = i * oldState.i
    val newState = oldState.copy(i = newValue)
    IO(newState, newValue)
  }


  val a = add (1);
  val b = a.run(IntState(1))

  b.map(t => println(s"b state = ${t._1}"))

  val forExpressionX: StateT[IO, IntState, Int] = for {
    _ <- add(2)
    _ <- add(5)
    x <- multiply(15)
  } yield x

  val forExpression = for {
    _ <- add(2)
    _ <- add(5)
    x <- multiply(47)
  } yield x

  val result: IO[(IntState, Int)] = forExpression.run(IntState(0))

  result.map(tuple => println(s"IntState = ${tuple._1}"))
}
