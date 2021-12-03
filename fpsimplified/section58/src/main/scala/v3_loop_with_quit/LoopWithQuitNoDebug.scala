package v3_loop_with_quit

import monads.{IO, Monad, StateT}


object LoopWithQuitNoDebug extends  App {

  def getLine() : IO[String] = IO(scala.io.StdIn.readLine())
  def putStr(s: String): IO[Unit] = IO(print(s))

  def toInt(s: String): Int = {
    try {
      s.toInt
    }
    catch {
      case e: NumberFormatException => 0
    }
  }

  case class SumState(sum: Int)

  // an implementation of the `Monad` trait for the `IO` type
  implicit val IOMonad = new Monad[IO] {
    def lift[A](a: => A): IO[A] = {
      IO(a)
    }

    override def flatMap[A, B](ma: IO[A])(f: A => IO[B]): IO[B] = ma.flatMap(f)
  }

  def doSumWithStateT(newValue: Int) : StateT[IO, SumState, Int]  = StateT{
    (oldState: SumState) => {

      val newSum  = newValue + oldState.sum

      val newState: SumState = oldState.copy(sum = newSum)

      IO(newState, newSum)
    }
  }

  /**
    * the purpose of this function is to “lift” an IO action into the StateT monad.
    * given an IO instance named `io` as input, the anonymous function transforms
    * the `IO[A]` into an `IO[(SumState, A)]`; that result is then wrapped in a `StateT`.
    */
  def liftIOIntoStateT[A](io: IO[A]): StateT[IO, SumState, A]  = StateT {
    s: SumState => {
      io.map(a => (s, a))
    }
  }

  // new versions of the i/o functions that uses StateT
  def getLineAsStateT(): StateT[IO, SumState, String]  = liftIOIntoStateT(getLine)
  def putStrAsStateT(s: String) : StateT[IO, SumState, Unit] = liftIOIntoStateT(putStr(s))

  def sumLoop: StateT[IO, SumState, Unit] = for {
    _     <- putStrAsStateT("\ngive me an int: ")
    input <- getLineAsStateT //see that StateT flatMap takes a function with input A and return StateT
    _     <- if (input == "q") {
              liftIOIntoStateT(IO(()))
             }
            else for {
              i     <- liftIOIntoStateT(IO(toInt(input))) //that is why we can toInt (input), because input type is A
              _     <- doSumWithStateT(i)
              _     <- sumLoop
            } yield ()
  } yield ()

  val result = sumLoop.run(SumState(0)).run
  println(s"Final SumState: ${result}")

}
