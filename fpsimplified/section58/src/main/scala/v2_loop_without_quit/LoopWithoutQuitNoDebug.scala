package v2_loop_without_quit

import monads.{IO, Monad, StateT}

object LoopWithoutQuitNoDebug extends  App {
  def getLine(): IO[String] = IO(scala.io.StdIn.readLine())
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
/*
  in for comprehension the last part (map part) is evaluated first and next flatMap above it
  and continue until the flatMap of the first generator.
    the left part of flatMap generator like `u` in below example is something that is used
    as first generator flatMap variable.
  for {
    u <- Some(c)
    v <- Some(d)
  } yield u + v

   so u above translation is  like Some(c).flatMap((u) -> Some(d).map((v)=> { u + v}))
  */
  def sumLoop: StateT[IO, SumState, Unit] = for {
    _     <- putStrAsStateT("\ngive me an int: ")
    input <- getLineAsStateT //see that StateT flatMap takes a function with input A and return StateT
    i     <- liftIOIntoStateT(IO(toInt(input))) //that is why we can toInt (input), because input type is A
    _     <- doSumWithStateT(i)
    _     <- sumLoop
  } yield ()
  getLineAsStateT


//  val result: (SumState, Unit) = sumLoop.run(SumState(0)).run
//
//  println(s"Final SumState: ${result._1}")
}
