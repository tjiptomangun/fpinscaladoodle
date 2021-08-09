//scala -Xprint:parse section58.scalac.scala

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

trait Monad[M[_]] {

  def lift[A](a: => A): M[A]

  def flatMap[A, B](ma: M[A])(f: A => M[B]):M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => lift[B](f(a)))

}


case class StateT[M[_], S, A] (run: S => M[(S, A)]){
  def flatMap[B](g: A => StateT[M, S, B])(implicit M: Monad[M]): StateT[M, S, B] = StateT {
    (s0: S) => M.flatMap(run(s0)) {
      case (s1, a) => g(a).run(s1)
    }
  }

  def map[B](f: A => B)(implicit M:Monad[M]): StateT[M, S, B] = flatMap(a => StateT.point(f(a)))

}

object StateT {
  def point[M[_], S, A](v: A)(implicit M: Monad[M]): StateT[M, S, A] = StateT(run  = s => M.lift((s, v)))
}

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
  def sumLoop: StateT[IO, SumState, Unit] = for {
    _     <- putStrAsStateT("\ngive me an int: ")
    input <- getLineAsStateT
    i     <- liftIOIntoStateT(IO(toInt(input)))
    _     <- doSumWithStateT(i)
    _     <- sumLoop
  } yield ()

}
