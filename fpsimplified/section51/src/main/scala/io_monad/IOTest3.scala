package io_monad

object IOTest3 extends App{
  def loop: IO[Unit] = for {
    _     <- putStrLn("Type something")
    input <- getLine
    _     <- putStrLn(s"You said '$input'.")
    _     <- if ( input == "quit") IO(()) else loop
  } yield()
  loop.run
}
