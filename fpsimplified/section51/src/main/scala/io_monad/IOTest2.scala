package io_monad

object IOTest2 extends App{
  def forExpression: IO[Unit] = for {
    _          <- putStrLn("Erste Namen?")
    firstName  <- getLine
    _          <- putStrLn("Letzte Namen ?")
    lastName   <- getLine
    fName = firstName.toUpperCase
    lName = lastName.toUpperCase
    _          <- putStrLn((s"Erste: $fName, letzte: $lName"))
  } yield()
  forExpression.run

}
