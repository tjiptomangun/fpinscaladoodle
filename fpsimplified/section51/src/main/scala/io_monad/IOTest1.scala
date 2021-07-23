package io_monad

object IOTest1 extends  App{

  for {
    _         <- putStrLn("First name ?")
    firstName <- getLine
    _         <- putStrLn("Last name ?")
    lastName  <- getLine
    _         <- putStrLn(s"Firs: $firstName, last; $lastName")
  } yield  ()

}
