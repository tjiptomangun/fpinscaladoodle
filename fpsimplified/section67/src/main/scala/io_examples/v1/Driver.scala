package io_examples.v1

import scala.util.{Failure, Success, Try}

object Driver extends App {
  val passwdFile : Try[String] = FileUtils.readTextFileAsString("/etc/passwd")

  passwdFile match {
    case Success(s) => println(s)
    case Failure(e) => println(e)
  }
}
