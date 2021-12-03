package io_examples.v2

import io_examples.common.MiscUtils

import scala.util.{Failure, Success}

object Driver2 extends App{
  val passwd : IO[String] = FileUtils2.readTextFileAsString("/etc/passwd")

  passwd match {
    case  Success(s) => println(s)
    case Failure(e) => {
      val msg = MiscUtils.getFullStackTrace(e)
      println(msg)
    }
  }

}
