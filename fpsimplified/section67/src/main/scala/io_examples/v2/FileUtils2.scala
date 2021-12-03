package io_examples.v2

import java.io.File

import scala.util.Try
import io_examples.common.Control.using

object FileUtils2 {
  def readTextFileAsString(filename: String): IO[String] = {
    Try {
      val lines = using(io.Source.fromFile(filename)) {
        source =>
          (for (line <-source.getLines) yield line).toList
      }
      lines.mkString("\n")
    }
  }
  def copyFile(srcFile: File, destFile:File): IO[Boolean] = ???
  def readFileToByteArray(file: File):IO[Array[Byte]]  = ???

}
