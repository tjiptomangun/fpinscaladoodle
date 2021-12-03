package io_examples.v1

import java.io.File

import scala.util.Try
import io_examples.common.Control.using

/**
  * returns instance of Try
  */
object FileUtils {
  def readTextFileAsString(filename: String): Try [String] = {
    Try {
      val lines = using(io.Source.fromFile(filename)) {
        source =>
          (for ( line <- source.getLines) yield  line).toList
      }
      lines.mkString("\n")
    }
  }

  /**
    *
    * Test scaladoc
    */

  def copyFile(srcFile: File, destFile:File): Try[Boolean] = ???
  def readFileToByteArray(file: File):Try[Array[Byte]]  = ???
}
