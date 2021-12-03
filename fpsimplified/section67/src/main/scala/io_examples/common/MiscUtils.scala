package io_examples.common

import java.io.{PrintWriter, StringWriter}

object  MiscUtils {
  /**
    * Get the *full* (complete) stack trace as a string
    */
  def getFullStackTrace(t: Throwable): String = {
    val sw = new StringWriter()
    t.printStackTrace(new PrintWriter(sw))
    sw.toString
  }

}
